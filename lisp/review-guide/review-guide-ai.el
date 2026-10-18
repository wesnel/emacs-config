;;; review-guide-ai.el --- Model requests for review-guide -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ask a model, through gptel, to arrange a diff's hunks into chunks to
;; read in order, or to review the diff.  Replies are checked against
;; the parsed diff before anything uses them: a model can name hunks
;; that do not exist, forget others, and cite lines outside the diff,
;; and GitHub rejects a review whose comments fall outside it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'review-guide-diff)

(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(declare-function gptel-request "gptel-request")

(defgroup review-guide nil
  "Read pull requests in model-guided chunks, and review them with a model."
  :group 'tools)

(defcustom review-guide-backend nil
  "The gptel backend for review-guide requests, or nil for `gptel-backend'."
  :type '(choice (const :tag "Use gptel-backend" nil) sexp))

(defcustom review-guide-model nil
  "The gptel model for review-guide requests, or nil for `gptel-model'.
The model must support structured output, since every request carries
a JSON schema."
  :type '(choice (const :tag "Use gptel-model" nil) symbol))

(defcustom review-guide-max-diff-chars 400000
  "Refuse to send a diff whose prompt text is longer than this.
Most models' context windows hold roughly four characters a token."
  :type 'natnum)

(defcustom review-guide-group-system-prompt
  "You help a human review a pull request by deciding the order to read it in.

Split the diff's hunks into chunks.  A chunk is a set of hunks that make sense to
read together because they serve one purpose: a new type and the code that
constructs it, a renamed function and its call sites, a behaviour change and its
tests.  Order the chunks so that reading them in order explains the change:
what everything else depends on comes first, the core change before peripheral
updates, mechanical or generated changes last.

For each chunk give a short title and a summary of two to four sentences that
says what the hunks change and why, as far as the diff and the description show,
and points at anything that deserves a careful reviewer's attention.  Do not
review the code; orient the reader.

Every hunk id must appear in exactly one chunk.  Also give a short overview of
the whole pull request."
  "System prompt for arranging hunks into chunks."
  :type 'string)

(defcustom review-guide-review-system-prompt
  "You are reviewing a pull request.  Report only problems a careful senior
reviewer would raise: bugs, incorrect edge cases, security problems, data loss,
races, misleading names or documentation, and missing tests for risky behaviour.
Do not comment on style a formatter or linter would catch, and do not praise.

Anchor each comment to the lines it is about, using the hunk id and the L/R line
numbers shown before each diff line: side LEFT with an L number for a removed
line, side RIGHT with an R number for an added or unchanged line.  For a single
line, start_line equals line.  For a range, start_line is its first line and both
ends lie in the same hunk on the same side.

Write each body as the comment a human would post on the pull request: direct,
specific, with a suggested fix where there is one.  An empty list of comments is
a fine answer."
  "System prompt for reviewing a diff."
  :type 'string)

(defconst review-guide-ai--group-schema
  '(:type "object"
    :properties
    (:overview (:type "string")
     :chunks (:type "array"
              :items (:type "object"
                      :properties (:title (:type "string")
                                   :summary (:type "string")
                                   :hunks (:type "array" :items (:type "string")))
                      :required ["title" "summary" "hunks"])))
    :required ["overview" "chunks"]))

(defconst review-guide-ai--review-schema
  '(:type "object"
    :properties
    (:summary (:type "string")
     :comments (:type "array"
                :items (:type "object"
                        :properties (:hunk (:type "string")
                                     :side (:type "string" :enum ["LEFT" "RIGHT"])
                                     :start_line (:type "integer")
                                     :line (:type "integer")
                                     :severity (:type "string"
                                                :enum ["issue" "suggestion" "question" "nit"])
                                     :body (:type "string"))
                        :required ["hunk" "side" "start_line" "line" "severity" "body"])))
    :required ["summary" "comments"]))

(cl-defstruct (review-guide-chunk (:constructor review-guide-chunk-create)
                                  (:copier nil))
  "Hunks to read together, with the model's account of them."
  title summary hunks)

(cl-defstruct (review-guide-finding (:constructor review-guide-finding-create)
                                    (:copier nil))
  "A comment the model proposes, anchored to a line or range of the diff."
  hunk path side start-line line severity body
  (commit nil :documentation "The `review-guide-hunk-commit' of its hunk.")
  (state 'proposed :documentation "One of `proposed', `forwarded' or `dismissed'.")
  (id nil :documentation "Its id in a review record, such as \"1.F2\", or nil.")
  (outcome nil :documentation "How a review record's triage settled it, as
text shown with it, or nil."))

(defun review-guide-ai-prompt (title description hunks)
  "Return the user prompt for a pull request with TITLE and DESCRIPTION.
HUNKS are its parsed `review-guide-hunk's."
  (let ((prompt
         (concat "# " (or title "Untitled pull request") "\n\n"
                 (if (string-empty-p (string-trim (or description "")))
                     "(No description.)"
                   (string-trim description))
                 "\n\n# Diff\n\n"
                 (mapconcat #'review-guide-hunk-format hunks "\n"))))
    (when (> (length prompt) review-guide-max-diff-chars)
      (user-error "The diff is %d characters, over `review-guide-max-diff-chars'"
                  (length prompt)))
    prompt))

(defun review-guide-ai--parse (response)
  "Parse the JSON object in RESPONSE into an alist.
Tolerate a reply wrapped in a Markdown code fence."
  (let ((text (string-trim response)))
    (when (string-match (rx bos "```" (* nonl) "\n" (group (* anything)) "```" eos) text)
      (setq text (match-string 1 text)))
    (json-parse-string text :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun review-guide-ai-chunks (reply hunks)
  "Return the chunks in the parsed group REPLY, checked against HUNKS.
Each hunk lands in exactly one chunk: ids the diff does not have are
dropped, a hunk named twice stays in its first chunk, chunks left
empty are dropped, and hunks the model left out are collected into a
final chunk in diff order."
  (let ((known (mapcar (lambda (h) (cons (review-guide-hunk-id h) h)) hunks))
        (seen (make-hash-table :test #'equal))
        chunks)
    (dolist (chunk (alist-get 'chunks reply))
      (let ((members
             (cl-loop for id in (alist-get 'hunks chunk)
                      for hunk = (alist-get id known nil nil #'equal)
                      when (and hunk (not (gethash id seen)))
                      collect (progn (puthash id t seen) hunk))))
        (when members
          (push (review-guide-chunk-create
                 :title (alist-get 'title chunk)
                 :summary (alist-get 'summary chunk)
                 :hunks members)
                chunks))))
    (when-let* ((rest (cl-remove-if (lambda (h) (gethash (review-guide-hunk-id h) seen))
                                    hunks)))
      (push (review-guide-chunk-create
             :title "Not placed by the model"
             :summary "The model's reply did not place these hunks in any chunk."
             :hunks rest)
            chunks))
    (nreverse chunks)))

(defun review-guide-ai-findings (reply hunks)
  "Return the findings in the parsed review REPLY, checked against HUNKS.
Return (FINDINGS . DROPPED), where DROPPED counts comments that name
an unknown hunk or lines that hunk does not have on the given side.
A start line after the end line is swapped with it."
  (let ((dropped 0) findings)
    (dolist (c (alist-get 'comments reply))
      (let-alist c
        (let* ((hunk (cl-find .hunk hunks :key #'review-guide-hunk-id :test #'equal))
               (line (and (integerp .line) .line))
               (other (if (integerp .start_line) .start_line line))
               (start (and line (min other line)))
               (end (and line (max other line))))
          (if (and hunk line (stringp .body)
                   (review-guide-hunk-covers-p hunk .side start)
                   (review-guide-hunk-covers-p hunk .side end))
              (push (review-guide-finding-create
                     :hunk .hunk :path (review-guide-hunk-path hunk) :side .side
                     :commit (review-guide-hunk-commit hunk)
                     :start-line start :line end
                     :severity (intern (or .severity "suggestion"))
                     :body (string-trim .body))
                    findings)
            (cl-incf dropped)))))
    (cons (nreverse findings) dropped)))

(defun review-guide-finding-thread (finding)
  "Return the draft review thread for FINDING, without a body.
The alist carries GitHub's names for a draft review thread's fields,
which is also the form `pr-review' keeps pending threads in, and a
commit field when the finding's hunk has one."
  (let ((side (review-guide-finding-side finding))
        (start (review-guide-finding-start-line finding))
        (line (review-guide-finding-line finding))
        (commit (review-guide-finding-commit finding)))
    (append (when (< start line)
              `((startLine . ,start) (startSide . ,side)))
            `((path . ,(review-guide-finding-path finding))
              (line . ,line)
              (side . ,side))
            (when commit
              `((commit . ,commit))))))

(defun review-guide-ai--request (prompt system schema buffer on-reply on-error)
  "Send PROMPT with SYSTEM and SCHEMA, calling ON-REPLY with the parsed reply.
On failure call ON-ERROR with a description instead.  Both run in
BUFFER, and neither is called once BUFFER has been killed."
  (require 'gptel)
  (let ((gptel-backend (or review-guide-backend gptel-backend))
        (gptel-model (or review-guide-model gptel-model))
        (gptel-use-tools nil)
        (gptel-use-context nil))
    (gptel-request prompt
      :buffer buffer
      :system system
      :schema schema
      :callback
      (lambda (response info)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (cond
             ((stringp response)
              (let ((reply (condition-case err
                               (review-guide-ai--parse response)
                             (json-error
                              (funcall on-error
                                       (format "unreadable reply: %s"
                                               (error-message-string err)))
                              nil))))
                (when reply (funcall on-reply reply))))
             ((null response)
              (funcall on-error (format "%s" (plist-get info :status))))
             ((eq response 'abort)
              (funcall on-error "aborted")))))))))

(defun review-guide-ai-group (title description hunks buffer callback errback)
  "Ask for HUNKS arranged into chunks to read in order.
TITLE and DESCRIPTION are the pull request's.  Call CALLBACK in BUFFER
with the overview and the checked chunks, or ERRBACK with a
description of the failure."
  (review-guide-ai--request
   (review-guide-ai-prompt title description hunks)
   review-guide-group-system-prompt review-guide-ai--group-schema buffer
   (lambda (reply)
     (funcall callback (alist-get 'overview reply)
              (review-guide-ai-chunks reply hunks)))
   errback))

(defun review-guide-ai-review (title description hunks buffer callback errback)
  "Ask for a review of HUNKS.
TITLE and DESCRIPTION are the pull request's.  Call CALLBACK in BUFFER
with the summary, the checked findings, and the number of comments
dropped for citing lines outside the diff, or ERRBACK with a
description of the failure."
  (review-guide-ai--request
   (review-guide-ai-prompt title description hunks)
   review-guide-review-system-prompt review-guide-ai--review-schema buffer
   (lambda (reply)
     (pcase-let ((`(,findings . ,dropped) (review-guide-ai-findings reply hunks)))
       (funcall callback (alist-get 'summary reply) findings dropped)))
   errback))

(provide 'review-guide-ai)
;;; review-guide-ai.el ends here
