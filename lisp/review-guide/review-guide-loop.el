;;; review-guide-loop.el --- Fixups an automated review loop committed -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The fixups an automated review loop committed on a branch, as a
;; review-guide source.  The loop answers each review thread with one
;; `git commit --fixup' carrying the thread as trailers:
;;
;;   Review-Thread: <thread URL>
;;   Review-Bot: <bot>
;;   Review-Round: <round>
;;
;; The guide shows one chunk per such commit, in round order, holding
;; that commit's own diff, so it opens without asking the model.  Each
;; hunk's lines are numbered within its commit; see
;; `review-guide-hunk-commit'.  Comments, visiting and feedback work as
;; for `review-guide-local'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'review-guide-ai)
(require 'review-guide-diff)
(require 'review-guide-local)
(require 'review-guide-source)

(declare-function review-guide-open "review-guide")

(cl-defstruct (review-guide-loop (:include review-guide-local)
                                 (:constructor review-guide-loop-create)
                                 (:copier nil))
  "The review loop's fixups on the branch HEAD since BASE.")

(cl-defstruct (review-guide-loop-fixup (:constructor review-guide-loop-fixup-create)
                                       (:copier nil))
  "A commit the review loop made to answer one review thread."
  commit subject body thread bot round)

;;;; Git

(defconst review-guide-loop--trailers '("Review-Thread" "Review-Bot" "Review-Round")
  "The trailers a review loop's fixup carries.
`review-guide-loop--fixups' reads them into fields in this order.")

(defun review-guide-loop--strip-trailers (body)
  "Return the commit message BODY without its final paragraph of trailers."
  (let ((paragraphs (split-string (string-trim body) "\n\n")))
    (string-join
     (if (and paragraphs
              (cl-every (lambda (line)
                          (string-match-p (rx bos (+ (any alnum "-")) ":" (+ " ")) line))
                        (split-string (car (last paragraphs)) "\n" t)))
         (butlast paragraphs)
       paragraphs)
     "\n\n")))

(defun review-guide-loop--first-value (values)
  "Return the first of a trailer's VALUES, which git puts on separate lines."
  (and values (car (split-string values "\n" t))))

(defun review-guide-loop--fixups (source)
  "Return the review loop's fixups on SOURCE's branch, in round order.
They are the commits since SOURCE's base that carry a Review-Thread
trailer.  Within a round, and for commits without a Review-Round
trailer, which come last, they stay in the branch's order."
  (let* ((log (review-guide-local--git
               (review-guide-local-directory source) '(0)
               "log" "--reverse" "--no-merges"
               (concat "--format=%H%x1f%s%x1f%b"
                       (mapconcat (lambda (key)
                                    (format "%%x1f%%(trailers:key=%s,valueonly,unfold)" key))
                                   review-guide-loop--trailers "")
                       "%x1e")
               (format "%s..%s" (review-guide-local-base source)
                       (review-guide-local-head source))
               "--"))
         (fixups
          (cl-loop
           for record in (split-string log "\x1e" t "[\n]+")
           for (commit subject body thread bot round)
           = (mapcar (lambda (field)
                       (unless (string-empty-p (string-trim field))
                         (string-trim field)))
                     (split-string record "\x1f"))
           when thread
           collect (review-guide-loop-fixup-create
                    :commit commit :subject subject
                    :body (review-guide-loop--strip-trailers (or body ""))
                    :thread (review-guide-loop--first-value thread)
                    :bot (review-guide-loop--first-value bot)
                    :round (review-guide-loop--first-value round)))))
    (sort fixups
          (lambda (a b)
            (< (review-guide-loop--round-number a) (review-guide-loop--round-number b))))))

(defun review-guide-loop--round-number (fixup)
  "Return FIXUP's round as a number to sort by."
  (let ((round (review-guide-loop-fixup-round fixup)))
    (if (and round (string-match-p (rx bos (+ digit) eos) round))
        (string-to-number round)
      most-positive-fixnum)))

(defun review-guide-loop--target (fixup)
  "Return the subject of the commit FIXUP is a fixup of."
  (replace-regexp-in-string (rx bos (+ (or "fixup! " "amend! " "squash! "))) ""
                            (review-guide-loop-fixup-subject fixup)))

(defun review-guide-loop--title (fixup)
  "Return FIXUP's round, bot and target, as a chunk title."
  (let ((who (string-join
              (delq nil (list (when-let* ((round (review-guide-loop-fixup-round fixup)))
                                (format "Round %s" round))
                              (review-guide-loop-fixup-bot fixup)))
              ", ")))
    (concat (unless (string-empty-p who) (concat who ": "))
            (review-guide-loop--target fixup))))

(defun review-guide-loop--summary (fixup)
  "Return FIXUP's message body and review thread, as a chunk summary."
  (string-join (delq nil (list (let ((body (review-guide-loop-fixup-body fixup)))
                                 (unless (string-empty-p body) body))
                               (review-guide-loop-fixup-thread fixup)))
               "\n\n"))

;;;; The source

(cl-defmethod review-guide-source-diff ((source review-guide-loop))
  "Return each fixup's own diff, headed by a commit line naming it."
  (let ((directory (review-guide-local-directory source)))
    (mapconcat
     (lambda (fixup)
       (let ((commit (review-guide-loop-fixup-commit fixup)))
         (concat (format "commit %s\n" commit)
                 (apply #'review-guide-local--git directory '(0)
                        (append review-guide-local--diff-args
                                (list (concat commit "^") commit "--"))))))
     (review-guide-loop--fixups source)
     "")))

(cl-defmethod review-guide-source-describe ((source review-guide-loop))
  (let ((branch (review-guide-local-head source)))
    (list (format "%s review loop on %s since %s"
                  (file-name-nondirectory
                   (directory-file-name (review-guide-local-directory source)))
                  branch (review-guide-local-base-label source))
          (format "Review-loop fixups on %s" branch)
          (concat
           "Each commit below is a fixup an automated review loop made to answer one"
           " review thread.  A hunk's line numbers are those of its own commit.\n\n"
           (mapconcat (lambda (fixup)
                        (format "* %s %s\n\n%s\n\n"
                                (substring (review-guide-loop-fixup-commit fixup) 0 12)
                                (review-guide-loop--title fixup)
                                (review-guide-loop--summary fixup)))
                      (review-guide-loop--fixups source) "")))))

(cl-defmethod review-guide-source-chunks ((source review-guide-loop) hunks)
  "Return one chunk per fixup, holding the HUNKS of its commit."
  (mapcar (lambda (fixup)
            (review-guide-chunk-create
             :title (review-guide-loop--title fixup)
             :summary (review-guide-loop--summary fixup)
             :hunks (cl-remove-if-not
                     (lambda (hunk)
                       (equal (review-guide-hunk-commit hunk)
                              (review-guide-loop-fixup-commit fixup)))
                     hunks)))
          (review-guide-loop--fixups source)))

;;;; Entry point

(defun review-guide-loop--default-base (directory branch)
  "Return where to look for the review loop's fixups on BRANCH, or nil.
That is the nearest other local branch that BRANCH contains and the
default branch does not, which in a stack of pull requests is the one
below BRANCH, or else the default branch."
  (let* ((default (cl-find-if (lambda (rev) (review-guide-local--rev directory rev))
                              (remove "@{upstream}" review-guide-local--default-bases)))
         (below (split-string
                 (apply #'review-guide-local--git directory '(0)
                        "for-each-ref" "--format=%(refname:short)"
                        (concat "--merged=" branch)
                        (append (when default (list (concat "--no-merged=" default)))
                                '("refs/heads/")))
                 "\n" t))
         (nearest
          (cl-loop for ref in below
                   for distance = (string-to-number
                                   (review-guide-local--git
                                    directory '(0) "rev-list" "--count"
                                    (format "%s..%s" ref branch) "--"))
                   when (> distance 0)
                   collect (cons distance ref) into candidates
                   finally return (cdar (sort candidates #'car-less-than-car)))))
    (or nearest default)))

(defun review-guide-loop--read-args ()
  "Read the arguments for `review-guide-loop'.
Without a prefix argument, review the current branch from its default
base; with one, ask for the branch and the base."
  (let* ((directory (string-trim (review-guide-local--git default-directory '(0)
                                                          "rev-parse" "--show-toplevel")))
         (current (let ((out (string-trim (review-guide-local--git
                                           directory '(0 1) "symbolic-ref"
                                           "--quiet" "--short" "HEAD"))))
                    (unless (string-empty-p out) out))))
    (if (not current-prefix-arg)
        (list directory
              (or current
                  (user-error "HEAD is not on a branch; choose one with a prefix argument")))
      (let* ((branch (completing-read
                      (format-prompt "Review-loop fixups on" current)
                      (split-string (review-guide-local--git
                                     directory '(0) "for-each-ref"
                                     "--format=%(refname:short)" "refs/heads/")
                                    "\n" t)
                      nil nil nil nil current))
             (default (review-guide-loop--default-base directory branch)))
        (list directory branch
              (completing-read (format-prompt "Since" default)
                               (split-string (review-guide-local--git
                                              directory '(0) "for-each-ref"
                                              "--format=%(refname:short)")
                                             "\n" t)
                               nil nil nil nil default))))))

;;;###autoload
(defun review-guide-loop (directory branch &optional base)
  "Show the review guide for the review loop's fixups on BRANCH.
DIRECTORY is the git repository.  The fixups are the commits since
where BASE and BRANCH diverged that carry a Review-Thread trailer,
one chunk each, in round order.  BASE defaults to the local branch
below BRANCH in its stack, or else the default branch.

Interactively, review the current branch; with a prefix argument, ask
for BRANCH and BASE."
  (interactive (review-guide-loop--read-args))
  (let* ((base (or base
                   (review-guide-loop--default-base directory branch)
                   (user-error "No default branch to look for fixups since")))
         (merge-base (string-trim (review-guide-local--git
                                   directory '(0 1 128) "merge-base" base branch)))
         (source (review-guide-loop-create
                  :directory (file-name-as-directory directory)
                  :base (if (string-empty-p merge-base)
                            (or (review-guide-local--rev directory base)
                                (user-error "No such revision: %s" base))
                          merge-base)
                  :base-label base
                  :head branch)))
    (unless (review-guide-loop--fixups source)
      (user-error "No review-loop fixups on %s since %s" branch base))
    (review-guide-open source)))

(provide 'review-guide-loop)
;;; review-guide-loop.el ends here
