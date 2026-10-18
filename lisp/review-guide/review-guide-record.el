;;; review-guide-record.el --- Review records pr-loop writes -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `pr-loop review OWNER/NAME#N' reviews someone else's pull request
;; at one head, and triage settles each finding as propose, rebut,
;; duplicate-of or needs-operator.  It keeps the result in a record,
;; DIR/OWNER/NAME/N/HEAD12.json, where DIR is
;; `review-guide-record-directory' and HEAD12 the head's first twelve
;; hex digits.  `review-guide-record' opens the pull request in
;; `pr-review', and its guide with the record's findings as proposals:
;; those to propose first, then those left for the operator, with
;; rebutted and duplicate findings shown dismissed.  Forwarding,
;; dismissing and submitting work as for the model's own proposals.
;;
;; A record is untrusted: its reviewer read the pull request's code,
;; and its text says whatever that code led it to.  Text from a record
;; reaches a buffer only through `review-guide-record-text', and the
;; guide inserts it as plain text, never evaluated, used as a format
;; string or inserted where it would be read as Org.  Forwarding a
;; finding puts its body into the Markdown of a pending comment, for
;; the operator to edit and post: that buffer must run nothing a code
;; block's language names.  Nothing a record names is opened or run:
;; the pull request is the guide's own, and the record's file is found
;; from checked parts of it.
;;
;; A finding with no lines in the diff can be dismissed but not
;; forwarded, since a review comment must sit on the diff's lines; the
;; operator quotes it in the review's own comment instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'review-guide-ai)
(require 'review-guide-diff)
(require 'review-guide-pr-review)

(declare-function review-guide-open "review-guide")
(declare-function review-guide-show-record "review-guide")

(defcustom review-guide-record-directory
  (expand-file-name "pr-review" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Where `pr-loop review' keeps its reviews.
This must be the \"dir\" under \"review\" in pr-loop's configuration,
which home-manager writes from wgn.home.llm.prLoop.reviewDir."
  :type 'directory
  :group 'review-guide)

(defvar review-guide-record-section-functions nil
  "Functions that insert sections of a review record into its guide.
Each is called with a `review-guide-record' in the guide buffer, at
point after the record's own section, and inserts magit sections
there.  The record's data slot is the record as parsed, untrusted:
pass any text from it through `review-guide-record-text' first.")

(cl-defstruct (review-guide-record (:constructor review-guide-record--create)
                                   (:copier nil))
  "A pr-loop review record, its text cleaned for display."
  (key nil :documentation "OWNER/NAME#N, from the parts the file was found by.")
  (head nil :documentation "The 40-digit head it reviewed.")
  (file nil :documentation "The file it was read from.")
  (round nil :documentation "Its latest review round, or nil before the first.")
  (rerun nil :documentation "Non-nil when it is from a later run on the same
head that has not finished a round of its own: round, reviewer and
model are then the earlier run's.")
  reviewer model ran not-run-because unverified
  (findings nil :documentation "Its findings from every round, as
`review-guide-finding's in the order the guide proposes them.  Those
with no hunk have no lines in the diff.")
  (data nil :documentation "The record as parsed, uncleaned: objects as
alists, arrays as vectors, null and false as nil."))

;;;; Untrusted text

(defun review-guide-record--unsafe-p (char)
  "Return non-nil when CHAR is removed from a review record's text.
That is every control character but newline and tab, which a
terminal could act on; every format character (Unicode category Cf),
which includes the bidirectional controls that make text display as
something other than what it holds, the zero-width characters that
hide text inside other text, and the tag characters; the line and
paragraph separators, which break a line where none shows; and the
characters that display as nothing: variation selectors, the
combining grapheme joiner and the Hangul fillers."
  (and (not (memq char '(?\n ?\t)))
       (or (memq (get-char-code-property char 'general-category) '(Cc Cf Zl Zp))
           (<= #xe0000 char #xe007f)
           (<= #xfe00 char #xfe0f)
           (<= #xe0100 char #xe01ef)
           (memq char '(#x34f #x115f #x1160 #x3164 #xffa0)))))

(defun review-guide-record-text (text &optional limit one-line)
  "Return TEXT from a review record cleaned for display, or nil.
Remove the characters `review-guide-record--unsafe-p' accepts and
any text properties, and cut the result to LIMIT characters, 8192 by
default.  With ONE-LINE, also turn each run of newlines and tabs into
one space: text shown inside a line of the guide must not start a
line of its own, where it could pass for the guide's.  Anything but a
string, or a string left empty, gives nil."
  (when (stringp text)
    (let* ((clean (substring-no-properties
                   (cl-remove-if #'review-guide-record--unsafe-p text)))
           (clean (if one-line
                      (replace-regexp-in-string (rx (+ (any "\n\t"))) " " clean t t)
                    clean))
           (limit (or limit 8192)))
      (cond ((string-empty-p (string-trim clean)) nil)
            ((> (length clean) limit)
             (concat (substring clean 0 limit) (if one-line " [cut]" "\n[cut]")))
            (t clean)))))

;;;; Finding a record

(defconst review-guide-record--name-regexp
  (rx bos (any "A-Za-z0-9_") (* (any "A-Za-z0-9_.-")) eos)
  "A GitHub owner or repository name, as pr-loop accepts one.
It cannot be \".\" or \"..\", so it names no directory above.")

(defconst review-guide-record--head-regexp (rx bos (= 40 (any "0-9a-f")) eos))

(defconst review-guide-record--file-regexp (rx bos (= 12 (any "0-9a-f")) ".json" eos))

(defconst review-guide-record--max-size (* 16 1024 1024)
  "The largest record read.  Each text in one is at most 8 KiB.")

(defun review-guide-record--child (directory name)
  "Return the entry NAME of DIRECTORY, matched ignoring case, or nil.
GitHub ignores case in owners and names, and a URL may not keep it."
  (let ((exact (expand-file-name name directory)))
    (if (file-directory-p exact)
        exact
      (when-let* ((found (and (file-directory-p directory)
                              (cl-find-if (lambda (entry)
                                            (and (string-match-p review-guide-record--name-regexp
                                                                 entry)
                                                 (string-equal-ignore-case entry name)))
                                          (directory-files directory nil nil t)))))
        (expand-file-name found directory)))))

(defun review-guide-record--pr-directory (owner name number)
  "Return the directory of OWNER/NAME#NUMBER's reviews, or nil.
Signal an error when the parts are not a pull request's."
  (unless (and (stringp owner) (string-match-p review-guide-record--name-regexp owner)
               (stringp name) (string-match-p review-guide-record--name-regexp name)
               (natnump number) (> number 0))
    (user-error "%s/%s#%s is not a GitHub pull request" owner name number))
  (when-let* ((repo (review-guide-record--child
                     (or (review-guide-record--child review-guide-record-directory owner)
                         (expand-file-name owner review-guide-record-directory))
                     name)))
    (let ((directory (expand-file-name (number-to-string number) repo)))
      (and (file-directory-p directory) directory))))

(defun review-guide-record--heads (directory)
  "Return the HEAD12s DIRECTORY holds records of, newest first.
A record is a regular file, or a link to one."
  (mapcar (lambda (file) (file-name-sans-extension (car file)))
          (sort (cl-remove-if-not
                 (lambda (file)
                   (and (string-match-p review-guide-record--file-regexp (car file))
                        (file-regular-p (expand-file-name (car file) directory))))
                 (directory-files-and-attributes directory nil nil t))
                (lambda (a b) (time-less-p (file-attribute-modification-time (cdr b))
                                           (file-attribute-modification-time (cdr a)))))))

(defun review-guide-record--read (file)
  "Return the JSON object in FILE as an alist, its arrays as vectors.
The file is decoded as UTF-8 and nothing else: no format or file
handler acts on it."
  (when (> (file-attribute-size (file-attributes (file-truename file)))
           review-guide-record--max-size)
    (user-error "%s is too large to be a review record" file))
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert-file-contents-literally file)
    (decode-coding-region (point-min) (point-max) 'utf-8)
    (goto-char (point-min))
    (condition-case err
        (json-parse-buffer :object-type 'alist :array-type 'array
                           :null-object nil :false-object nil)
      (json-error
       (user-error "%s is not a review record: %s" file (error-message-string err))))))

;;;; Reading a record

(defconst review-guide-record--outcomes
  '("propose" "needs-operator" nil "rebut" "duplicate-of")
  "The outcomes triage settles findings with, nil for none yet, in the
order the guide proposes them.")

(defconst review-guide-record--severities '("issue" "suggestion" "question" "nit"))

(defconst review-guide-record--id-regexp (rx bos (+ digit) ".F" (+ digit) eos)
  "A finding's id across rounds, ROUND.ID, such as 1.F2.")

(defun review-guide-record--finding (finding)
  "Return the record's FINDING as a `review-guide-finding', with no hunk.
Its lines are those of the head, on the RIGHT side of the diff."
  (let-alist finding
    (let* ((line (and (natnump .line) (> .line 0) .line))
           (end (if (and line (natnump .endLine) (>= .endLine line)) .endLine line))
           (outcome (car (member .outcome (remq nil review-guide-record--outcomes))))
           (id (and (natnump .round) (stringp .id)
                    (let ((id (format "%d.%s" .round .id)))
                      (and (string-match-p review-guide-record--id-regexp id) id))))
           (of (and (equal outcome "duplicate-of") (stringp .duplicateOf)
                    (string-match-p review-guide-record--id-regexp .duplicateOf)
                    .duplicateOf)))
      (review-guide-finding-create
       :id id
       :path (review-guide-record-text .path 1024 t)
       :start-line line :line end
       :severity (intern (or (car (member .severity review-guide-record--severities))
                             "suggestion"))
       :body (or (review-guide-record-text .body) "")
       :outcome (if of (concat outcome " " of) outcome)
       :state (if (member outcome '("rebut" "duplicate-of")) 'dismissed 'proposed)))))

(defun review-guide-record--rank (finding)
  "Return where FINDING's outcome comes in `review-guide-record--outcomes'."
  (let ((outcome (car (split-string (or (review-guide-finding-outcome finding) "")))))
    (or (cl-position outcome review-guide-record--outcomes :test #'equal) 2)))

(defun review-guide-record--object-p (value)
  "Return non-nil when VALUE is a parsed JSON object."
  (and (listp value)
       (cl-every (lambda (member) (and (consp member) (symbolp (car member)))) value)))

(defun review-guide-record--array (value what)
  "Return the parsed JSON array VALUE as a list, or nil for null.
Signal a `user-error' naming WHAT when VALUE is anything else."
  (cond ((null value) nil)
        ((vectorp value) (append value nil))
        (t (user-error "%s is not an array" what))))

(defun review-guide-record--objects (value what)
  "Return the parsed JSON array of objects VALUE as a list of alists.
Signal a `user-error' naming WHAT when it is not one."
  (let ((objects (review-guide-record--array value what)))
    (unless (cl-every #'review-guide-record--object-p objects)
      (user-error "%s holds something other than objects" what))
    objects))

(defun review-guide-record--build (owner name number head file data)
  "Return the record of OWNER/NAME#NUMBER at HEAD that FILE holds.
DATA is FILE as parsed.  Signal a `user-error' when it is not that
record."
  (unless (review-guide-record--object-p data)
    (user-error "it is not a JSON object"))
  (let-alist data
    (unless (eql .version 1)
      (user-error "it is of a version this guide cannot read"))
    (unless (and (stringp .repo)
                 (string-equal-ignore-case .repo (format "%s/%s" owner name))
                 (eql .pr number)
                 (equal .head head))
      (user-error "it is not the review of %s/%s#%d at %s" owner name number
                  (substring head 0 12)))
    (let* ((prior (review-guide-record--objects .prior "its prior field"))
           (round (and (natnump .round) (> .round 0) .round))
           ;; A run on a head reviewed before saves round 0, the earlier
           ;; rounds in prior, until its own round is done.
           (latest (if round data (car (last prior))))
           (latest-round (let ((n (alist-get 'round latest)))
                           (and (natnump n) (> n 0) n))))
      (review-guide-record--create
       :key (format "%s/%s#%d" owner name number) :head head :file file :data data
       :round latest-round
       :rerun (and (not round) latest-round t)
       :reviewer (and latest-round
                      (review-guide-record-text (alist-get 'reviewer latest) 100 t))
       :model (and latest-round
                   (review-guide-record-text (alist-get 'model latest) 100 t))
       :ran (eq .ran t)
       :not-run-because (review-guide-record-text .notRunBecause nil t)
       :unverified (delq nil (mapcar #'review-guide-record-text
                                     (review-guide-record--array .unverified
                                                                 "its unverified field")))
       :findings (cl-stable-sort
                  (mapcar #'review-guide-record--finding
                          (append (mapcan (lambda (round)
                                            (review-guide-record--objects
                                             (alist-get 'findings round)
                                             "a prior round's findings field"))
                                          prior)
                                  (review-guide-record--objects .findings
                                                                "its findings field")))
                  #'< :key #'review-guide-record--rank)))))

(defun review-guide-record-load (owner name number head)
  "Return the review record of OWNER/NAME#NUMBER at HEAD.
Signal a `user-error' when there is none, or the file is not that
pull request's record at HEAD."
  (unless (and (stringp head) (string-match-p review-guide-record--head-regexp head))
    (user-error "%S is not a commit" head))
  (let* ((key (format "%s/%s#%d" owner name number))
         (directory (or (review-guide-record--pr-directory owner name number)
                        (user-error "No review of %s under %s" key review-guide-record-directory)))
         (file (expand-file-name (concat (substring head 0 12) ".json") directory)))
    (unless (file-regular-p file)
      (user-error "No review of %s at %s%s" key (substring head 0 12)
                  (if-let* ((heads (review-guide-record--heads directory)))
                      (format "; reviewed at %s" (string-join heads ", "))
                    "")))
    (let ((data (review-guide-record--read file)))
      ;; Only the build's own messages are shown: another error's data
      ;; may hold the record's text.
      (condition-case err
          (review-guide-record--build owner name number head file data)
        (user-error
         (user-error "%s is not a review record: %s" file (cadr err)))
        (error
         (user-error "%s is not a review record in a shape this guide reads" file))))))

(defun review-guide-record-anchor (record hunks)
  "Anchor RECORD's findings to HUNKS, and return those anchored.
A finding is anchored to the first hunk of its path holding all its
lines on the RIGHT side; one with no such hunk is left without one."
  (cl-loop
   for finding in (review-guide-record-findings record)
   for path = (review-guide-finding-path finding)
   for start = (review-guide-finding-start-line finding)
   for end = (review-guide-finding-line finding)
   for hunk = (and path start
                   (cl-find-if (lambda (hunk)
                                 (and (equal (review-guide-hunk-path hunk) path)
                                      (review-guide-hunk-covers-p hunk "RIGHT" start)
                                      (review-guide-hunk-covers-p hunk "RIGHT" end)))
                               hunks))
   do (setf (review-guide-finding-hunk finding) (and hunk (review-guide-hunk-id hunk))
            (review-guide-finding-side finding) (and hunk "RIGHT")
            (review-guide-finding-commit finding) (and hunk (review-guide-hunk-commit hunk)))
   when hunk collect finding))

;;;; Entry point

(defun review-guide-record--keys ()
  "Return the OWNER/NAME#N of each pull request with a record, newest first."
  (let (found)
    (dolist (owner (and (file-directory-p review-guide-record-directory)
                        (directory-files review-guide-record-directory nil nil t)))
      (when (string-match-p review-guide-record--name-regexp owner)
        (let ((owner-directory (expand-file-name owner review-guide-record-directory)))
          (dolist (name (and (file-directory-p owner-directory)
                             (directory-files owner-directory nil nil t)))
            (when (string-match-p review-guide-record--name-regexp name)
              (let ((repo (expand-file-name name owner-directory)))
                (dolist (number (and (file-directory-p repo) (directory-files repo nil nil t)))
                  (when (string-match-p (rx bos (any "1-9") (* digit) eos) number)
                    (let ((directory (expand-file-name number repo)))
                      (when-let* ((head (and (file-directory-p directory)
                                             (car (review-guide-record--heads directory)))))
                        (push (cons (format "%s/%s#%s" owner name number)
                                    (file-attribute-modification-time
                                     (file-attributes
                                      (expand-file-name (concat head ".json") directory))))
                              found)))))))))))
    (mapcar #'car (sort found (lambda (a b) (time-less-p (cdr b) (cdr a)))))))

(defun review-guide-record--read-url ()
  "Read a pull request with a record, and return its URL."
  (let* ((keys (or (review-guide-record--keys)
                   (user-error "No review records under %s" review-guide-record-directory)))
         (key (completing-read "Review record of: " keys nil t)))
    (unless (string-match (rx bos (group (+ (not "/"))) "/" (group (+ (not "#")))
                              "#" (group (+ digit)) eos)
                          key)
      (user-error "Not a pull request: %s" key))
    (format "https://github.com/%s/%s/pull/%s"
            (match-string 1 key) (match-string 2 key) (match-string 3 key))))

;;;###autoload
(defun review-guide-record (source)
  "Show the review guide for SOURCE with its review record's findings.
SOURCE is a `pr-review' buffer, and the record the one pr-loop wrote
for the head its diff ends at.  Interactively, SOURCE is the current
`pr-review' buffer, the forge pull request at point, or a pull request
with a record, read from the minibuffer."
  (interactive (list (review-guide-pr-review--read-source
                      #'review-guide-record--read-url)))
  (let ((record (apply #'review-guide-record-load (review-guide-pr-review-head source))))
    (set-buffer (review-guide-open source))
    (review-guide-show-record record)))

(provide 'review-guide-record)
;;; review-guide-record.el ends here
