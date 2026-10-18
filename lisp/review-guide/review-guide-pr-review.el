;;; review-guide-pr-review.el --- Pull requests open in pr-review -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A `pr-review' buffer as a review-guide source.  Comments go to the
;; buffer's pending review, which `pr-review' submits to GitHub.
;; Everything that reaches into `pr-review' internals is in this file,
;; so a change there has one place to land.

;;; Code:

(require 'cl-lib)
(require 'review-guide-source)

(defvar pr-review--pr-info)
(defvar pr-review--pr-path)
(defvar pr-review--pending-review-threads)
(declare-function pr-review-mode "pr-review")
(declare-function pr-review-open-url "pr-review")
(declare-function pr-review-submit-review "pr-review-action")
(declare-function pr-review--fetch-compare-cached "pr-review-api")
(declare-function pr-review--current-commit-base "pr-review-api")
(declare-function pr-review--current-commit-head "pr-review-api")
(declare-function pr-review--goto-diff-line "pr-review-render")
(declare-function pr-review--open-input-buffer "pr-review-input")
(declare-function pr-review--add-pending-review-thread-exit-callback "pr-review-action")
(declare-function review-guide-open "review-guide")

(cl-defmethod review-guide-source-diff ((source buffer))
  (with-current-buffer source
    (pr-review--fetch-compare-cached (pr-review--current-commit-base)
                                     (pr-review--current-commit-head))))

(cl-defmethod review-guide-source-describe ((source buffer))
  (with-current-buffer source
    (pcase-let ((`(,owner ,repo ,number) pr-review--pr-path))
      (let-alist pr-review--pr-info
        (list (format "%s/%s#%s" owner repo number) .title .body)))))

(cl-defmethod review-guide-source-threads ((source buffer))
  (buffer-local-value 'pr-review--pending-review-threads source))

(cl-defmethod review-guide-source-visit ((source buffer) path side line &optional _commit)
  (pop-to-buffer source)
  (unless (pr-review--goto-diff-line path side line)
    (message "Line %s:%s of %s is not shown in %s" side line path (buffer-name source))))

(cl-defmethod review-guide-source-add-thread ((source buffer) thread initial on-add)
  (with-current-buffer source
    (pr-review--open-input-buffer
     (format "Comment on %s." (alist-get 'path thread))
     (when initial (lambda () (insert initial)))
     (lambda (body)
       (pr-review--add-pending-review-thread-exit-callback source thread body)
       (funcall on-add)))))

(cl-defmethod review-guide-source-submit ((source buffer) _on-done)
  "Choose whether the review comments, approves or requests changes,
and write its top-level comment, in `pr-review'."
  (pop-to-buffer source)
  (call-interactively #'pr-review-submit-review))

(defun review-guide-pr-review--open (url)
  "Open the pull request at URL in `pr-review' and return its buffer."
  (save-window-excursion
    (pr-review-open-url url)
    (current-buffer)))

(declare-function forge-current-pullreq "forge-pullreq")
(declare-function forge-get-url "forge-commands")

(defun review-guide-pr-review--read-source (&optional read-url)
  "Return the `pr-review' buffer to guide, opening one if needed.
Use the current buffer when it is one, the forge pull request at
point when there is one, and otherwise the URL that READ-URL, a
function of no arguments, returns, or else one read from the
minibuffer."
  (cond
   ((derived-mode-p 'pr-review-mode) (current-buffer))
   ((and (fboundp 'forge-current-pullreq) (forge-current-pullreq))
    (review-guide-pr-review--open (forge-get-url (forge-current-pullreq))))
   (t (review-guide-pr-review--open
       (if read-url
           (funcall read-url)
         (read-string "Pull request URL: "))))))

(defun review-guide-pr-review-head (source)
  "Return (OWNER NAME NUMBER HEAD) for the pull request in SOURCE.
SOURCE is a `pr-review' buffer showing a GitHub pull request.  NUMBER
is an integer, and HEAD the commit SOURCE's diff ends at: the pull
request's head, unless only some of its commits are selected."
  (with-current-buffer source
    (pcase-let ((`(,owner ,name ,number) pr-review--pr-path))
      (unless (alist-get 'headRefOid pr-review--pr-info)
        (user-error "%s is not a GitHub pull request" (buffer-name)))
      (list owner name
            (if (stringp number) (string-to-number number) number)
            (pr-review--current-commit-head)))))

;;;###autoload
(defun review-guide (source)
  "Show the review guide for the pull request in the `pr-review' buffer SOURCE.
Interactively, SOURCE is the current `pr-review' buffer, the forge pull
request at point, or a URL read from the minibuffer."
  (interactive (list (review-guide-pr-review--read-source)))
  (review-guide-open source))

(provide 'review-guide-pr-review)
;;; review-guide-pr-review.el ends here
