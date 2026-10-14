;;; review-guide-source.el --- Where a review-guide diff comes from -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A source supplies the diff a guide shows and keeps the comments
;; written on it until they are submitted.  Pending comments are
;; alists of GitHub's draft review thread fields: path, line and side,
;; startLine and startSide for a range, and body.  When the diff is a
;; series of commits, each comment also has a commit field naming the
;; commit its lines are numbered in; see `review-guide-hunk-commit'.

;;; Code:

(require 'cl-lib)

(cl-defgeneric review-guide-source-diff (source)
  "Return the unified diff SOURCE is reviewing.")

(cl-defgeneric review-guide-source-describe (source)
  "Return (NAME TITLE DESCRIPTION) for SOURCE.
NAME identifies the guide's buffer.  TITLE and DESCRIPTION tell the
model what the change is for; either may be nil.")

(cl-defgeneric review-guide-source-chunks (source hunks)
  "Return SOURCE's own arrangement of HUNKS into `review-guide-chunk's.
HUNKS are parsed from SOURCE's diff.  Return nil to have the model
arrange them instead."
  (ignore source hunks)
  nil)

(cl-defgeneric review-guide-source-threads (source)
  "Return the pending comments in SOURCE.")

(cl-defgeneric review-guide-source-add-thread (source thread initial on-add)
  "Edit a comment for THREAD, starting from INITIAL, then add it to SOURCE.
THREAD has no body yet.  Call ON-ADD with no arguments once the
comment is added.")

(cl-defgeneric review-guide-source-delete-thread (source thread)
  "Remove the pending comment THREAD from SOURCE."
  (ignore thread)
  (user-error "Edit this comment where %s keeps it"
              (car (review-guide-source-describe source))))

(cl-defgeneric review-guide-source-visit (source path side line &optional commit)
  "Show LINE of PATH on SIDE of SOURCE's diff, in another window.
COMMIT is the `review-guide-hunk-commit' of the hunk holding the line.")

(cl-defgeneric review-guide-source-submit (source on-done)
  "Send the pending comments in SOURCE on to wherever they belong.
Call ON-DONE with no arguments once they have gone, when SOURCE can
tell.")

(provide 'review-guide-source)
;;; review-guide-source.el ends here
