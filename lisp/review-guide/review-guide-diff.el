;;; review-guide-diff.el --- Unified diff hunks for review-guide -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Parse a unified diff into hunks that carry the coordinates GitHub
;; uses for review comments: a path, a side (LEFT for the base, RIGHT
;; for the head) and a line number on that side.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct (review-guide-hunk (:constructor review-guide-hunk-create)
                                 (:copier nil))
  "One hunk of a unified diff."
  (id nil :documentation "Identifier in diff order, such as \"H3\".")
  (path nil :documentation "Path on the head side, or the base side for a deletion.")
  (old-start 0) (old-count 0) (new-start 0) (new-count 0)
  (header nil :documentation "The \"@@ ... @@\" line, including any function context.")
  (lines nil :documentation "Body lines, each still prefixed with \" \", \"-\" or \"+\"."))

(defconst review-guide-diff--hunk-header-regexp
  (rx bol "@@ -" (group (+ digit)) (? "," (group (+ digit)))
      " +" (group (+ digit)) (? "," (group (+ digit))) " @@"))

(defun review-guide-diff--strip-prefix (path prefixed)
  "Return PATH without its a/ or b/ prefix when PREFIXED is non-nil.
Return nil for /dev/null."
  (cond ((equal path "/dev/null") nil)
        ((and prefixed (string-match (rx bos (or "a/" "b/")) path))
         (substring path (match-end 0)))
        (t path)))

(defun review-guide-diff-parse (diff)
  "Parse the unified DIFF string into a list of `review-guide-hunk's.
Hunks are numbered H1, H2, ... in the order they appear.  Both git's
a/ and b/ prefixed form and the unprefixed form that `pr-review'
fetches are accepted.  Files without hunks, such as binary files and
pure renames, contribute nothing."
  (let ((hunks nil) (n 0) (old-left 0) (new-left 0)
        old-path new-path prefixed current)
    (dolist (line (split-string diff "\n"))
      (cond
       ((string-prefix-p "diff --git " line)
        (when current (push current hunks))
        (setq current nil old-path nil new-path nil
              prefixed (string-prefix-p "diff --git a/" line)))
       ((and (null current) (string-prefix-p "--- " line))
        (setq old-path (review-guide-diff--strip-prefix
                        (string-trim-right (substring line 4) "\t.*") prefixed)))
       ((and (null current) (string-prefix-p "+++ " line))
        (setq new-path (review-guide-diff--strip-prefix
                        (string-trim-right (substring line 4) "\t.*") prefixed)))
       ((string-match review-guide-diff--hunk-header-regexp line)
        (when current (push current hunks))
        (setq current
              (review-guide-hunk-create
               :id (format "H%d" (cl-incf n))
               :path (or new-path old-path)
               :old-start (string-to-number (match-string 1 line))
               :old-count (if (match-string 2 line)
                              (string-to-number (match-string 2 line))
                            1)
               :new-start (string-to-number (match-string 3 line))
               :new-count (if (match-string 4 line)
                              (string-to-number (match-string 4 line))
                            1)
               :header line)
              old-left (review-guide-hunk-old-count current)
              new-left (review-guide-hunk-new-count current)))
       ((and current (string-prefix-p "\\" line))
        (push line (review-guide-hunk-lines current)))
       ;; The header's counts say where the body ends.  A blank line
       ;; before then is a context line whose leading space was lost.
       ((and current (or (> old-left 0) (> new-left 0))
             (string-match-p (rx bos (or (any " +-") eos)) line))
        (let ((line (if (string-empty-p line) " " line)))
          (pcase (aref line 0)
            (?- (cl-decf old-left))
            (?+ (cl-decf new-left))
            (_ (cl-decf old-left) (cl-decf new-left)))
          (push line (review-guide-hunk-lines current))))
       (current
        (push current hunks)
        (setq current nil))))
    (when current (push current hunks))
    (dolist (hunk hunks)
      (setf (review-guide-hunk-lines hunk)
            (nreverse (review-guide-hunk-lines hunk))))
    (nreverse hunks)))

(defun review-guide-hunk-coordinates (hunk)
  "Return the lines of HUNK as a list of (TEXT SIDE LINE).
SIDE is \"LEFT\" for removed lines and \"RIGHT\" for added and context
lines, which is where GitHub anchors a comment on each.  The
\"\\ No newline at end of file\" marker is skipped."
  (let ((old (review-guide-hunk-old-start hunk))
        (new (review-guide-hunk-new-start hunk))
        result)
    (dolist (text (review-guide-hunk-lines hunk))
      (pcase (and (> (length text) 0) (aref text 0))
        (?- (push (list text "LEFT" old) result)
            (cl-incf old))
        (?+ (push (list text "RIGHT" new) result)
            (cl-incf new))
        (?\\)
        (_ (push (list text "RIGHT" new) result)
           (cl-incf old)
           (cl-incf new))))
    (nreverse result)))

(defun review-guide-hunk-covers-p (hunk side line)
  "Return non-nil when HUNK has a commentable LINE on SIDE."
  (cl-some (pcase-lambda (`(,_ ,s ,l)) (and (equal s side) (eql l line)))
           (review-guide-hunk-coordinates hunk)))

(provide 'review-guide-diff)
;;; review-guide-diff.el ends here
