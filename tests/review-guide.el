;;; review-guide.el --- review-guide tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name "../lisp/review-guide"
                               (file-name-directory load-file-name)))
(require 'review-guide-diff)
(require 'review-guide-ai)

(defconst review-guide-test--diff "\
diff --git a/lib/a.el b/lib/a.el
index 1111111..2222222 100644
--- a/lib/a.el
+++ b/lib/a.el
@@ -1,4 +1,5 @@ (defun a ()
 one
-two
+TWO
+three

 four
@@ -10,2 +11,2 @@
-ten
+TEN
 eleven
diff --git a/lib/new.el b/lib/new.el
new file mode 100644
--- /dev/null
+++ b/lib/new.el
@@ -0,0 +1,2 @@
+first
+second
\\ No newline at end of file
diff --git a/old.el b/old.el
deleted file mode 100644
--- a/old.el
+++ /dev/null
@@ -1 +0,0 @@
-gone
diff --git a/logo.png b/logo.png
Binary files a/logo.png and b/logo.png differ
")

(defun review-guide-test--hunks ()
  (review-guide-diff-parse review-guide-test--diff))

(ert-deftest review-guide-diff-parses-hunks-across-files ()
  (let ((hunks (review-guide-test--hunks)))
    (should (equal (mapcar #'review-guide-hunk-id hunks) '("H1" "H2" "H3" "H4")))
    (should (equal (mapcar #'review-guide-hunk-path hunks)
                   '("lib/a.el" "lib/a.el" "lib/new.el" "old.el")))
    (should (equal (mapcar #'review-guide-hunk-old-count hunks) '(4 2 0 1)))))

(ert-deftest review-guide-diff-numbers-lines-by-side ()
  (should (equal (review-guide-hunk-coordinates (car (review-guide-test--hunks)))
                 ;; The blank line lost its leading space and is context.
                 '((" one" "RIGHT" 1) ("-two" "LEFT" 2) ("+TWO" "RIGHT" 2)
                   ("+three" "RIGHT" 3) (" " "RIGHT" 4) (" four" "RIGHT" 5))))
  (should (equal (review-guide-hunk-coordinates (nth 2 (review-guide-test--hunks)))
                 '(("+first" "RIGHT" 1) ("+second" "RIGHT" 2)))))

(ert-deftest review-guide-diff-accepts-unprefixed-paths ()
  (let ((hunks (review-guide-diff-parse "\
diff --git lib/a.el lib/a.el
--- lib/a.el
+++ lib/a.el
@@ -1 +1 @@
-a
+b
")))
    (should (equal (mapcar #'review-guide-hunk-path hunks) '("lib/a.el")))))

(ert-deftest review-guide-diff-formats-hunks-with-coordinates ()
  (should (equal (review-guide-hunk-format (nth 1 (review-guide-test--hunks)))
                 "### H2 lib/a.el\n@@ -10,2 +11,2 @@\nL10   |-ten\nR11   |+TEN\nR12   | eleven\n")))

(ert-deftest review-guide-chunks-place-every-hunk-once ()
  (let* ((hunks (review-guide-test--hunks))
         (chunks (review-guide-ai-chunks
                  '((overview . "o")
                    (chunks ((title . "New file") (summary . "s1") (hunks "H3" "H9"))
                            ((title . "Edits") (summary . "s2") (hunks "H1" "H3"))
                            ((title . "Ghost") (summary . "s3") (hunks "H7"))))
                  hunks)))
    (should (equal (mapcar #'review-guide-chunk-title chunks)
                   '("New file" "Edits" "Not placed by the model")))
    (should (equal (mapcar (lambda (c)
                             (mapcar #'review-guide-hunk-id (review-guide-chunk-hunks c)))
                           chunks)
                   '(("H3") ("H1") ("H2" "H4"))))))

(ert-deftest review-guide-findings-keep-only-lines-in-the-diff ()
  (pcase-let* ((hunks (review-guide-test--hunks))
               (`(,findings . ,dropped)
                (review-guide-ai-findings
                 '((summary . "s")
                   (comments
                    ((hunk . "H1") (side . "RIGHT") (start_line . 3) (line . 2)
                     (severity . "issue") (body . " Range, reversed. "))
                    ((hunk . "H1") (side . "LEFT") (start_line . 2) (line . 2)
                     (severity . "nit") (body . "Removed line."))
                    ((hunk . "H1") (side . "LEFT") (start_line . 3) (line . 3)
                     (severity . "nit") (body . "Line 3 was not removed."))
                    ((hunk . "H2") (side . "RIGHT") (start_line . 2) (line . 12)
                     (severity . "issue") (body . "Starts outside the hunk."))
                    ((hunk . "H8") (side . "RIGHT") (start_line . 1) (line . 1)
                     (severity . "issue") (body . "No such hunk."))))
                 hunks)))
    (should (= dropped 3))
    (should (equal (mapcar #'review-guide-finding-thread findings)
                   '(((startLine . 2) (startSide . "RIGHT")
                      (path . "lib/a.el") (line . 3) (side . "RIGHT"))
                     ((path . "lib/a.el") (line . 2) (side . "LEFT")))))
    (should (equal (review-guide-finding-body (car findings)) "Range, reversed."))
    (should (eq (review-guide-finding-severity (car findings)) 'issue))))

(ert-deftest review-guide-reply-may-be-fenced ()
  (should (equal (review-guide-ai--parse "```json\n{\"a\": [1]}\n```")
                 '((a 1)))))

(ert-deftest review-guide-refuses-oversized-diffs ()
  (let ((review-guide-max-diff-chars 50))
    (should-error (review-guide-ai-prompt "t" "d" (review-guide-test--hunks))
                  :type 'user-error)))

;;;; The guide buffer, with a stub source and the model stubbed out

(require 'review-guide-source)

(cl-defstruct review-guide-test-source
  "A source over the test diff, recording what the guide sends it."
  threads added)

(cl-defmethod review-guide-source-diff ((_ review-guide-test-source))
  review-guide-test--diff)

(cl-defmethod review-guide-source-describe ((_ review-guide-test-source))
  '("o/r#1" "Title" "Description"))

(cl-defmethod review-guide-source-threads ((source review-guide-test-source))
  (review-guide-test-source-threads source))

(cl-defmethod review-guide-source-add-thread
  ((source review-guide-test-source) thread initial on-add)
  (push (cons thread initial) (review-guide-test-source-added source))
  (funcall on-add))

(defmacro review-guide-test--with-guide (threads &rest body)
  "Run BODY in a guide over the test diff, with pending THREADS.
The model replies at once.  BODY sees the guide's source as `source'."
  (declare (indent 1))
  `(let ((source (make-review-guide-test-source :threads ,threads)))
     (unwind-protect
         (cl-letf (((symbol-function 'review-guide-ai-group)
                    (lambda (_title _desc hunks _buffer callback _errback)
                      (funcall callback "Overview."
                               (review-guide-ai-chunks
                                '((chunks ((title . "Edits") (summary . "Why.")
                                           (hunks "H2" "H1"))))
                                hunks))))
                   ((symbol-function 'review-guide-ai-review)
                    (lambda (_title _desc hunks _buffer callback _errback)
                      (pcase-let ((`(,findings . ,_)
                                   (review-guide-ai-findings
                                    '((comments ((hunk . "H2") (side . "RIGHT")
                                                 (start_line . 11) (line . 11)
                                                 (severity . "issue")
                                                 (body . "Why upper case?"))))
                                    hunks)))
                        (funcall callback "Looks risky." findings 0)))))
           (save-window-excursion
             (review-guide-open source)
             ,@body))
       (when-let* ((guide (get-buffer "*review-guide o/r#1*")))
         (kill-buffer guide)))))

(defun review-guide-test--goto (text)
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(ert-deftest review-guide-buffer-shows-chunks-in-the-model-order ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-guide nil
    (let ((text (buffer-string)))
      (should (string-match-p "Overview\\." text))
      (should (< (string-search "1. Edits" text)
                 (string-search "@@ -10,2 +11,2 @@" text)
                 (string-search "@@ -1,4 +1,5 @@" text)
                 (string-search "2. Not placed by the model" text)
                 (string-search "lib/new.el" text))))))

(ert-deftest review-guide-buffer-comments-on-a-range ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-guide nil
    (review-guide-test--goto "-two")
    (let ((beg (point)))
      (review-guide-test--goto "+three")
      (review-guide-comment beg (point)))
    (should (equal (review-guide-test-source-added source)
                   '((((startLine . 2) (startSide . "LEFT")
                       (path . "lib/a.el") (line . 3) (side . "RIGHT"))))))))

(ert-deftest review-guide-buffer-forwards-and-dismisses-findings ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-guide
      '(((path . "lib/a.el") (line . 1) (side . "RIGHT") (body . "Mine.")))
    (should (string-match-p "Pending comment on RIGHT 1" (buffer-string)))
    (review-guide-review)
    (should (string-match-p "Looks risky\\." (buffer-string)))
    (review-guide-test--goto "AI issue on RIGHT 11")
    (review-guide-dismiss)
    (should (string-match-p "AI issue on RIGHT 11 (dismissed)" (buffer-string)))
    (review-guide-test--goto "AI issue on RIGHT 11")
    (review-guide-dismiss)
    (review-guide-forward)
    (should (equal (review-guide-test-source-added source)
                   '((((path . "lib/a.el") (line . 11) (side . "RIGHT"))
                      . "Why upper case?"))))
    (should (string-match-p "AI issue on RIGHT 11 (forwarded)" (buffer-string)))
    (review-guide-test--goto "AI issue on RIGHT 11")
    (should-error (review-guide-forward) :type 'user-error)))

;;;; Local changes

(require 'review-guide-local)

(defmacro review-guide-test--with-repo (&rest body)
  "Run BODY with `repo' bound to a fresh repository of local changes.
On branch work, branched from main, a.txt has one committed change
and one uncommitted one, new.txt is untracked, and x.log is ignored.
Git ignores the user's configuration, so it neither signs nor hooks."
  (declare (indent 0))
  `(let* ((repo (file-name-as-directory
                 (file-truename (make-temp-file "review-guide-repo" t))))
          (process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1"
                     "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@example.com"
                     "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@example.com")
                   process-environment)))
     (unwind-protect
         (progn
           (review-guide-test--git repo "init" "--quiet" "--initial-branch=main")
           (review-guide-test--write repo "a.txt" "one\ntwo\nthree\n")
           (review-guide-test--write repo ".gitignore" "*.log\n")
           (review-guide-test--git repo "add" "a.txt" ".gitignore")
           (review-guide-test--git repo "commit" "--quiet" "-m" "Base")
           (review-guide-test--git repo "switch" "--quiet" "-c" "work")
           (review-guide-test--write repo "a.txt" "one\nTWO\nthree\n")
           (review-guide-test--git repo "commit" "--quiet" "-am" "Shout two\n\nBecause.")
           (review-guide-test--write repo "a.txt" "one\nTWO\nTHREE\n")
           (review-guide-test--write repo "new.txt" "fresh\n")
           (review-guide-test--write repo "x.log" "noise\n")
           ,@body)
       (delete-directory repo t))))

(defun review-guide-test--git (repo &rest args)
  (apply #'review-guide-local--git repo '(0) args))

(defun review-guide-test--write (repo file text)
  (with-temp-file (expand-file-name file repo)
    (insert text)))

(defun review-guide-test--local (repo &optional head)
  (review-guide-local-create
   :directory repo :base-label "main" :head head
   :base (string-trim (review-guide-test--git repo "merge-base" "main" "HEAD"))))

(defun review-guide-test--diff-lines (source)
  (mapcan (lambda (hunk) (copy-sequence (review-guide-hunk-lines hunk)))
          (review-guide-diff-parse (review-guide-source-diff source))))

(ert-deftest review-guide-local-working-tree-includes-untracked-files ()
  (review-guide-test--with-repo
    (let ((source (review-guide-test--local repo)))
      (should (equal (mapcar #'review-guide-hunk-path
                             (review-guide-diff-parse (review-guide-source-diff source)))
                     '("a.txt" "new.txt")))
      (should (equal (review-guide-test--diff-lines source)
                     '(" one" "-two" "-three" "+TWO" "+THREE" "+fresh"))))))

(ert-deftest review-guide-local-revision-excludes-the-working-tree ()
  (review-guide-test--with-repo
    (should (equal (review-guide-test--diff-lines (review-guide-test--local repo "HEAD"))
                   '(" one" "-two" "+TWO" " three")))))

(ert-deftest review-guide-local-describes-the-commits ()
  (review-guide-test--with-repo
    (pcase-let ((`(,name ,title ,description)
                 (review-guide-source-describe (review-guide-test--local repo))))
      (should (equal name (format "%s main..working tree"
                                  (file-name-nondirectory (directory-file-name repo)))))
      (should (equal title "Local changes on work"))
      (should (equal description "Commit messages:\n\n* Shout two\n\nBecause.\n\n")))))

(ert-deftest review-guide-local-defaults-to-the-default-branch ()
  (review-guide-test--with-repo
    (let ((default-directory repo)
          (current-prefix-arg nil))
      (should (equal (review-guide-local--read-args)
                     (list (directory-file-name repo) "main"))))))

(ert-deftest review-guide-local-visits-the-base-side ()
  (review-guide-test--with-repo
    (let ((source (review-guide-test--local repo)))
      (save-window-excursion
        (review-guide-source-visit source "a.txt" "LEFT" 2)
        (unwind-protect
            (progn
              (should buffer-read-only)
              (should (equal (buffer-substring (point) (line-end-position)) "two")))
          (kill-buffer))))))

(ert-deftest review-guide-local-submits-comments-as-markdown ()
  (review-guide-test--with-repo
    (let ((source (review-guide-test--local repo))
          (kill-ring nil))
      (setf (review-guide-local-threads source)
            '(((path . "new.txt") (line . 1) (side . "RIGHT") (body . "Is this needed?"))
              ((startLine . 2) (startSide . "LEFT")
               (path . "a.txt") (line . 2) (side . "RIGHT") (body . " Why shout? "))))
      (save-window-excursion
        (review-guide-source-submit source)
        (kill-buffer))
      (should (equal (car kill-ring)
                     (concat
                      "Review comments on "
                      (car (review-guide-source-describe source)) ".\n"
                      "\n## a.txt, removed line 2 to line 2\n\n"
                      "```diff\n-two\n-three\n+TWO\n```\n\n"
                      "Why shout?\n"
                      "\n## new.txt, line 1\n\n"
                      "```diff\n+fresh\n```\n\n"
                      "Is this needed?\n")))
      (should-not (review-guide-local-threads source)))))

(ert-deftest review-guide-local-adds-and-deletes-comments ()
  (let ((source (review-guide-local-create))
        (added 0))
    (save-window-excursion
      (review-guide-source-add-thread
       source '((path . "a.txt") (line . 2) (side . "RIGHT")) "Draft"
       (lambda () (cl-incf added)))
      (should (equal (buffer-string) "Draft"))
      (goto-char (point-max))
      (insert ", edited.")
      (review-guide-comment-finish))
    (should (= added 1))
    (should (equal (review-guide-source-threads source)
                   '(((path . "a.txt") (line . 2) (side . "RIGHT")
                      (body . "Draft, edited.")))))
    (save-window-excursion
      (review-guide-source-add-thread
       source '((path . "a.txt") (line . 3) (side . "RIGHT")) nil
       (lambda () (cl-incf added)))
      (review-guide-comment-abort))
    (should (= added 1))
    (review-guide-source-delete-thread source (car (review-guide-source-threads source)))
    (should-not (review-guide-source-threads source))))

(ert-deftest review-guide-local-fences-code-holding-backticks ()
  (should (equal (review-guide-local--fence "+```elisp") "````"))
  (should (equal (review-guide-local--fence "+x") "```")))

;;; review-guide.el ends here
