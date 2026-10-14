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

(ert-deftest review-guide-diff-records-each-hunks-commit ()
  (let* ((one (make-string 40 ?1))
         (two (make-string 40 ?2))
         (hunks (review-guide-diff-parse
                 (concat "commit " one "\n" review-guide-test--diff
                         "commit " two "\n"
                         "diff --git a/lib/a.el b/lib/a.el\n--- a/lib/a.el\n+++ b/lib/a.el\n"
                         "@@ -1 +1 @@\n-a\n+b\n"))))
    (should (equal (mapcar #'review-guide-hunk-commit hunks) (list one one one one two)))
    (should (equal (review-guide-hunk-lines (nth 3 hunks)) '("-gone")))
    (should (string-prefix-p "### H5 lib/a.el in commit 222222222222\n"
                             (review-guide-hunk-format (nth 4 hunks))))
    (should-not (review-guide-hunk-commit (car (review-guide-test--hunks))))))

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

(defconst review-guide-test--threads
  '(((path . "new.txt") (line . 1) (side . "RIGHT") (body . "Is this needed?"))
    ((startLine . 2) (startSide . "LEFT")
     (path . "a.txt") (line . 2) (side . "RIGHT") (body . " Why shout? "))))

(ert-deftest review-guide-local-submits-comments-as-markdown ()
  (review-guide-test--with-repo
    (let ((source (review-guide-test--local repo))
          (later '((path . "a.txt") (line . 3) (side . "RIGHT") (body . "Later.")))
          done sent)
      (setf (review-guide-local-threads source)
            (copy-sequence review-guide-test--threads))
      (save-window-excursion
        (review-guide-source-submit source (lambda () (setq done t)))
        (should review-guide-feedback-mode)
        (should (equal (buffer-string)
                       (concat
                        "Review comments on "
                        (car (review-guide-source-describe source)) ".\n"
                        "\n## a.txt, removed line 2 to line 2\n\n"
                        "```diff\n-two\n-three\n+TWO\n```\n\n"
                        "Why shout?\n"
                        "\n## new.txt, line 1\n\n"
                        "```diff\n+fresh\n```\n\n"
                        "Is this needed?\n")))
        (goto-char (point-max))
        (insert "\nThanks.\n")
        ;; A comment written while the feedback is open is not in it.
        (push later (review-guide-local-threads source))
        (review-guide-feedback-send (cons "Test" (lambda (text) (setq sent text)))))
      (should done)
      (should (string-suffix-p "Is this needed?\n\nThanks." sent))
      (should (equal (review-guide-local-threads source) (list later))))))

(ert-deftest review-guide-local-cancelling-feedback-keeps-comments ()
  (review-guide-test--with-repo
    (let ((source (review-guide-test--local repo))
          done)
      (setf (review-guide-local-threads source)
            (copy-sequence review-guide-test--threads))
      (save-window-excursion
        (review-guide-source-submit source (lambda () (setq done t)))
        (review-guide-feedback-cancel))
      (should-not done)
      (should (equal (review-guide-local-threads source) review-guide-test--threads)))))

(define-derived-mode review-guide-test-terminal-mode fundamental-mode "Terminal")

(ert-deftest review-guide-send-finds-buffers-working-in-the-repository ()
  ;; The change under review is in a worktree nested in the repository,
  ;; at sub/.  Work in the main checkout above it counts; work in a
  ;; directory that merely contains it does not.
  (review-guide-test--with-repo
    (let ((worktree (expand-file-name "sub/" repo))
          (main (generate-new-buffer " *main checkout*"))
          (inside (generate-new-buffer " *inside worktree*"))
          (ancestor (generate-new-buffer " *ancestor*"))
          (unrelated (generate-new-buffer " *unrelated*")))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "deeper" worktree) t)
            (dolist (setup `((,main . ,repo)
                             (,inside . ,(expand-file-name "deeper/" worktree))
                             (,ancestor . ,(file-name-directory
                                            (directory-file-name repo)))
                             (,unrelated . ,(expand-file-name "~/"))))
              (with-current-buffer (car setup)
                (review-guide-test-terminal-mode)
                (setq default-directory (cdr setup))))
            (should (equal (sort (mapcar #'buffer-name
                                         (review-guide-send--buffers
                                          'review-guide-test-terminal-mode worktree))
                                 #'string<)
                           (list (buffer-name inside) (buffer-name main)))))
        (mapc #'kill-buffer (list main inside ancestor unrelated))))))

(ert-deftest review-guide-send-always-offers-the-kill-ring-last ()
  (let ((review-guide-send-target-functions
         (list (lambda (root) (list (cons (concat "Agent in " root) #'ignore))))))
    (should (equal (mapcar #'car (review-guide-send-targets "/repo/"))
                   '("Agent in /repo/" "Copy to the kill ring")))))

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

;;;; Review-loop fixups

(require 'review-guide-loop)

(defmacro review-guide-test--with-loop-repo (&rest body)
  "Run BODY in `review-guide-test--with-repo' after a review loop's work.
On top of the commit \"Shout two\", the loop fixed it in round 2 for
Qodo, made an ordinary commit adding new.txt, and fixed it again in
round 1 for Copilot.  BODY sees those fixups as `qodo' and `copilot'."
  (declare (indent 0))
  `(review-guide-test--with-repo
     (let ((pr-commit (review-guide-test--head repo)))
       (review-guide-test--git
        repo "commit" "--quiet" "-a" (concat "--fixup=" pr-commit)
        "--trailer" "Review-Thread: https://github.com/o/r/pull/1#discussion_r2"
        "--trailer" "Review-Bot: Qodo" "--trailer" "Review-Round: 2")
       (let ((qodo (review-guide-test--head repo)))
         (review-guide-test--git repo "add" "new.txt")
         (review-guide-test--git repo "commit" "--quiet" "-m" "Add new")
         (review-guide-test--write repo "a.txt" "ONE\nTWO\nTHREE\n")
         (review-guide-test--git
          repo "commit" "--quiet" "-a" (concat "--fixup=" pr-commit)
          "-m" "Copilot asked for it."
          "--trailer" "Review-Thread: https://github.com/o/r/pull/1#discussion_r1"
          "--trailer" "Review-Bot: Copilot" "--trailer" "Review-Round: 1")
         (let ((copilot (review-guide-test--head repo)))
           ,@body)))))

(defun review-guide-test--head (repo)
  (string-trim (review-guide-test--git repo "rev-parse" "HEAD")))

(defun review-guide-test--loop (repo)
  (review-guide-loop-create
   :directory repo :base-label "main" :head "work"
   :base (string-trim (review-guide-test--git repo "merge-base" "main" "work"))))

(ert-deftest review-guide-loop-chunks-each-fixup-in-round-order ()
  (review-guide-test--with-loop-repo
    (let* ((source (review-guide-test--loop repo))
           (chunks (review-guide-source-chunks
                    source (review-guide-diff-parse (review-guide-source-diff source)))))
      (should (equal (mapcar #'review-guide-chunk-title chunks)
                     '("Round 1, Copilot: Shout two" "Round 2, Qodo: Shout two")))
      (should (equal (mapcar #'review-guide-chunk-summary chunks)
                     '("Copilot asked for it.\n\nhttps://github.com/o/r/pull/1#discussion_r1"
                       "https://github.com/o/r/pull/1#discussion_r2")))
      (should (equal (mapcar (lambda (chunk)
                               (mapcar (lambda (hunk)
                                         (list (review-guide-hunk-commit hunk)
                                               (review-guide-hunk-path hunk)
                                               (review-guide-hunk-lines hunk)))
                                       (review-guide-chunk-hunks chunk)))
                             chunks)
                     `(((,copilot "a.txt" ("-one" "+ONE" " TWO" " THREE")))
                       ((,qodo "a.txt" (" one" " TWO" "-three" "+THREE")))))))))

(ert-deftest review-guide-loop-includes-fixups-for-local-findings ()
  (review-guide-test--with-loop-repo
    (review-guide-test--write repo "a.txt" "ONE\nTWO\nTHREE!\n")
    (review-guide-test--git
     repo "commit" "--quiet" "-a" (concat "--fixup=" pr-commit)
     "-m" "The reviewer found the shout uneven."
     "--trailer" "Review-Finding: 2.1")
    (let* ((source (review-guide-test--loop repo))
           (chunks (review-guide-source-chunks
                    source (review-guide-diff-parse (review-guide-source-diff source)))))
      (should (equal (mapcar #'review-guide-chunk-title chunks)
                     '("Round 1, Copilot: Shout two" "Round 2, Qodo: Shout two"
                       "Round 2, local review: Shout two")))
      (should (equal (review-guide-chunk-summary (car (last chunks)))
                     "The reviewer found the shout uneven.\n\nLocal review finding 2.1")))))

(ert-deftest review-guide-loop-visits-the-fixup-and-its-parent ()
  (review-guide-test--with-loop-repo
    (let ((source (review-guide-test--loop repo)))
      (pcase-dolist (`(,side ,line ,commit ,text)
                     `(("LEFT" 3 ,qodo "three") ("RIGHT" 3 ,qodo "THREE")
                       ("LEFT" 1 ,copilot "one") ("RIGHT" 1 ,copilot "ONE")))
        (save-window-excursion
          (review-guide-source-visit source "a.txt" side line commit)
          (unwind-protect
              (progn
                (should buffer-read-only)
                (should (equal (buffer-substring (point) (line-end-position)) text)))
            (kill-buffer)))))))

(ert-deftest review-guide-loop-defaults-to-the-branch-below-in-the-stack ()
  (review-guide-test--with-loop-repo
    (should (equal (review-guide-loop--default-base repo "work") "main"))
    (review-guide-test--git repo "switch" "--quiet" "-c" "top")
    (review-guide-test--git repo "commit" "--quiet" "--allow-empty" "-m" "On top")
    (should (equal (review-guide-loop--default-base repo "top") "work"))
    (let ((default-directory repo)
          (current-prefix-arg nil))
      (should (equal (review-guide-loop--read-args)
                     (list (directory-file-name repo) "top"))))))

(ert-deftest review-guide-loop-opens-without-the-model ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-loop-repo
    (let ((source (review-guide-test--loop repo)))
      (unwind-protect
          (cl-letf (((symbol-function 'review-guide-ai-group)
                     (lambda (&rest _) (error "The model was asked")))
                    ((symbol-function 'review-guide-ai-review)
                     (lambda (_title _desc hunks _buffer callback _errback)
                       (pcase-let ((`(,findings . ,_)
                                    (review-guide-ai-findings
                                     '((comments ((hunk . "H2") (side . "RIGHT")
                                                  (start_line . 3) (line . 3)
                                                  (severity . "nit")
                                                  (body . "Why shout?"))))
                                     hunks)))
                         (funcall callback "Fine." findings 0)))))
            (save-window-excursion
              (review-guide-open source)
              (let ((text (buffer-string)))
                (should (< (string-search "1. Round 1, Copilot: Shout two" text)
                           (string-search "+ONE" text)
                           (string-search "2. Round 2, Qodo: Shout two" text)
                           (string-search "+THREE" text)))
                (should-not (string-search "new.txt" text)))
              (review-guide-test--goto "-three")
              (save-window-excursion
                (review-guide-visit)
                (unwind-protect
                    (should (equal (buffer-substring (point) (line-end-position)) "three"))
                  (kill-buffer)))
              (review-guide-review)
              (review-guide-test--goto "AI nit on RIGHT 3")
              (let ((guide (current-buffer)))
                (save-window-excursion
                  (review-guide-forward)
                  (review-guide-comment-finish))
                (set-buffer guide))
              (should (equal (review-guide-source-threads source)
                             `(((path . "a.txt") (line . 3) (side . "RIGHT")
                                (commit . ,qodo) (body . "Why shout?")))))
              ;; The thread is shown under its own commit's line 3 only.
              (should (= (how-many "Pending comment on RIGHT 3" (point-min) (point-max))
                         1))
              (should (string-search
                       (concat "## a.txt, line 3 in commit " (substring qodo 0 12)
                               "\n\n```diff\n+THREE\n```\n\nWhy shout?\n")
                       (review-guide-local-feedback
                        "x" (review-guide-diff-parse (review-guide-source-diff source))
                        (review-guide-source-threads source))))))
        (when-let* ((guide (get-buffer
                            (format "*review-guide %s*"
                                    (car (review-guide-source-describe source))))))
          (kill-buffer guide))))))

(defmacro review-guide-test--with-loop-guide (&rest body)
  "Run BODY in a guide opened on the loop repo's review-loop source.
`source' is the source and `qodo' and `copilot' the fixups.  The model
is not asked for anything."
  (declare (indent 0))
  `(review-guide-test--with-loop-repo
     (let ((source (review-guide-test--loop repo)))
       (unwind-protect
           (cl-letf (((symbol-function 'review-guide-ai-group)
                      (lambda (&rest _) (error "The model was asked"))))
             (save-window-excursion
               (review-guide-open source)
               ,@body))
         (when-let* ((guide (get-buffer
                             (format "*review-guide %s*"
                                     (car (review-guide-source-describe source))))))
           (kill-buffer guide))))))

(ert-deftest review-guide-loop-comment-records-its-commit ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-loop-guide
    (review-guide-test--goto "+THREE")
    (let ((guide (current-buffer)))
      (save-window-excursion
        (review-guide-comment (point) (point))
        (insert "Mine.")
        (review-guide-comment-finish))
      (set-buffer guide))
    (should (equal (review-guide-source-threads source)
                   `(((path . "a.txt") (line . 3) (side . "RIGHT")
                      (commit . ,qodo) (body . "Mine.")))))))

(ert-deftest review-guide-loop-comment-refuses-a-range-across-commits ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-loop-guide
    (review-guide-test--goto "+ONE")
    (let ((beg (point)))
      (review-guide-test--goto "+THREE")
      (should-error (review-guide-comment beg (point)) :type 'user-error))
    (should-not (review-guide-source-threads source))))

(ert-deftest review-guide-loop-reopened-with-another-base-shows-that-base ()
  (skip-unless (require 'magit-section nil t))
  (require 'review-guide)
  (review-guide-test--with-loop-repo
    (let ((wide (review-guide-test--loop repo))
          (narrow (review-guide-loop-create
                   :directory repo :base-label "qodo" :base qodo :head "work")))
      (unwind-protect
          (cl-letf (((symbol-function 'review-guide-ai-group)
                     (lambda (&rest _) (error "The model was asked"))))
            (save-window-excursion
              (review-guide-open wide)
              (should (string-search "Qodo: Shout two" (buffer-string)))
              (review-guide-open narrow)
              (let ((text (buffer-string)))
                (should (string-search "Copilot: Shout two" text))
                (should-not (string-search "Qodo: Shout two" text)))))
        (dolist (source (list wide narrow))
          (when-let* ((guide (get-buffer
                              (format "*review-guide %s*"
                                      (car (review-guide-source-describe source))))))
            (kill-buffer guide)))))))

(defconst review-guide-test--lisp
  (expand-file-name "../lisp/review-guide"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory holding the package's sources.")

(ert-deftest review-guide-loop-autoload-reaches-the-entry-point ()
  "A fresh Emacs with only the generated autoloads can run the command."
  ;; The sources are copied next to the generated loaddefs.el: Emacs 29
  ;; records each autoload's file relative to the loaddefs file, and
  ;; resolves it against `load-path' rather than that file's directory.
  (let* ((scratch (make-temp-file "review-guide-autoloads" t))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (with-temp-buffer
          (dolist (source (directory-files review-guide-test--lisp t "\\.el\\'"))
            (copy-file source (file-name-as-directory scratch)))
          (let ((status (call-process
                         emacs nil t nil "--batch" "-Q"
                         "--eval" (format "(add-to-list 'load-path %S)" scratch)
                         "--eval" (format "(loaddefs-generate %S %S)"
                                          scratch (expand-file-name "loaddefs.el" scratch))
                         "--eval" (format "(load %S nil t)"
                                          (expand-file-name "loaddefs.el" scratch))
                         "--eval" "(unless (autoloadp (symbol-function 'review-guide-loop)) (error \"review-guide-loop is not autoloaded\"))"
                         "--eval" "(autoload-do-load (symbol-function 'review-guide-loop) 'review-guide-loop)"
                         "--eval" "(unless (fboundp 'review-guide-open) (error \"void-function review-guide-open\"))")))
            (ert-info ((buffer-string))
              (should (zerop status)))))
      (delete-directory scratch t))))

;;; review-guide.el ends here
