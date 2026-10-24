;;; review-guide.el --- review-guide tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defconst review-guide-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/review-guide" review-guide-test--directory))
(require 'review-guide-diff)
;; The lock that holds review records, as default.el turns it on.
(load (expand-file-name "../lisp/wgn-agent-plans.el" review-guide-test--directory) nil t)

;;;; Diffs

(defconst review-guide-test--small-diff "\
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

(ert-deftest review-guide-diff-parses-hunks-across-files ()
  (let ((hunks (review-guide-diff-parse review-guide-test--small-diff)))
    (should (equal (mapcar #'review-guide-hunk-id hunks) '("H1" "H2" "H3" "H4")))
    (should (equal (mapcar #'review-guide-hunk-path hunks)
                   '("lib/a.el" "lib/a.el" "lib/new.el" "old.el")))
    (should (equal (mapcar #'review-guide-hunk-old-count hunks) '(4 2 0 1)))))

(ert-deftest review-guide-diff-numbers-lines-by-side ()
  (let ((hunks (review-guide-diff-parse review-guide-test--small-diff)))
    (should (equal (review-guide-hunk-coordinates (car hunks))
                   ;; The blank line lost its leading space and is context.
                   '((" one" "RIGHT" 1) ("-two" "LEFT" 2) ("+TWO" "RIGHT" 2)
                     ("+three" "RIGHT" 3) (" " "RIGHT" 4) (" four" "RIGHT" 5))))
    (should (equal (review-guide-hunk-coordinates (nth 2 hunks))
                   '(("+first" "RIGHT" 1) ("+second" "RIGHT" 2))))))

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

;;;; Records
;;
;; tests/fixtures/review-record.org and pr-12.org are byte for byte the
;; golden files of agent-tools' internal/record, which its renderer is
;; held to: testdata/range.golden.org and pr.golden.org.

(defconst review-guide-test--base "9f8e7d6c5b4a39281706f5e4d3c2b1a098765432")
(defconst review-guide-test--head "1a2b3c4d5e6f708192a3b4c5d6e7f80912345678")

(defun review-guide-test--fixture (name)
  "Return the text of the fixture NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "fixtures/" name)
                                            review-guide-test--directory))
    (buffer-string)))

(defun review-guide-test--diff ()
  "Return a diff holding the hunks the fixture's walkthrough names, and one more."
  (let ((lines (lambda (prefix word from to)
                 (mapconcat (lambda (n) (format "%s%s %d\n" prefix word n))
                            (number-sequence from to) ""))))
    (concat
     "diff --git a/internal/x/cache.go b/internal/x/cache.go\nnew file mode 100644\n"
     "--- /dev/null\n+++ b/internal/x/cache.go\n@@ -0,0 +1,40 @@\n"
     (funcall lines "+" "cache" 1 40)
     "diff --git a/internal/x/cache_test.go b/internal/x/cache_test.go\nnew file mode 100644\n"
     "--- /dev/null\n+++ b/internal/x/cache_test.go\n@@ -0,0 +1,25 @@\n"
     (funcall lines "+" "test" 1 25)
     "diff --git a/cmd/x/main.go b/cmd/x/main.go\n--- a/cmd/x/main.go\n+++ b/cmd/x/main.go\n"
     "@@ -38,6 +40,19 @@ func main() {\n"
     (funcall lines "-" "old" 38 43)
     (funcall lines "+" "new" 40 58)
     "diff --git a/README.md b/README.md\n--- a/README.md\n+++ b/README.md\n"
     "@@ -1 +1 @@\n-Old.\n+New.\n")))

(defun review-guide-test--write (file contents)
  "Write CONTENTS to FILE, making its directory, as git delegate would.
It is copied into place by cp, since the lock refuses Emacs's own writes
into the records directory."
  (make-directory (file-name-directory file) t)
  (let ((temp (make-temp-file "review-guide-write-")))
    (unwind-protect
        (progn
          (with-temp-file temp (insert contents))
          (unless (eq 0 (call-process "cp" nil nil nil temp file))
            (error "cp %s %s failed" temp file)))
      (delete-file temp))))

(defmacro review-guide-test--with-records (&rest body)
  "Run BODY with the records directory under a temp dir, holding the fixtures.
Binds `root', `records' (owner/repo's directory of records), `range-file'
and `pr-file'.  No git delegate or forge is asked anything."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "review-guide-" t)))
          (process-environment
           (append (list (concat "XDG_STATE_HOME=" root "state")
                         (concat "XDG_CACHE_HOME=" root "cache"))
                   process-environment))
          ;; Not the operator's, whatever the environment says.
          (wgn/agent-plans-config-file (concat root "config.json"))
          (review-guide-records-directory nil)
          (review-guide-cache-directory nil)
          (records (expand-file-name "state/agent-tools/reviews/owner/repo/" root))
          (range-file (expand-file-name (concat review-guide-test--base ".."
                                                review-guide-test--head ".org")
                                        records))
          (pr-file (expand-file-name "pr-12.org" records)))
     (unwind-protect
         (progn
           (review-guide-test--write range-file (review-guide-test--fixture "review-record.org"))
           (review-guide-test--write pr-file (review-guide-test--fixture "pr-12.org"))
           (cl-letf (((symbol-function 'review-guide--forge-head) #'ignore)
                     ((symbol-function 'review-guide--delegate-show) #'ignore))
             ,@body))
       (dolist (buffer (buffer-list))
         (when (or (string-prefix-p "*review-guide " (buffer-name buffer))
                   (and (buffer-file-name buffer)
                        (string-prefix-p root (buffer-file-name buffer))))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory root t))))

(defmacro review-guide-test--in-view (target &rest body)
  "Run BODY in the view of TARGET over the fixtures and `review-guide-test--diff'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'review-guide--diff)
              (lambda (&rest _) (review-guide-diff-parse (review-guide-test--diff)))))
     (save-window-excursion
       (review-guide ,target)
       ,@body)))

(defun review-guide-test--need-view ()
  "Skip unless magit-section is there, then load review-guide."
  (unless (require 'magit-section nil t)
    (ert-skip "magit-section is not installed"))
  (require 'review-guide))

(ert-deftest review-guide-reads-the-range-record ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((range (review-guide-read-range range-file)))
      (should (equal (list (review-guide-range-repo range) (review-guide-range-base range)
                           (review-guide-range-head range) (review-guide-range-pr range)
                           (review-guide-range-title range) (review-guide-range-updated range))
                     (list "owner/repo" review-guide-test--base review-guide-test--head 12
                           "Cache the parsed config" "[2026-10-20 Tue 14:05]")))
      (should (equal (mapcar (lambda (chunk)
                               (list (review-guide-chunk-id chunk) (review-guide-chunk-title chunk)
                                     (review-guide-chunk-hunks chunk)))
                             (review-guide-range-chunks range))
                     '(("C1" "The cache and its key"
                        ((:path "internal/x/cache.go" :new-start 1 :new-lines 40
                                :old-start 0 :old-lines 0)
                         (:path "internal/x/cache_test.go" :new-start 1 :new-lines 25
                                :old-start 0 :old-lines 0)))
                       ("C2" "Callers read through the cache"
                        ((:path "cmd/x/main.go" :new-start 40 :new-lines 19
                                :old-start 38 :old-lines 6))))))
      (should (equal (review-guide-chunk-summary (car (review-guide-range-chunks range)))
                     "Adds a cache keyed by the config file's path and mtime.\n\nA miss parses the file as before."))
      (let ((diagram (car (review-guide-range-diagrams range))))
        (should (equal (list (review-guide-diagram-id diagram) (review-guide-diagram-title diagram)
                             (review-guide-diagram-drawn-by diagram)
                             (review-guide-diagram-critiqued-by diagram)
                             (review-guide-diagram-verdict diagram))
                       '("D1" "Load with the cache" "claude" "codex" "holds")))
        (should (string-prefix-p "{\"kind\": \"sequence\"" (review-guide-diagram-json diagram))))
      (should (equal (mapcar (lambda (finding)
                               (list (review-guide-finding-state finding)
                                     (review-guide-finding-id finding)
                                     (review-guide-finding-path finding)
                                     (review-guide-finding-line finding)
                                     (review-guide-finding-end-line finding)
                                     (review-guide-finding-side finding)
                                     (review-guide-finding-outcome-target finding)))
                             (review-guide-range-findings range))
                     '(("OPEN" "F1" "internal/x/cache.go" 88 94 "new" nil)
                       ("FIX" "F2" "internal/x/cache_test.go" 12 nil "new" nil)
                       ("DEFER" "F3" nil nil nil nil nil)
                       ("OPERATOR" "F4" "cmd/x/main.go" 38 43 "old" nil)
                       ("DUPLICATE" "F5" "internal/x/cache.go" 88 nil "new" "F1"))))
      (let ((f1 (car (review-guide-range-findings range)))
            (f2 (nth 1 (review-guide-range-findings range))))
        (should (equal (review-guide-finding-evidence f1)
                       "Two writes within one mtime tick keep the old entry:\n\n    key := path + mtime.String()"))
        (should-not (review-guide-finding-reason f1))
        (should (equal (review-guide-finding-reason f2)
                       "Added a test that changes the file between loads.")))
      (should (equal (mapcar (lambda (round)
                               (list (review-guide-round-id round) (review-guide-round-reviewer round)
                                     (review-guide-round-model round) (review-guide-round-red-check round)
                                     (review-guide-round-unverified round)
                                     (review-guide-round-dropped round)))
                             (review-guide-range-rounds range))
                     '(("R1" "codex" "gpt-5.5" "session"
                        ("Whether mtime has a second's resolution on the CI runner.")
                        ("A finding at internal/x/gone.go:3, which is not in the tip."))
                       ("R2" "security" "claude-opus-5-5" "absent" nil nil)))))))

(ert-deftest review-guide-reads-the-pr-record ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((pr (review-guide-read-pr pr-file)))
      (should (equal (list (review-guide-pr-repo pr) (review-guide-pr-number pr))
                     '("owner/repo" 12)))
      (should (equal (mapcar (lambda (thread)
                               (list (review-guide-thread-id thread)
                                     (review-guide-thread-authors thread)
                                     (review-guide-thread-resolved thread)
                                     (review-guide-thread-path thread)
                                     (review-guide-thread-line thread)
                                     (length (review-guide-thread-comments thread))
                                     (review-guide-thread-draft thread)))
                             (review-guide-pr-threads pr))
                     '(("PRRT_kwDOAbc001" ("copilot-pull-request-reviewer:Bot") nil
                        "internal/x/cache.go" 88 1 nil)
                       ("PRRT_kwDOAbc002" ("qodo-code-review:Bot" "wesnel:User") t
                        "internal/x/cache_test.go" 12 2 nil)
                       ("PRRT_kwDOAbc003" ("alice:User") nil "cmd/x/main.go" 40 1
                        (:outcome "rebut" :posted nil
                                  :body "Nothing calls it: `git grep LoadOld` finds only its definition."))
                       ("PRRT_kwDOAbc004" ("bob:User") nil "README.md" nil 1
                        (:outcome "fix" :posted "[2026-10-20 Tue 15:05]"
                                  :body "Done in \"Describe the cache\".")))))
      (should (equal (car (review-guide-thread-comments (nth 2 (review-guide-pr-threads pr))))
                     "Why not keep the old loader?\n\n```go\nLoadOld()\n```")))))

(ert-deftest review-guide-counts-are-computed-from-the-sections ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (should (equal (review-guide-counts (review-guide-read-range range-file)
                                        (review-guide-read-pr pr-file))
                   '(:open 1 :deferred 1 :operator 1 :unanswered 1 :drafted 1)))))

(ert-deftest review-guide-refuses-what-is-no-record ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((text (review-guide-test--fixture "review-record.org")))
      ;; Org would read the setup file as soon as the buffer is in Org mode.
      (review-guide-test--write range-file
                                (replace-regexp-in-string "^\\*\\* Diagrams$"
                                                          "#+SETUPFILE: /tmp/x.org\n** Diagrams"
                                                          text t t))
      (should (string-search ":33 is not a line of a review record"
                             (cadr (should-error (review-guide-read-range range-file)
                                                 :type 'user-error))))
      (review-guide-test--write range-file
                                (replace-regexp-in-string ":VERSION: 2" ":VERSION: 3" text t t))
      (should-error (review-guide-read-range range-file) :type 'user-error)
      (review-guide-test--write range-file (concat text "#+begin_example\nopen\n"))
      (should-error (review-guide-read-range range-file) :type 'user-error))))

(ert-deftest review-guide-refuses-a-record-naming-another-range ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (review-guide-test--write range-file
                              (replace-regexp-in-string ":REPO: owner/repo" ":REPO: owner/other"
                                                        (review-guide-test--fixture "review-record.org")
                                                        t t))
    (should (string-search "not what its path does"
                           (cadr (should-error (review-guide "owner/repo#12") :type 'user-error))))))

(ert-deftest review-guide-anchors-findings-at-their-lines ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let* ((findings (review-guide-range-findings (review-guide-read-range range-file)))
           (anchors (review-guide-anchor findings (review-guide-diff-parse (review-guide-test--diff)))))
      ;; F1 and F5 lie past the new file's 40 lines, and F3 has no line.
      (should (equal (mapcar (lambda (anchor)
                               (cons (review-guide-finding-id (car anchor)) (cdr anchor)))
                             anchors)
                     '(("F2" "H2" "RIGHT" 12) ("F4" "H3" "LEFT" 43)))))))

(defun review-guide-test--order (text &rest strings)
  "Return non-nil when STRINGS occur in TEXT in that order."
  (let ((at 0))
    (cl-every (lambda (string)
                (let ((found (string-search string text at)))
                  (and found (setq at (1+ found)))))
              strings)))

(ert-deftest review-guide-view-shows-the-walkthrough-findings-and-threads ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (review-guide-test--in-view "https://github.com/owner/repo/pull/12/files"
      (let ((text (buffer-string)))
        (should (equal (buffer-name) "*review-guide owner/repo 9f8e7d6c5b4a..1a2b3c4d5e6f*"))
        (should (string-prefix-p
                 "owner/repo 9f8e7d6c5b4a..1a2b3c4d5e6f #12  Cache the parsed config\nFindings: 1 open, 1 deferred, 1 for the operator; Threads: 1 unanswered, 1 drafts unposted\n"
                 text))
        (should (review-guide-test--order
                 text
                 "1. The cache and its key" "A miss parses the file as before."
                 "internal/x/cache.go  @@ -0,0 +1,40 @@"
                 "internal/x/cache_test.go  @@ -0,0 +1,25 @@" "+test 12\n"
                 "    FIX F2 test  No test of a miss" "Added a test that changes" "+test 13\n"
                 "2. Callers read through the cache"
                 "cmd/x/main.go  @@ -38,6 +40,19 @@ func main() {" "-old 43\n"
                 "    OPERATOR F4 question  Drop the old loader?" "+new 40\n"
                 "3. Not in the walkthrough" "README.md  @@ -1 +1 @@"
                 "Diagrams" "D1  Load with the cache" "drawn by claude, critiqued by codex: holds"
                 "Findings outside the diff here"
                 "  OPEN F1 bug on internal/x/cache.go:88-94 (new)  The key ignores the file's size"
                 "    key := path + mtime.String()"
                 "  DEFER F3 design  Bound the cache"
                 "  DUPLICATE F5 bug on internal/x/cache.go:88 (new)  Same as F1"
                 "Outcome, as F1:"
                 "Threads of #12"
                 "PRRT_kwDOAbc003  cmd/x/main.go:40  unresolved" "alice:User:" "```go"
                 "Draft, rebut, not posted"
                 "PRRT_kwDOAbc004  README.md  unresolved" "Draft, fix, posted [2026-10-20 Tue 15:05]"
                 "Rounds" "R1  codex (gpt-5.5), [2026-10-20 Tue 14:05], red-check session"
                 "Unverified:" "Dropped:" "R2  security (claude-opus-5-5)"))
        ;; A diagram with no Mermaid from git delegate show is shown as its JSON.
        (should (string-search "{\"kind\": \"sequence\"" text))
        (should buffer-read-only)))))

(defvar review-guide-test--evaluated nil)

(ert-deftest review-guide-view-shows-text-as-the-file-holds-it ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (setq review-guide-test--evaluated nil)
    (review-guide-test--write
     range-file
     (replace-regexp-in-string
      "^Every case hits the cache\\.$"
      (concat "[[elisp:(setq review-guide-test--evaluated t)][click]] %s %S\n"
              ",#+begin_src emacs-lisp\n(delete-file \"x\")\n,#+end_src\n,* not a heading")
      (review-guide-test--fixture "review-record.org") t t))
    (review-guide-test--in-view "owner/repo#12"
      (let ((text (buffer-string)))
        (should (string-search
                 "[[elisp:(setq review-guide-test--evaluated t)][click]] %s %S\n" text))
        (should (string-search "#+begin_src emacs-lisp\n" text))
        (should (string-search "* not a heading\n" text))
        (should-not (string-search ",#+" text))
        (let ((at (string-search "[[elisp:" text)))
          (dolist (property '(button keymap local-map mouse-face help-echo display invisible))
            (should-not (get-text-property (1+ at) property))))
        (should-not review-guide-test--evaluated)))))

(ert-deftest review-guide-view-renders-or-shows-mermaid ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (cl-letf (((symbol-function 'review-guide--delegate-show)
               (lambda (source)
                 (should (equal (plist-get source :show) "owner/repo#12"))
                 '((range (diagrams ((id . "D1") (mermaid . "sequenceDiagram\n  main->>cache: Load"))))))))
      (review-guide-test--in-view "owner/repo#12"
        ;; No renderer here, so the Mermaid is shown as its source.
        (should (string-search "    sequenceDiagram\n      main->>cache: Load\n" (buffer-string)))
        (should-not (string-search "its JSON" (buffer-string)))))))

(ert-deftest review-guide-opens-pr-review-and-a-terminal-for-post ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((checkout (file-name-as-directory (expand-file-name "checkout" root)))
          opened term)
      (make-directory checkout)
      (cl-letf (((symbol-function 'review-guide--checkout) (lambda (&rest _) checkout))
                ((symbol-function 'pr-review) (lambda (url &rest _) (setq opened url)))
                ((symbol-function 'make-term)
                 (lambda (name program startfile &rest switches)
                   (setq term (list name program startfile switches default-directory))
                   (get-buffer-create " *review-guide-test term*")))
                ((symbol-function 'term-char-mode) #'ignore))
        (unwind-protect
            (review-guide-test--in-view "owner/repo#12"
              (review-guide-pr-review)
              (should (equal opened "https://github.com/owner/repo/pull/12"))
              (with-current-buffer "*review-guide owner/repo 9f8e7d6c5b4a..1a2b3c4d5e6f*"
                (review-guide-post))
              (should (equal term (list "pr-loop post owner/repo#12" "pr-loop" nil
                                        '("post" "12") checkout))))
          (kill-buffer " *review-guide-test term*"))))))

(ert-deftest review-guide-reads-a-range-and-a-branch-in-their-checkout ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let* ((repo (file-name-as-directory (expand-file-name "proj" root)))
           (git (lambda (&rest args)
                  (with-temp-buffer
                    (unless (eq 0 (apply #'call-process "git" nil t nil
                                         "-c" "commit.gpgsign=false" "-c" "tag.gpgsign=false"
                                         "-c" "user.name=Test" "-c" "user.email=test@example.com"
                                         args))
                      (error "git %S: %s" args (buffer-string)))
                    (string-trim (buffer-string))))))
      (make-directory repo)
      (let ((default-directory repo))
        (funcall git "init" "-q")
        (funcall git "remote" "add" "origin" "git@github.com:own/proj.git")
        (review-guide-test--write (expand-file-name "a.txt" repo) "one\n")
        (funcall git "add" "a.txt")
        (funcall git "commit" "-q" "-m" "One")
        (review-guide-test--write (expand-file-name "a.txt" repo) "one\ntwo\n")
        (funcall git "commit" "-q" "-am" "Two")
        (let* ((base (funcall git "rev-parse" "HEAD~1"))
               (head (funcall git "rev-parse" "HEAD"))
               (file (expand-file-name (format "state/agent-tools/reviews/own/proj/%s..%s.org"
                                               base head)
                                       root)))
          (review-guide-test--write
           file
           (replace-regexp-in-string
            "^:PR: 12\n" ""
            (replace-regexp-in-string
             review-guide-test--head head
             (replace-regexp-in-string
              review-guide-test--base base
              (replace-regexp-in-string ":REPO: owner/repo" ":REPO: own/proj"
                                        (review-guide-test--fixture "review-record.org") t t)
              t t)
             t t)
            t t))
          (dolist (target (list (concat base ".." head) "HEAD"))
            (save-window-excursion
              (review-guide target)
              (let ((text (buffer-string)))
                (should (string-prefix-p (format "own/proj %s..%s  Cache the parsed config\n"
                                                 (substring base 0 12) (substring head 0 12))
                                         text))
                ;; The record's hunks are not in this diff; its own hunk is.
                (should (string-search "which the diff here does not hold" text))
                (should (review-guide-test--order text "3. Not in the walkthrough"
                                                  "a.txt  @@ -1 +1,2 @@" "+two\n"))
                (should-not (string-search "Threads of" text))
                (should (equal (file-truename default-directory) (file-truename repo))))))
          (should-error (review-guide (concat head ".." base)) :type 'user-error)
          (should-error (review-guide "-x") :type 'user-error))))))

;;;; Visiting a record

(defconst review-guide-test--local-variables "\
#+title: review
-*- mode: emacs-lisp; review-guide-test--prop-line: 1 -*-
* Review
#+begin_example
Local Variables:
review-guide-test--local: 1
eval: (setq-local review-guide-test--evaled t)
End:
#+end_example
"
  "A record whose free text holds file variables, unneutralised.")

(defvar review-guide-test--prop-line nil)
(defvar review-guide-test--local nil)
(defvar review-guide-test--evaled nil)

(defun review-guide-test--file-variables (file &optional first-line)
  "Visit FILE with every file variable allowed, and return those it set.
They follow the major mode the visit chose.  With FIRST-LINE, the -*-
line is moved to line 1, where Emacs reads it."
  (when first-line
    (review-guide-test--write
     file (replace-regexp-in-string "\\`#\\+title: review\n\\(.*\\)\n" "\\1\n#+title: review\n"
                                    review-guide-test--local-variables t)))
  (let* ((enable-local-variables :all)
         (enable-local-eval t)
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (cons major-mode
                (seq-filter (lambda (variable) (local-variable-p variable))
                            '(review-guide-test--prop-line review-guide-test--local
                                                           review-guide-test--evaled))))
      (kill-buffer buffer))))

(defmacro review-guide-test--protected (&rest body)
  "Run BODY with `wgn/agent-plans-protect-mode' on, and off again after."
  (declare (indent 0))
  `(let ((org-blocker-hook nil)
         (find-file-hook nil)
         (org-mode-hook nil)
         (write-file-functions nil)
         (after-revert-hook nil))
     (cl-progv (and (boundp 'revert-buffer-restore-functions)
                    '(revert-buffer-restore-functions))
         (and (boundp 'revert-buffer-restore-functions)
              (list revert-buffer-restore-functions))
       (unwind-protect
           (progn (wgn/agent-plans-protect-mode 1) ,@body)
         (wgn/agent-plans-protect-mode -1)))))

(ert-deftest review-guide-record-sets-no-file-variables ()
  (review-guide-test--need-view)
  (require 'org)
  (review-guide-test--with-records
    (let ((record (expand-file-name "local.org" records))
          (elsewhere (expand-file-name "elsewhere/local.org" root)))
      (dolist (file (list record elsewhere))
        (review-guide-test--write file review-guide-test--local-variables))
      ;; The test bites: outside the records, Emacs sets them.
      (should (equal (review-guide-test--file-variables elsewhere)
                     '(org-mode review-guide-test--local review-guide-test--evaled)))
      (should (equal (review-guide-test--file-variables elsewhere t)
                     '(emacs-lisp-mode review-guide-test--prop-line review-guide-test--local
                                       review-guide-test--evaled)))
      (review-guide-test--protected
        (should (equal (review-guide-test--file-variables record) '(org-mode)))
        (review-guide-test--write record review-guide-test--local-variables)
        (should (equal (review-guide-test--file-variables record t) '(org-mode)))
        ;; A backup holds the same text.
        (review-guide-test--write (concat record "~") review-guide-test--local-variables)
        (should (equal (review-guide-test--file-variables (concat record "~")) '(org-mode)))
        (should (equal (review-guide-test--file-variables (concat record "~") t) '(org-mode)))
        ;; review-guide's own visit sets none either, the lock off or on.
        (dolist (protect '(t nil))
          (wgn/agent-plans-protect-mode (if protect 1 -1))
          (save-window-excursion
            (with-current-buffer (review-guide-test--visit (list :range-file record
                                                                 :directory root))
              (should (equal buffer-file-name record))
              (should (eq major-mode 'org-mode))
              (should buffer-read-only)
              (should (equal default-directory root))
              (should-not (or (local-variable-p 'review-guide-test--prop-line)
                              (local-variable-p 'review-guide-test--local)
                              (local-variable-p 'review-guide-test--evaled)))
              (kill-buffer))))))))

(defun review-guide-test--visit (source)
  "Visit the record of SOURCE as the view's o does, every file variable allowed.
Return the record's buffer."
  (with-temp-buffer
    (setq-local review-guide--source source)
    (let ((enable-local-variables :all)
          (enable-local-eval t))
      (review-guide-visit-record))
    (current-buffer)))

(ert-deftest review-guide-record-buffer-refuses-edits ()
  (review-guide-test--need-view)
  (require 'org)
  (review-guide-test--with-records
    (review-guide-test--protected
      (save-window-excursion
        (with-current-buffer (review-guide-test--visit (list :range-file range-file))
          (should (equal buffer-file-name range-file))
          (should (string-search "git delegate and pr-loop" header-line-format))
          (let ((refused (should-error (let ((inhibit-read-only t)) (insert "edited\n"))
                                       :type 'user-error)))
            (should (string-search "git delegate and pr-loop write" (cadr refused))))
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\* OPEN F1$")
          (ignore-errors
            (let ((inhibit-read-only t)
                  (org-inhibit-logging t))
              (org-todo "FIX")))
          (should (equal (org-get-todo-state) "OPEN"))
          (should-not (buffer-modified-p))
          (with-temp-buffer
            (should-error (write-region "x" nil range-file) :type 'user-error)
            (should-error (write-region "x" nil (expand-file-name "new.org" records))
                          :type 'user-error))
          (should (equal (with-temp-buffer
                           (insert-file-contents range-file)
                           (buffer-string))
                         (review-guide-test--fixture "review-record.org")))
          ;; Emacs's own files beside a record, as a save's backup and an
          ;; auto-save make, are not records.
          (copy-file range-file (concat range-file "~"))
          (with-temp-buffer
            (write-region "x" nil (expand-file-name (concat "#" (file-name-nondirectory range-file) "#")
                                                    records)))
          ;; C-x C-q is the operator's deliberate edit.
          (read-only-mode -1)
          (goto-char (point-max))
          (insert "edited\n")
          (save-buffer)
          (should (string-suffix-p "\nedited\n" (with-temp-buffer
                                                   (insert-file-contents range-file)
                                                   (buffer-string)))))))))

(ert-deftest review-guide-customised-records-directory-is-held-too ()
  (review-guide-test--need-view)
  (require 'org)
  (review-guide-test--with-records
    (let* ((review-guide-records-directory (expand-file-name "custom/" root))
           (record (expand-file-name "owner/repo/local.org" review-guide-records-directory)))
      (review-guide-test--write record review-guide-test--local-variables)
      (review-guide-test--protected
        (should (equal (review-guide-test--file-variables record t) '(org-mode)))
        (save-window-excursion
          (with-current-buffer (review-guide-test--visit (list :range-file record))
            (should (string-search "git delegate and pr-loop" header-line-format))
            (should-error (let ((inhibit-read-only t)) (insert "edited\n")) :type 'user-error)
            (kill-buffer)))
        (with-temp-buffer
          (should-error (write-region "x" nil (expand-file-name "new.org" (file-name-directory record)))
                        :type 'user-error))
        ;; Where git delegate writes is held still.
        (with-temp-buffer
          (should-error (write-region "x" nil range-file) :type 'user-error))))))

;;;; Paths no link holds, and text that looks like structure
;;
;; tests/fixtures/odd-paths.org and odd-paths-pr.org were rendered by
;; agent-tools' internal/record, and parse back there to the values
;; they were rendered from.

(ert-deftest review-guide-reads-paths-no-link-holds ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((odd (expand-file-name "odd-paths.org" root))
          (odd-pr (expand-file-name "odd-paths-pr.org" root)))
      (review-guide-test--write odd (review-guide-test--fixture "odd-paths.org"))
      (review-guide-test--write odd-pr (review-guide-test--fixture "odd-paths-pr.org"))
      (let ((range (review-guide-read-range odd)))
        ;; The summary's own hunk-like line is text, not a hunk.
        (should (equal (review-guide-chunk-hunks (car (review-guide-range-chunks range)))
                       '((:path "app/[id]/page.tsx" :new-start 1 :new-lines 3
                                :old-start 0 :old-lines 0)
                         (:path "docs/My File (1) [draft].md" :new-start 5 :new-lines 2
                                :old-start 5 :old-lines 1)
                         (:path "naïve.txt" :new-start 1 :new-lines 1
                                :old-start 0 :old-lines 0)
                         (:path "50%.txt" :new-start 0 :new-lines 0
                                :old-start 1 :old-lines 2)
                         (:path "internal/x/cache.go" :new-start 1 :new-lines 40
                                :old-start 0 :old-lines 0))))
        (should (equal (mapcar #'review-guide-finding-path (review-guide-range-findings range))
                       '("app/[id]/page.tsx" "docs/My File (1) [draft].md" "evil‮txt.go"))))
      (should (equal (mapcar #'review-guide-thread-path
                             (review-guide-pr-threads (review-guide-read-pr odd-pr)))
                     '("app/[id]/page.tsx" "naïve.txt"))))))

(ert-deftest review-guide-refuses-a-path-its-writer-does-not-write ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    ;; Each decodes, but internal/record writes none of them so: it
    ;; writes A as itself, a valid path unescaped, and hex in capitals.
    (dolist (text '("app/%41.tsx" "a%20b" "app/%5bid%5D" "50%.txt" "a%2" "x y%25"))
      (review-guide-test--write
       range-file (replace-regexp-in-string "^:PATH: internal/x/cache_test.go$"
                                            (concat ":PATH: " text)
                                            (review-guide-test--fixture "review-record.org") t t))
      (should (string-search (concat text " is not a path as the record")
                             (cadr (should-error (review-guide-read-range range-file)
                                                 :type 'user-error)))))))

(ert-deftest review-guide-view-shows-and-anchors-paths-no-link-holds ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((odd (expand-file-name "odd-paths.org" root))
          (odd-pr (expand-file-name "odd-paths-pr.org" root))
          (diff (concat
                 "diff --git a/app/[id]/page.tsx b/app/[id]/page.tsx\nnew file mode 100644\n"
                 "--- /dev/null\n+++ b/app/[id]/page.tsx\n@@ -0,0 +1,3 @@\n+one\n+two\n+three\n"
                 "diff --git a/evil‮txt.go b/evil‮txt.go\nnew file mode 100644\n"
                 "--- /dev/null\n+++ b/evil‮txt.go\n@@ -0,0 +1 @@\n+x\n")))
      (review-guide-test--write odd (review-guide-test--fixture "odd-paths.org"))
      (review-guide-test--write odd-pr (review-guide-test--fixture "odd-paths-pr.org"))
      (with-temp-buffer
        (review-guide-mode)
        (setq review-guide--source (list :owner "owner" :repo "repo" :directory root)
              review-guide--range (review-guide-read-range odd)
              review-guide--pr (review-guide-read-pr odd-pr))
        (cl-letf (((symbol-function 'review-guide--diff)
                   (lambda (&rest _) (review-guide-diff-parse diff))))
          (review-guide--render))
        (let ((text (buffer-string)))
          (should (review-guide-test--order
                   text
                   "1. Paths no link holds"
                   "app/[id]/page.tsx  @@ -0,0 +1,3 @@" "+two\n"
                   "    OPEN F1 bug  Bracketed path" "+three\n"
                   "docs/My File (1) [draft].md  @@ -5,1 +5,2 @@, which the diff here does not hold"
                   "naïve.txt  @@ -0,0 +1,1 @@, which"
                   "50%.txt  @@ -1,2 +0,0 @@, which"
                   "2. Not in the walkthrough"
                   "evil%E2%80%AEtxt.go  @@ -0,0 +1 @@"
                   "    OPEN F3 bug  An overriding path"
                   "Findings outside the diff here"
                   "  OPEN F2 bug on docs/My File (1) [draft].md:6 (new)"
                   "Threads of #12"
                   "PRRT_kwDOAbc001  app/[id]/page.tsx:2  unresolved"
                   "PRRT_kwDOAbc002  naïve.txt:12  resolved"))
          ;; A bidirectional control is shown as the record writes it.
          (should-not (string-search "‮" text)))))))

(ert-deftest review-guide-header-is-the-top-headings-drawer ()
  (review-guide-test--need-view)
  (review-guide-test--with-records
    (let ((odd (expand-file-name "odd-paths.org" root)))
      (review-guide-test--write odd (review-guide-test--fixture "odd-paths.org"))
      ;; Its walkthrough's summary holds a line ":PR: 99".
      (should-not (review-guide--header-value odd "PR"))
      (should (equal (review-guide--header-value odd "UPDATED") "[2026-10-20 Tue 14:05]"))
      (should (equal (review-guide--header-value range-file "PR") "12"))
      (should (equal (review-guide--header-value pr-file "PR") "12")))))

;;; review-guide.el ends here
