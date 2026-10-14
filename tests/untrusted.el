;;; untrusted.el --- Untrusted directory tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'eglot)
(load (expand-file-name "../lisp/wgn-untrusted.el"
                        (file-name-directory load-file-name)) nil t)
;; Keep the bindings below dynamic when this file is compiled.
(defvar wgn/untrusted-directories)
(declare-function wgn/untrusted-dir-p "wgn-untrusted" (file))

(defvar wgn-untrusted-test-var nil)
(put 'wgn-untrusted-test-var 'safe-local-variable #'symbolp)
(defvar wgn-untrusted-test-evaluated nil)

(defmacro wgn-untrusted-test--with-dirs (&rest body)
  "Run BODY with `untrusted' and `trusted' bound to fresh directories.
Only `untrusted' is in `wgn/untrusted-directories'."
  (declare (indent 0))
  `(let* ((parent (make-temp-file "wgn-untrusted-" t))
          (untrusted (file-name-as-directory (expand-file-name "pr-review" parent)))
          (trusted (file-name-as-directory (expand-file-name "work" parent)))
          (wgn/untrusted-directories (list untrusted)))
     (ignore trusted)
     (make-directory untrusted)
     (make-directory trusted)
     (unwind-protect (progn ,@body)
       (delete-directory parent t))))

(defun wgn-untrusted-test--write (file contents)
  "Write CONTENTS to FILE."
  (with-temp-file file (insert contents)))

(defmacro wgn-untrusted-test--visiting (file &rest body)
  "Run BODY in a buffer visiting FILE, then kill it."
  (declare (indent 1))
  `(let ((buffer (find-file-noselect ,file)))
     (unwind-protect (with-current-buffer buffer ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer))))

(ert-deftest wgn-untrusted-dir-p-inside-and-outside ()
  (wgn-untrusted-test--with-dirs
    (should (wgn/untrusted-dir-p untrusted))
    (should (wgn/untrusted-dir-p (expand-file-name "a/b/missing.go" untrusted)))
    (should-not (wgn/untrusted-dir-p trusted))
    (should-not (wgn/untrusted-dir-p (expand-file-name "x.go" trusted)))
    (should-not (wgn/untrusted-dir-p parent))
    (let ((sibling (concat (directory-file-name untrusted) "-other/")))
      (make-directory sibling)
      (should-not (wgn/untrusted-dir-p (expand-file-name "x.go" sibling))))))

(ert-deftest wgn-untrusted-dir-p-missing-root ()
  (wgn-untrusted-test--with-dirs
    (let ((wgn/untrusted-directories (list (expand-file-name "absent/" parent))))
      (should-not (wgn/untrusted-dir-p (expand-file-name "absent/x.go" parent))))))

(ert-deftest wgn-untrusted-dir-p-follows-symlinks ()
  (wgn-untrusted-test--with-dirs
    (let ((target (expand-file-name "repo/" untrusted))
          (outside (expand-file-name "notes.txt" trusted)))
      (make-directory target)
      (wgn-untrusted-test--write outside "")
      ;; A trusted path leading into an untrusted directory.
      (make-symbolic-link target (expand-file-name "link" trusted))
      (should (wgn/untrusted-dir-p (expand-file-name "link/main.go" trusted)))
      ;; An untrusted path leading out to a trusted file.
      (make-symbolic-link outside (expand-file-name "repo/notes.txt" untrusted))
      (should (wgn/untrusted-dir-p (expand-file-name "repo/notes.txt" untrusted)))
      ;; A root named through a symlink.
      (make-symbolic-link untrusted (expand-file-name "root-link" parent))
      (let ((wgn/untrusted-directories (list (expand-file-name "root-link/" parent))))
        (should (wgn/untrusted-dir-p (expand-file-name "repo/x.go" untrusted)))))))

(ert-deftest wgn-untrusted-dir-p-ignores-case-where-the-file-system-does ()
  (wgn-untrusted-test--with-dirs
    (skip-unless (file-name-case-insensitive-p untrusted))
    (should (wgn/untrusted-dir-p
             (expand-file-name "PR-REVIEW/x.go" parent)))))

(ert-deftest wgn-untrusted-eglot-ensure-skips-untrusted-buffers ()
  (wgn-untrusted-test--with-dirs
    (let ((bad (expand-file-name "main.go" untrusted))
          (good (expand-file-name "main.go" trusted)))
      (wgn-untrusted-test--write bad "")
      (wgn-untrusted-test--write good "")
      (wgn-untrusted-test--visiting bad
        (kill-local-variable 'post-command-hook)
        (eglot-ensure)
        (should-not (local-variable-p 'post-command-hook)))
      (wgn-untrusted-test--visiting good
        (kill-local-variable 'post-command-hook)
        (eglot-ensure)
        (should (local-variable-p 'post-command-hook))))))

(ert-deftest wgn-untrusted-eglot-refuses-untrusted-buffers ()
  (wgn-untrusted-test--with-dirs
    (let ((bad (expand-file-name "main.go" untrusted))
          (guessed nil))
      (wgn-untrusted-test--write bad "")
      (wgn-untrusted-test--visiting bad
        (cl-letf (((symbol-function 'eglot--guess-contact)
                   (lambda (&rest _) (setq guessed t) (error "Guessed")))
                  ((symbol-function 'eglot--connect)
                   (lambda (&rest _) (error "Connected"))))
          (should-error (call-interactively #'eglot) :type 'user-error)
          (should-not guessed)
          (should-error (eglot nil nil nil nil nil) :type 'user-error))))))

(ert-deftest wgn-untrusted-envrc-stays-off ()
  (skip-unless (require 'envrc nil t))
  (wgn-untrusted-test--with-dirs
    (let ((bad (expand-file-name "main.go" untrusted))
          (good (expand-file-name "main.go" trusted)))
      (wgn-untrusted-test--write bad "")
      (wgn-untrusted-test--write good "")
      (cl-letf (((symbol-function 'envrc--update) #'ignore))
        (wgn-untrusted-test--visiting bad
          (envrc-mode 1)
          (should-not (bound-and-true-p envrc-mode)))
        (wgn-untrusted-test--visiting good
          (envrc-mode 1)
          (should (bound-and-true-p envrc-mode))
          (envrc-mode -1))))))

(defconst wgn-untrusted-test--git-environment
  '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
  "Keeps the user's Git configuration, and its signing, out of the tests.")

(defun wgn-untrusted-test--git (dir &rest args)
  "Run git with ARGS in DIR, or signal an error."
  (let ((default-directory dir)
        (process-environment (append wgn-untrusted-test--git-environment
                                     process-environment)))
    (unless (eql 0 (apply #'call-process "git" nil nil nil args))
      (error "git %S failed in %s" args dir))))

(defun wgn-untrusted-test--filtered-repo (dir)
  "Make a repository in DIR whose clean filter creates DIR/../filtered.
Return the committed file, whose changed time makes the next `git
status' run the filter on it."
  (let ((file (expand-file-name "main.go" dir))
        (mark (expand-file-name "../filtered" dir)))
    (make-directory dir t)
    (wgn-untrusted-test--git dir "init" "-q")
    (wgn-untrusted-test--git dir "config" "filter.mark.clean"
                             (format "touch %s; cat" (shell-quote-argument mark)))
    (wgn-untrusted-test--git dir "config" "filter.mark.smudge" "cat")
    (wgn-untrusted-test--write (expand-file-name ".gitattributes" dir) "* filter=mark\n")
    (wgn-untrusted-test--write file "package main\n")
    (wgn-untrusted-test--git dir "add" ".")
    (wgn-untrusted-test--git dir "-c" "user.name=t" "-c" "user.email=t@example.com"
                             "-c" "commit.gpgsign=false" "commit" "-q" "-m" "x")
    (delete-file mark)
    (set-file-times file (time-subtract nil 100))
    file))

(ert-deftest wgn-untrusted-vc-stays-off ()
  (wgn-untrusted-test--with-dirs
    (let* ((process-environment (append wgn-untrusted-test--git-environment
                                        process-environment))
           (bad (wgn-untrusted-test--filtered-repo
                 (file-name-as-directory (expand-file-name "repo" untrusted))))
           (good (wgn-untrusted-test--filtered-repo
                  (file-name-as-directory (expand-file-name "repo" trusted)))))
      (wgn-untrusted-test--visiting bad
        (should-not (vc-backend bad))
        (should-not vc-mode)
        ;; A mode chosen later keeps it off.
        (emacs-lisp-mode)
        (vc-refresh-state)
        (should-not (vc-backend bad)))
      (should-not (file-exists-p (expand-file-name "filtered" untrusted)))
      ;; The same repository elsewhere runs the filter, so the test can fail.
      (wgn-untrusted-test--visiting good
        (should (eq (vc-backend good) 'Git))
        (vc-state good))
      (should (file-exists-p (expand-file-name "filtered" trusted))))))

(ert-deftest wgn-untrusted-forge-skips-untrusted-buffers ()
  (skip-unless (require 'forge nil t))
  (wgn-untrusted-test--with-dirs
    (let ((bad (expand-file-name "main.go" untrusted))
          (good (expand-file-name "main.go" trusted))
          (asked nil))
      (wgn-untrusted-test--write bad "")
      (wgn-untrusted-test--write good "")
      (cl-letf (((symbol-function 'forge-db) (lambda (&rest _) t))
                ((symbol-function 'forge-get-repository)
                 (lambda (&rest _) (setq asked t) nil)))
        (wgn-untrusted-test--visiting bad
          (setq-local bug-reference-url-format nil)
          (setq asked nil)
          (forge-bug-reference-setup)
          (should-not asked))
        (wgn-untrusted-test--visiting good
          (setq-local bug-reference-url-format nil)
          (setq asked nil)
          (forge-bug-reference-setup)
          (should asked))))))

(defun wgn-untrusted-test--write-locals (dir)
  "Give DIR a .dir-locals.el and a file with local variables; return the file."
  (wgn-untrusted-test--write
   (expand-file-name ".dir-locals.el" dir)
   "((nil . ((wgn-untrusted-test-var . dir)
         (eval . (setq wgn-untrusted-test-evaluated t)))))\n")
  (let ((file (expand-file-name "notes.txt" dir)))
    (wgn-untrusted-test--write
     file
     "-*- mode: emacs-lisp -*-\n;; Local Variables:\n;; wgn-untrusted-test-var: file\n;; End:\n")
    file))

(ert-deftest wgn-untrusted-local-variables-ignored ()
  (wgn-untrusted-test--with-dirs
    (let ((file (wgn-untrusted-test--write-locals untrusted))
          (wgn-untrusted-test-evaluated nil))
      (wgn-untrusted-test--visiting file
        (should-not (local-variable-p 'wgn-untrusted-test-var))
        (should-not (eq major-mode 'emacs-lisp-mode))
        ;; A mode chosen later must not pick them up either.
        (emacs-lisp-mode)
        (should-not (local-variable-p 'wgn-untrusted-test-var)))
      (let ((dired (dired-noselect untrusted)))
        (unwind-protect
            (with-current-buffer dired
              (should-not (local-variable-p 'wgn-untrusted-test-var)))
          (kill-buffer dired)))
      (should-not wgn-untrusted-test-evaluated))))

(ert-deftest wgn-untrusted-local-variables-apply-elsewhere ()
  (wgn-untrusted-test--with-dirs
    (let ((file (wgn-untrusted-test--write-locals trusted))
          ;; Skip the `eval', which would otherwise ask.
          (enable-local-variables :safe))
      (wgn-untrusted-test--visiting file
        (should (eq major-mode 'emacs-lisp-mode))
        (should (eq wgn-untrusted-test-var 'file))))))

;;; untrusted.el ends here
