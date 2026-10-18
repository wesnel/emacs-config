;;; markdown.el --- Code block language tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../lisp/wgn-markdown.el"
                        (file-name-directory load-file-name)) nil t)
;; Before the packages load, as default.el does.
(wgn/markdown-guard-code-block-modes)
(require 'markdown-mode nil t)
(require 'markdown-overlays nil t)
(require 'org)
(require 'hl-line)

(defvar diff-font-lock-syntax)
(defvar wgn/fence--fontifying)
(declare-function wgn/fence-mode-p "wgn-markdown")
(declare-function wgn/markdown-guard-code-block-modes "wgn-markdown")
(declare-function gptel-agent--fontify-block "gptel-agent-tools")

(defmacro wgn-markdown-test--keeping-off (modes &rest body)
  "Run BODY with the global minor MODES off, and turn them off again after."
  (declare (indent 1))
  `(progn
     (dolist (mode ',modes) (funcall mode -1))
     (unwind-protect (progn ,@body)
       (dolist (mode ',modes) (funcall mode -1)))))

(defmacro wgn-markdown-test--offering (modes &rest body)
  "Run BODY with `auto-mode-alist' naming each of MODES.
markdown-mode 2.6 fontifies a block only in a mode that alist names."
  (declare (indent 1))
  `(let ((auto-mode-alist
          (append (mapcar (lambda (mode)
                            (cons (format "\\`wgn-markdown-offer-%s\\'" mode) mode))
                          ',modes)
                  auto-mode-alist)))
     ,@body))

(defvar wgn/fence--scratch-buffers)

(defun wgn-markdown-test--scratch-buffers (&optional mode)
  "Return the scratch buffers code blocks were fontified in, in MODE if given."
  (cl-remove-if-not
   (lambda (buffer)
     (and (string-match-p wgn/fence--scratch-buffers (buffer-name buffer))
          (or (not mode)
              (string-match-p (concat ":" (regexp-quote (symbol-name mode)) "\\*?\\'")
                              (buffer-name buffer)))))
   (buffer-list)))

(defun wgn-markdown-test--fontify (text)
  "Fontify TEXT as GFM with native code blocks; return the fontified text."
  (let ((markdown-fontify-code-blocks-natively t))
    (with-temp-buffer
      (insert text)
      (gfm-mode)
      (font-lock-ensure)
      (buffer-string))))

(defun wgn-markdown-test--face (text word)
  "Return the face list on WORD in TEXT."
  (ensure-list (get-text-property (string-search word text) 'face text)))

(ert-deftest wgn-markdown-fence-cannot-toggle-a-minor-mode ()
  (skip-unless (featurep 'markdown-mode))
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let ((text (wgn-markdown-test--fontify
                 "```global-hl-line\nx\n```\n\n```emacs-lisp\n(defun f ())\n```\n")))
      (should-not global-hl-line-mode)
      (should (memq 'font-lock-keyword-face (wgn-markdown-test--face text "defun"))))))

(ert-deftest wgn-markdown-fence-cannot-pick-a-mode-by-cookie ()
  (skip-unless (featurep 'markdown-mode))
  (wgn-markdown-test--keeping-off (auto-save-visited-mode)
    (wgn-markdown-test--fontify "```octave-maybe\n% -*- mode: auto-save-visited -*-\nx\n```\n")
    (should-not auto-save-visited-mode)))

(ert-deftest wgn-markdown-fence-reads-no-local-variables ()
  (skip-unless (featurep 'markdown-mode))
  (require 'message)
  (let ((asked nil)
        (default-directory (file-name-as-directory (make-temp-file "wgn-markdown" t))))
    (unwind-protect
        (progn
          (with-temp-file ".dir-locals.el"
            (insert "((nil . ((fill-column . 34))))\n"))
          (cl-letf (((symbol-function 'hack-local-variables-confirm)
                     (lambda (&rest _) (setq asked t) nil)))
            (wgn-markdown-test--fontify
             (concat "```message\nhi\n\nLocal Variables:\neval: (error \"evaluated\")\nEnd:\n```\n\n"
                     "```emacs-lisp\n;; Local Variables:\n;; fill-column: 33\n;; End:\n(defun f ())\n```\n\n"
                     "```diff\n+x\n```\n"))))
      (delete-directory default-directory t))
    (should-not asked)
    (dolist (buffer (wgn-markdown-test--scratch-buffers))
      (with-current-buffer buffer
        (should-not (eq major-mode 'message-mode))
        (should-not buffer-file-name)
        (should-not buffer-auto-save-file-name)
        (should-not delayed-mode-hooks)
        (should-not (local-variable-p 'fill-column))))))

(ert-deftest wgn-markdown-fence-knows-each-markdown-mode-scratch-buffer ()
  (dolist (name '(" *markdown-code-fontification:python-mode*"
                  " markdown-code-fontification:python-mode"
                  " *org-src-fontification:python-mode*"))
    (should (string-match-p wgn/fence--scratch-buffers name))))

(ert-deftest wgn-markdown-fence-runs-no-mode-hooks ()
  (skip-unless (featurep 'markdown-mode))
  (let* ((ran nil)
         (marker (lambda () (setq ran t))))
    (add-hook 'prog-mode-hook marker)
    (unwind-protect
        (let ((text (wgn-markdown-test--fontify
                     "```python\ndef f(): pass\n```\n\n```ruby\ndef g; end\n```\n")))
          (should-not ran)
          (should (memq 'font-lock-keyword-face (wgn-markdown-test--face text "def"))))
      (remove-hook 'prog-mode-hook marker))))

(ert-deftest wgn-markdown-fence-fontifies-modes-it-must-load ()
  (skip-unless (featurep 'markdown-mode))
  (wgn-markdown-test--offering (html-mode makefile-mode)
    (let ((text (wgn-markdown-test--fontify
                 "```html\n<b>bold</b>\n```\n\n```makefile\nall:\n\techo hi\n```\n")))
      (should (memq 'font-lock-function-name-face (wgn-markdown-test--face text "b>")))
      (should (memq 'makefile-targets (wgn-markdown-test--face text "all"))))))

(ert-deftest wgn-markdown-fence-cannot-reach-a-minor-mode-by-alias ()
  (skip-unless (featurep 'markdown-mode))
  ;; markdown-code-lang-modes maps ditaa to artist-mode, a minor mode.
  (wgn-markdown-test--fontify "```ditaa\n+--+\n```\n")
  (should-not (wgn-markdown-test--scratch-buffers 'artist-mode))
  (should-not (wgn/fence-mode-p 'artist-mode)))

(ert-deftest wgn-markdown-fence-loads-no-denied-library ()
  (skip-unless (featurep 'markdown-mode))
  ;; Loading midnight.el starts a timer; tar-mode is denied by name.
  (should-not (featurep 'midnight))
  (should-not (featurep 'tar-mode))
  (let ((timers (copy-sequence timer-list)))
    (wgn-markdown-test--fontify "```midnight\nx\n```\n\n```tar\nx\n```\n")
    (should-not (featurep 'midnight))
    (should-not (featurep 'tar-mode))
    (should (equal timer-list timers))))

(ert-deftest wgn-markdown-fence-leaves-no-timer ()
  (skip-unless (featurep 'markdown-mode))
  ;; Each starts a global timer when its mode is set.
  (let ((timers (copy-sequence timer-list))
        (idle (copy-sequence timer-idle-list)))
    (wgn-markdown-test--fontify
     "```bibtex\n@book{x,}\n```\n\n```idlwave\nx\n```\n\n```vhdl\nx\n```\n")
    (should (equal timer-list timers))
    (should (equal timer-idle-list idle))))

(defvar wgn-markdown-test-failure nil)

(define-derived-mode wgn-markdown-test-mode prog-mode "Test"
  "Sets another buffer's mode, then fails as `wgn-markdown-test-failure' says."
  (with-temp-buffer (emacs-lisp-mode))
  (when wgn-markdown-test-failure
    (signal (car wgn-markdown-test-failure) (cdr wgn-markdown-test-failure))))

(defun wgn-markdown-test--fontify-test-mode ()
  "Fontify a block in `wgn-markdown-test-mode', setting the mode afresh."
  (mapc #'kill-buffer (wgn-markdown-test--scratch-buffers 'wgn-markdown-test-mode))
  (wgn-markdown-test--offering (wgn-markdown-test-mode)
    (wgn-markdown-test--fontify "```wgn-markdown-test\nx\n```\n")))

(ert-deftest wgn-markdown-fence-runs-no-hooks-anywhere-and-leaks-none ()
  (skip-unless (featurep 'markdown-mode))
  (let* ((ran nil)
         (marker (lambda () (push (buffer-name) ran))))
    (add-hook 'prog-mode-hook marker)
    (unwind-protect
        (progn
          ;; Hooks don't run in the other buffer the mode makes either.
          (let ((wgn-markdown-test-failure nil))
            (wgn-markdown-test--fontify-test-mode))
          (should-not ran)
          ;; Fontification that signals or quits leaves hooks running
          ;; everywhere else.
          (dolist (failure '((error "test") (quit)))
            (let ((wgn-markdown-test-failure failure)
                  (failed nil))
              (condition-case nil
                  (wgn-markdown-test--fontify-test-mode)
                ((error quit) (setq failed t)))
              (should failed))
            (should-not wgn/fence--fontifying)
            (setq ran nil)
            (with-temp-buffer (emacs-lisp-mode))
            (should ran)))
      (remove-hook 'prog-mode-hook marker))))

(defvar wgn-markdown-test-real-buffer nil
  "A buffer that exists before fontification, whose mode the next mode sets.")

(defun wgn-markdown-test-named-hook-function (&rest _))

(define-derived-mode wgn-markdown-test-global-mode prog-mode "Test"
  "Changes global state as some mode bodies do."
  (add-to-list 'auto-mode-alist '("\\.wgn-markdown-test\\'" . wgn-markdown-test-global-mode))
  (add-hook 'electric-indent-functions (lambda (_char) 'wgn-markdown-test))
  (add-hook 'electric-indent-functions #'wgn-markdown-test-named-hook-function)
  (when (buffer-live-p wgn-markdown-test-real-buffer)
    (with-current-buffer wgn-markdown-test-real-buffer (emacs-lisp-mode))))

(ert-deftest wgn-markdown-fence-mode-body-leaves-no-global-state ()
  (skip-unless (featurep 'markdown-mode))
  (wgn-markdown-test--offering (wgn-markdown-test-global-mode)
    (wgn-markdown-test--mode-body-leaves-no-global-state)))

(defun wgn-markdown-test--mode-body-leaves-no-global-state ()
  "Fontify a block in `wgn-markdown-test-global-mode' and check what stays."
  (let* ((alist auto-mode-alist)
         (indent (default-value 'electric-indent-functions))
         (ran nil)
         (marker (lambda () (push (buffer-name) ran)))
         (wgn-markdown-test-real-buffer (generate-new-buffer "wgn-markdown-test-real")))
    (add-hook 'prog-mode-hook marker)
    (unwind-protect
        (progn
          (wgn-markdown-test--fontify "```wgn-markdown-test-global\nx\n```\n")
          (should (equal auto-mode-alist alist))
          ;; The closure is gone; the named function, which a library
          ;; may add only once, stays.
          (should (equal (remove #'wgn-markdown-test-named-hook-function
                                 (default-value 'electric-indent-functions))
                         indent))
          ;; A buffer that existed before keeps its mode hooks.
          (should (equal ran (list (buffer-name wgn-markdown-test-real-buffer)))))
      (remove-hook 'prog-mode-hook marker)
      (remove-hook 'electric-indent-functions #'wgn-markdown-test-named-hook-function)
      (setq auto-mode-alist alist)
      (kill-buffer wgn-markdown-test-real-buffer))))

(ert-deftest wgn-markdown-fence-keeps-what-a-library-load-adds ()
  (skip-unless (featurep 'markdown-mode))
  (let ((directory (make-temp-file "wgn-markdown-lib" t))
        (alist auto-mode-alist))
    (unwind-protect
        ;; A block on its own, and one inside a Markdown block, whose
        ;; fontification encloses the inner one's.
        (pcase-dolist (`(,name ,text) '(("wmtl" "```wmtl\nx\n```\n")
                                        ("wmtn" "~~~markdown\n```wmtn\nx\n```\n~~~\n")))
          (ert-info (name)
            (let* ((mode (intern (format "wgn-markdown-test-%s-mode" name)))
                   (feature (intern (format "wgn-markdown-test-%s" name)))
                   (file (expand-file-name (format "%s.el" feature) directory))
                   (markdown-code-lang-modes (cons (cons name mode) markdown-code-lang-modes)))
              (with-temp-file file
                (insert ";;; -*- lexical-binding: t; -*-\n"
                        (format "(add-to-list 'auto-mode-alist '(\"\\\\.%s\\\\'\" . %s))\n" name mode)
                        (format "(define-derived-mode %s prog-mode \"Lib\")\n" mode)
                        (format "(provide '%s)\n" feature)))
              (autoload mode file)
              (let ((auto-mode-alist (cons (cons "\\`wgn-markdown-offer\\'" mode) auto-mode-alist)))
                ;; markdown-mode 2.6 fontifies a block only in a mode
                ;; `auto-mode-alist' names.
                (wgn-markdown-test--fontify text)
                (should (featurep feature))
                (should (assoc (format "\\.%s\\'" name) auto-mode-alist))))))
      (setq auto-mode-alist alist)
      (delete-directory directory t))))

(define-derived-mode wgn-markdown-test-inplace-mode prog-mode "Test"
  "Edits `auto-mode-alist' in place."
  (setcdr (assoc "\\.wgn-markdown-a\\'" auto-mode-alist) 'wgn-markdown-test-inplace-mode)
  (setq auto-mode-alist (assoc-delete-all "\\.wgn-markdown-b\\'" auto-mode-alist)))

(ert-deftest wgn-markdown-fence-undoes-edits-in-place ()
  (skip-unless (featurep 'markdown-mode))
  (let ((alist auto-mode-alist))
    (unwind-protect
        (progn
          ;; Behind the first entry, so that deleting it edits the list.
          ;; The last names the mode for markdown-mode 2.6, see
          ;; `wgn-markdown-test--offering'.
          (setq auto-mode-alist
                (append (list (car alist)
                              (cons "\\.wgn-markdown-a\\'" 'text-mode)
                              (cons "\\.wgn-markdown-b\\'" 'text-mode)
                              (cons "\\`wgn-markdown-offer\\'" 'wgn-markdown-test-inplace-mode))
                        (cdr alist)))
          (let ((before (copy-tree auto-mode-alist)))
            (wgn-markdown-test--fontify "```wgn-markdown-test-inplace\nx\n```\n")
            (should (equal auto-mode-alist before))))
      (setq auto-mode-alist alist))))

(ert-deftest wgn-markdown-clojure-fence-leaves-its-indentation-in-clojure ()
  (skip-unless (and (featurep 'markdown-mode) (locate-library "clojure-mode")))
  (let ((indent (default-value 'electric-indent-functions)))
    (unwind-protect
        (progn
          (wgn-markdown-test--fontify "```clojure\n(defn f [] \"doc\")\n```\n")
          (should (equal (cl-remove-if #'symbolp (default-value 'electric-indent-functions))
                         (cl-remove-if #'symbolp indent)))
          ;; Visiting Clojure still sets it up as clojure-mode does.
          (with-temp-buffer (clojure-mode))
          (should (cl-set-difference (default-value 'electric-indent-functions) indent)))
      (set-default 'electric-indent-functions indent))))

(ert-deftest wgn-markdown-js-ts-block-leaves-auto-mode-alist ()
  (skip-unless (and (treesit-available-p) (treesit-language-available-p 'javascript)))
  (require 'js)
  (let ((alist auto-mode-alist)
        (org-src-fontify-natively t))
    (unwind-protect
        (progn
          ;; markdown-mode offers a -ts-mode only when auto-mode-alist
          ;; names it; Org calls LANG-mode for any LANG.
          (with-temp-buffer
            (insert "#+begin_src js-ts\nlet x = 1;\n#+end_src\n")
            (org-mode)
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward "let")
            (should (memq 'font-lock-keyword-face
                          (ensure-list (get-text-property (1- (point)) 'face)))))
          (should (equal auto-mode-alist alist))
          ;; Visiting JavaScript still sets it up as js-ts-mode does.
          (with-temp-buffer (js-ts-mode))
          (should-not (equal auto-mode-alist alist)))
      (setq auto-mode-alist alist))))

(ert-deftest wgn-markdown-gptel-agent-block-picks-no-mode-from-its-text ()
  (skip-unless (require 'gptel-agent-tools nil t))
  (wgn-markdown-test--keeping-off (auto-save-visited-mode)
    (with-temp-buffer
      (insert "-*- mode: auto-save-visited -*-\nx\n")
      (gptel-agent--fontify-block "notes.txt" (point-min) (point-max))
      (should-not auto-save-visited-mode))
    (with-temp-buffer
      (insert "def f(): pass\n")
      (gptel-agent--fontify-block "x.py" (point-min) (point-max))
      (should (get-text-property 1 'font-lock-face)))))

(ert-deftest wgn-markdown-diff-fence-sets-no-mode-from-its-header ()
  (skip-unless (featurep 'markdown-mode))
  (require 'diff-mode)
  (let* ((calls nil)
         (trace (lambda (&rest _) (push (buffer-name) calls))))
    (advice-add 'set-auto-mode :before trace)
    (unwind-protect
        (wgn-markdown-test--keeping-off (auto-save-visited-mode)
          (dolist (syntax '(hunk-only hunk-also))
            (let ((diff-font-lock-syntax syntax))
              (wgn-markdown-test--fontify
               (concat "```diff\n--- a/x.tar\n+++ b/x.tar\n@@ -1,2 +1,2 @@\n x\n-y\n+z\n```\n\n"
                       "```diff\n--- a/x.m\n+++ b/x.m\n@@ -1,2 +1,2 @@\n"
                       " % -*- mode: auto-save-visited -*-\n-x\n+y\n```\n"))))
          (should-not calls)
          (should-not auto-save-visited-mode))
      (advice-remove 'set-auto-mode trace))))

(ert-deftest wgn-markdown-org-export-block-cannot-toggle-a-minor-mode ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let ((org-src-fontify-natively t))
      (with-temp-buffer
        (insert "#+begin_export global-hl-line\nx\n#+end_export\n")
        (org-mode)
        (font-lock-ensure)
        (should-not global-hl-line-mode)))))

(ert-deftest wgn-markdown-ordinary-fences-still-fontify ()
  (skip-unless (featurep 'markdown-mode))
  (pcase-dolist (`(,lang ,body ,word)
                 `(,@(and (locate-library "go-mode") '(("go" "func main() {}" "func")))
                   ("python" "def f(): pass" "def")
                   ("elisp" "(defun f ())" "defun")
                   ("emacs-lisp" "(defun f ())" "defun")
                   ("sh" "if true; then :; fi" "if")
                   ("bash" "if true; then :; fi" "if")
                   ("diff" "+added" "added")))
    (ert-info (lang)
      (let ((text (wgn-markdown-test--fontify (format "```%s\n%s\n```\n" lang body))))
        (should (cl-set-difference (wgn-markdown-test--face text word)
                                   '(markdown-code-face markdown-pre-face)))))))

(ert-deftest wgn-markdown-fence-modes ()
  (dolist (mode '(fundamental-mode emacs-lisp-mode python-mode html-mode makefile-mode
                  diff-mode conf-unix-mode css-mode))
    (ert-info ((symbol-name mode))
      (should (wgn/fence-mode-p mode))))
  (dolist (mode '(global-hl-line-mode auto-fill-mode octave-maybe-mode c-or-c++-mode
                  message-mode mail-mode org-mode latex-mode special-mode shell-mode
                  compilation-mode log-edit-mode image-mode archive-mode tar-mode hexl-mode
                  ses-mode nxml-mode plstore-mode delete-file
                  wgn-markdown-test-no-such-mode nil))
    (ert-info ((format "%s" mode))
      (should-not (wgn/fence-mode-p mode)))))

(ert-deftest wgn-markdown-overlays-fence-cannot-toggle-a-minor-mode ()
  (skip-unless (featurep 'markdown-overlays))
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let* ((markdown-overlays-highlight-blocks t)
           (ran nil)
           (marker (lambda () (setq ran t))))
      (add-hook 'prog-mode-hook marker)
      (unwind-protect
          (with-temp-buffer
            (insert "Reply:\n\n```global-hl-line\nx\n```\n\n```emacs-lisp\n(defun f ())\n```\n")
            (markdown-overlays-put)
            (should-not global-hl-line-mode)
            (should-not ran)
            (goto-char (point-min))
            (search-forward "defun")
            (should (cl-some (lambda (overlay)
                               (memq 'font-lock-keyword-face
                                     (ensure-list (overlay-get overlay 'face))))
                             (overlays-at (1- (point))))))
        (remove-hook 'prog-mode-hook marker)))))

(ert-deftest wgn-markdown-org-block-cannot-toggle-a-minor-mode ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let ((org-src-fontify-natively t))
      (with-temp-buffer
        (insert "#+begin_src global-hl-line\nx\n#+end_src\n\n#+begin_src emacs-lisp\n(defun f ())\n#+end_src\n")
        (org-mode)
        (font-lock-ensure)
        (should-not global-hl-line-mode)
        (goto-char (point-min))
        (search-forward "defun")
        (should (memq 'font-lock-keyword-face
                      (ensure-list (get-text-property (1- (point)) 'face))))
        ;; Outside fontification, the language's mode is Org's to choose.
        (should (eq (org-src-get-lang-mode "global-hl-line") 'global-hl-line-mode))))))

;;; markdown.el ends here
