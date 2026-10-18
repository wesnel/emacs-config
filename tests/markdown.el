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
(defvar wgn/org-src-edit-modes)
(defvar major-mode-remap-alist)
(defvar major-mode-remap-defaults)
(declare-function wgn/fence-mode-p "wgn-markdown")
(declare-function wgn/org-edit-src-anyway "wgn-markdown")
(declare-function wgn/markdown-guard-code-block-modes "wgn-markdown")
(declare-function gptel-agent--fontify-block "gptel-agent-tools")
(declare-function org-odt-do-format-code "ox-odt")

(defmacro wgn-markdown-test--keeping-off (modes &rest body)
  "Run BODY with the global minor MODES off, and turn them off again after."
  (declare (indent 1))
  `(progn
     (dolist (mode ',modes) (funcall mode -1))
     (unwind-protect (progn ,@body)
       (dolist (mode ',modes) (funcall mode -1)))))

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
    (dolist (buffer (buffer-list))
      (when (string-prefix-p " *markdown-code-fontification:" (buffer-name buffer))
        (with-current-buffer buffer
          (should-not (eq major-mode 'message-mode))
          (should-not buffer-file-name)
          (should-not buffer-auto-save-file-name)
          (should-not delayed-mode-hooks)
          (should-not (local-variable-p 'fill-column)))))))

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
  (let ((text (wgn-markdown-test--fontify
               "```html\n<b>bold</b>\n```\n\n```makefile\nall:\n\techo hi\n```\n")))
    (should (memq 'font-lock-function-name-face (wgn-markdown-test--face text "b>")))
    (should (memq 'makefile-targets (wgn-markdown-test--face text "all")))))

(ert-deftest wgn-markdown-fence-cannot-reach-a-minor-mode-by-alias ()
  (skip-unless (featurep 'markdown-mode))
  ;; markdown-code-lang-modes maps ditaa to artist-mode, a minor mode.
  (wgn-markdown-test--fontify "```ditaa\n+--+\n```\n")
  (should-not (get-buffer " *markdown-code-fontification:artist-mode*"))
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
  (when-let* ((scratch (get-buffer " *markdown-code-fontification:wgn-markdown-test-mode*")))
    (kill-buffer scratch))
  (wgn-markdown-test--fontify "```wgn-markdown-test\nx\n```\n"))

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
              (wgn-markdown-test--fontify text)
              (should (featurep feature))
              (should (rassq mode auto-mode-alist)))))
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
          (setq auto-mode-alist
                (append (list (car alist)
                              (cons "\\.wgn-markdown-a\\'" 'text-mode)
                              (cons "\\.wgn-markdown-b\\'" 'text-mode))
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

(defmacro wgn-markdown-test--org (text &rest body)
  "Run BODY in an Org buffer holding TEXT, noting runs of `prog-mode-hook'.
BODY sees `ran', the buffers the hook ran in.  Prompting fails."
  (declare (indent 1))
  `(let* ((ran nil)
          (marker (lambda () (push (buffer-name) ran))))
     (add-hook 'prog-mode-hook marker)
     (unwind-protect
         (cl-letf (((symbol-function 'read-from-minibuffer)
                    (lambda (prompt &rest _) (error "Prompted: %s" prompt)))
                   ((symbol-function 'read-string)
                    (lambda (prompt &rest _) (error "Prompted: %s" prompt))))
           (with-temp-buffer
             (insert ,text)
             (org-mode)
             (goto-char (point-min))
             ,@body))
       (remove-hook 'prog-mode-hook marker))))

(defun wgn-markdown-test--org-names-export-modes-p ()
  "Return non-nil when Org's exporters take a block's mode from
`org-src-get-lang-mode', which the guard relies on."
  (version<= "9.8" (org-version)))

(ert-deftest wgn-markdown-org-export-calls-only-allowed-modes-inertly ()
  (skip-unless (and (wgn-markdown-test--org-names-export-modes-p) (require 'htmlize nil t)))
  (require 'ox-html)
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (wgn-markdown-test--org ""
      (let ((html (org-export-string-as
                   (concat "#+begin_src global-hl-line\nx\n#+end_src\n\n"
                           "#+begin_src emacs-lisp\n(defun f ())\n#+end_src\n")
                   'html t)))
        (should-not global-hl-line-mode)
        (should-not ran)
        (should (string-match-p "<span[^>]*>defun</span>" html))
        (should (string-match-p "<pre class=\"src src-global-hl-line\">\\(<code>\\)?x\n" html))))))

(ert-deftest wgn-markdown-org-odt-export-calls-only-allowed-modes-inertly ()
  (skip-unless (wgn-markdown-test--org-names-export-modes-p))
  (require 'ox-odt)
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (wgn-markdown-test--org ""
      (let ((info (list :odt-fontify-srcblocks t)))
        (should (string-search ">x<" (org-odt-do-format-code "x" info "global-hl-line")))
        (should-not global-hl-line-mode)
        (should (string-search "OrgSrcFontLockKeywordFace"
                               (org-odt-do-format-code "(defun f ())" info "emacs-lisp")))
        (should-not ran)))))

(ert-deftest wgn-markdown-org-noweb-calls-only-allowed-modes-inertly ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (wgn-markdown-test--org
        (concat "#+name: inner\n#+begin_src emacs-lisp\n(inner)\n#+end_src\n\n"
                "#+begin_src global-hl-line :noweb yes :comments noweb\n<<inner>>\n#+end_src\n\n"
                "#+begin_src emacs-lisp :noweb yes :comments noweb\n<<inner>>\n#+end_src\n")
      (search-forward "global-hl-line :noweb")
      ;; Org asks for the comment syntax of a block without a mode.
      (should (equal (should-error (org-babel-expand-noweb-references))
                     '(error "Prompted: No comment syntax is defined.  Use: ")))
      (should-not global-hl-line-mode)
      (search-forward "emacs-lisp :noweb")
      (should (string-prefix-p ";; " (org-babel-expand-noweb-references)))
      (should-not ran))))

(ert-deftest wgn-markdown-org-tangle-calls-only-allowed-modes-inertly ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let* ((directory (file-name-as-directory (make-temp-file "wgn-markdown-tangle" t)))
           (org (expand-file-name "blocks.org" directory)))
      (unwind-protect
          (wgn-markdown-test--org ""
            (with-temp-file org
              (insert "#+begin_src global-hl-line :tangle a.txt\nx\n#+end_src\n\n"
                      "#+begin_src emacs-lisp :tangle b.el :comments link\n(f)\n#+end_src\n"))
            (with-current-buffer (find-file-noselect org)
              (setq ran nil)
              (unwind-protect (org-babel-tangle)
                (kill-buffer)))
            (should-not global-hl-line-mode)
            (should-not ran)
            (should (file-exists-p (expand-file-name "a.txt" directory)))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "b.el" directory))
              (should (string-prefix-p ";; [[" (buffer-string)))))
        (delete-directory directory t)))))

(defun wgn-markdown-test--edit (edit)
  "Call EDIT at point; return the edit buffer's mode and whether hooks ran.
The edit is aborted after."
  (let ((org-src-window-setup 'current-window)
        (ran nil))
    (let ((marker (lambda () (setq ran t))))
      (add-hook 'prog-mode-hook marker)
      (unwind-protect
          (save-window-excursion
            (funcall edit)
            (prog1 (list major-mode ran)
              (org-edit-src-abort)))
        (remove-hook 'prog-mode-hook marker)))))

(ert-deftest wgn-markdown-org-edit-refuses-modes-others-may-not-use ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (pcase-dolist (`(,edit ,bad ,good)
                   '((org-edit-src-code
                      "#+begin_src global-hl-line\nx\n#+end_src\n"
                      "#+begin_src emacs-lisp\n(f)\n#+end_src\n")
                     (org-edit-export-block
                      "#+begin_export global-hl-line\nx\n#+end_export\n"
                      "#+begin_export emacs-lisp\n(f)\n#+end_export\n")
                     (org-edit-inline-src-code "src_global-hl-line{x}\n" "src_emacs-lisp{(f)}\n")))
      (ert-info ((symbol-name edit))
        (wgn-markdown-test--org bad
          (forward-char 2)
          (should (string-match-p "\\`No editing in .global-hl-line-mode.:"
                                   (cadr (should-error (funcall edit) :type 'user-error))))
          (should-not global-hl-line-mode)
          (should-not (cl-some (lambda (buffer) (string-prefix-p "*Org Src" (buffer-name buffer)))
                               (buffer-list))))
        ;; An allowed mode is set as Org sets it, hooks included.
        (wgn-markdown-test--org good
          (forward-char 2)
          (should (equal (wgn-markdown-test--edit edit) '(emacs-lisp-mode t))))))))

(define-derived-mode wgn-markdown-test-special-mode special-mode "Test"
  "A mode outside every set a block may be edited in.")

(defun wgn-markdown-test--org-src-buffers ()
  "Return the live buffers Org opened to edit a block."
  (cl-remove-if-not (lambda (buffer) (string-prefix-p "*Org Src" (buffer-name buffer)))
                    (buffer-list)))

(ert-deftest wgn-markdown-org-edit-takes-the-operators-word-for-a-mode ()
  (pcase-dolist (`(,text ,edit)
                 '(("#+begin_src wgn-markdown-test-special\nx\n#+end_src\n" org-edit-src-code)
                   ("#+begin_src wgn-markdown-test-special :session s\nx\n#+end_src\n"
                    org-edit-src-code)
                   ("#+begin_export wgn-markdown-test-special\nx\n#+end_export\n"
                    org-edit-export-block)
                   ("src_wgn-markdown-test-special{x}\n" org-edit-inline-src-code)))
    (ert-info (text)
      (wgn-markdown-test--org text
        (forward-char 2)
        (should (string-match-p "\\`No editing in .wgn-markdown-test-special-mode.:"
                                (cadr (should-error (funcall edit) :type 'user-error))))
        ;; A prefix argument is no override.
        (let ((current-prefix-arg '(4))
              (this-command edit))
          (should-error (funcall edit) :type 'user-error))
        (let ((current-prefix-arg '(4))
              (this-command 'org-edit-special))
          (should-error (org-edit-special '(4))))
        (should-not (wgn-markdown-test--org-src-buffers))
        (let ((wgn/org-src-edit-modes '(wgn-markdown-test-special-mode)))
          (should (eq (car (wgn-markdown-test--edit edit)) 'wgn-markdown-test-special-mode)))
        ;; Once, whatever the block's session.
        (should (eq (car (wgn-markdown-test--edit #'wgn/org-edit-src-anyway))
                    'wgn-markdown-test-special-mode))
        (should-error (funcall edit) :type 'user-error)))))

(ert-deftest wgn-markdown-org-fills-and-comments-in-the-operators-modes ()
  ;; Org fills, comments and indents a block by editing it in its mode.
  (wgn-markdown-test--org "#+begin_src org\nsome words\n#+end_src\n"
    (search-forward "some")
    (should (org-fill-paragraph))
    (should (string-match-p "\n *some words\n#\\+end_src" (buffer-string))))
  (wgn-markdown-test--org "#+begin_src xml\n<a>some words</a>\n#+end_src\n"
    (search-forward "some")
    (should (org-fill-paragraph)))
  (wgn-markdown-test--org "#+begin_src latex\n\\foo\n#+end_src\n"
    (search-forward "\\foo")
    ;; AUCTeX maps latex to its own LaTeX-mode, which the default
    ;; `wgn/org-src-edit-modes' leaves out.
    (let ((this-command 'org-comment-dwim)
          (org-src-lang-modes (assoc-delete-all "latex" (copy-sequence org-src-lang-modes)))
          (major-mode-remap-alist nil)
          (major-mode-remap-defaults nil))
      (call-interactively #'org-comment-dwim))
    ;; Without a region, latex-mode's comment-dwim comments the line's end.
    (should (string-match-p "\\\\foo +%\n#\\+end_src" (buffer-string)))))

(ert-deftest wgn-markdown-org-edits-in-the-operators-modes ()
  ;; bibtex-mode derives from no mode; xml-mode is an alias of
  ;; nxml-mode, which neither has loaded yet.
  (pcase-dolist (`(,lang ,mode) '(("bibtex" bibtex-mode) ("xml" nxml-mode) ("nxml" nxml-mode)))
    (ert-info (lang)
      (wgn-markdown-test--org (format "#+begin_src %s\n<a/>\n#+end_src\n" lang)
        (forward-char 2)
        (should (eq (car (wgn-markdown-test--edit #'org-edit-src-code)) mode))))))

(ert-deftest wgn-markdown-org-edit-calls-no-minor-mode-with-a-variable ()
  ;; normal-erase-is-backspace-mode keeps its state in the terminal, so
  ;; it is in no list of minor modes; calling it swaps Backspace and DEL.
  (let ((before (terminal-parameter nil 'normal-erase-is-backspace))
        (org-src-tab-acts-natively t))
    (wgn-markdown-test--org "#+begin_src normal-erase-is-backspace\nsome words\n#+end_src\n"
      (search-forward "some")
      (should-error (org-edit-special) :type 'user-error)
      (ignore-errors (org-fill-paragraph))
      (ignore-errors (org-indent-line))
      (should (equal (terminal-parameter nil 'normal-erase-is-backspace) before))
      (should-not (wgn-markdown-test--org-src-buffers)))))

(ert-deftest wgn-markdown-org-edit-loads-no-denied-library ()
  (skip-unless (not (featurep 'ses)))
  (wgn-markdown-test--org "#+begin_src ses\nx\n#+end_src\n"
    (forward-char 2)
    (should-error (org-edit-special) :type 'user-error)
    (should-not (featurep 'ses))))

(ert-deftest wgn-markdown-org-prefix-on-another-command-is-no-override ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (wgn-markdown-test--org "#+begin_src global-hl-line\nx\n#+end_src\n"
      (search-forward "x")
      ;; C-u M-q
      (let ((current-prefix-arg '(4))
            (this-command 'org-fill-paragraph))
        (should-error (org-fill-paragraph)))
      (should-not global-hl-line-mode))))

(ert-deftest wgn-markdown-org-refusal-reaches-the-operator ()
  ;; Org 9.8 edits in a block's mode for M-q, M-; and key sequences
  ;; under a handler that drops the refusal.
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (dolist (command (list #'org-fill-paragraph
                           #'org-comment-dwim
                           (lambda () (interactive)
                             (org-babel-do-key-sequence-in-edit-buffer (kbd "C-e")))))
      (wgn-markdown-test--org "#+begin_src global-hl-line\nsome words\n#+end_src\n"
        (search-forward "some")
        (let ((this-command command))
          (should (string-match-p "\\`No editing in .global-hl-line-mode.:"
                                  (cadr (should-error (call-interactively command)
                                                      :type 'user-error)))))
        (should-not global-hl-line-mode)
        (should-not (wgn-markdown-test--org-src-buffers))))))

(ert-deftest wgn-markdown-org-edits-in-the-operators-other-modes ()
  (dolist (mode '(hledger-mode artist-mode cperl-mode texinfo-mode))
    (should (memq mode wgn/org-src-edit-modes)))
  ;; Org edits a ditaa block in artist-mode, a minor mode.
  (wgn-markdown-test--org "#+begin_src ditaa\n+--+\n#+end_src\n"
    (forward-char 2)
    (let ((org-src-window-setup 'current-window))
      (save-window-excursion
        (org-edit-src-code)
        (should (bound-and-true-p artist-mode))
        (org-edit-src-abort)))))

(ert-deftest wgn-markdown-org-refusal-reaches-the-operator-from-other-commands ()
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (dolist (command (list (lambda () (interactive)
                             (comment-region (line-beginning-position) (line-end-position)))
                           (lambda () (interactive)
                             (org-comment-or-uncomment-region
                              (line-beginning-position) (line-end-position)))
                           (lambda () (interactive) (org-insert-comment))
                           (lambda () (interactive) (org-fill-element))))
      (wgn-markdown-test--org "#+begin_src global-hl-line\nsome words\n#+end_src\n"
        (search-forward "some")
        (let ((this-command command))
          (should (string-match-p "\\`No editing in .global-hl-line-mode.:"
                                  (cadr (should-error (call-interactively command)
                                                      :type 'user-error)))))
        (should-not global-hl-line-mode)))))

(ert-deftest wgn-markdown-org-edit-buffer-text-chooses-no-mode ()
  ;; normal-mode would read a mode and variables from the block's text.
  (skip-unless (not (featurep 'ses)))
  (let ((before (terminal-parameter nil 'normal-erase-is-backspace)))
    (dolist (cookie '("normal-erase-is-backspace" "ses"))
      (ert-info (cookie)
        (wgn-markdown-test--org
            (format "#+begin_src emacs-lisp\n;; -*- mode: %s -*-\n(f)\n;; Local Variables:\n;; fill-column: 33\n;; End:\n#+end_src\n"
                    cookie)
          (forward-char 2)
          (let ((org-src-window-setup 'current-window)
                (enable-local-variables :all))
            (save-window-excursion
              (org-edit-src-code)
              (unwind-protect
                  (progn
                    (normal-mode)
                    (should (eq major-mode 'emacs-lisp-mode))
                    (should (bound-and-true-p org-src-mode))
                    (should-not (local-variable-p 'fill-column))
                    (should (equal (terminal-parameter nil 'normal-erase-is-backspace) before))
                    (should-not (featurep 'ses)))
                (org-edit-src-abort)))))))))

(defvar TeX-parse-self)
(defvar TeX-style-hook-list)

(defvar wgn/org-src-auctex)
(defvar org-export-show-temporary-export-buffer)
(declare-function wgn/org-src--remap "wgn-markdown")

(defun wgn-markdown-test--latex (how search &optional plain)
  "Do HOW at SEARCH in an Org file beside AUCTeX style files that leave a mark.
HOW is a symbol naming what to do.  Unless PLAIN, the block and the
file set variables that steer AUCTeX to those files.  Return the mode
an edit was in, or nil, and whether any style file loaded, as (MODE
MARKED)."
  (let* ((directory (file-name-as-directory (make-temp-file "wgn-markdown-tex" t)))
         (mark (expand-file-name "MARK" directory))
         (org-src-window-setup 'current-window)
         (enable-local-variables :all)
         mode)
    (unwind-protect
        (progn
          (dolist (file '("style/evil.el" "auto/evil.el" "sub/style/evil.el" "style/sub.el"))
            (make-directory (file-name-directory (expand-file-name file directory)) t)
            (with-temp-file (expand-file-name file directory)
              (insert (format "(write-region %S nil %S t)\n" file mark))))
          (with-temp-file (expand-file-name "doc.org" directory)
            (insert "* x\n#+begin_src latex\n\\documentclass{article}\n\\usepackage{evil}\n"
                    "\\begin{document}\nhello world here\n\\end{document}\n"
                    (if plain ""
                      "%%% Local Variables:\n%%% TeX-master: \"sub/evil\"\n%%% End:\n")
                    "#+end_src\n\n\\begin{equation}\n\\usepackage{evil}\nx\n\\end{equation}\n\n"
                    (if plain "" "# Local Variables:\n# TeX-parse-self: t\n# End:\n")))
          (setq TeX-style-hook-list nil)
          (with-current-buffer (find-file-noselect (expand-file-name "doc.org" directory))
            (unwind-protect
                (progn
                  (goto-char (point-min))
                  (search-forward search)
                  (save-window-excursion
                    (pcase how
                      ('fill (org-fill-paragraph))
                      ('comment (call-interactively #'org-comment-dwim))
                      ('tab (org-cycle))
                      (_
                       (org-edit-special)
                       (setq mode major-mode)
                       (pcase how
                         ('normal-mode (normal-mode))
                         ('C-c-C-n (let ((command (key-binding (kbd "C-c C-n"))))
                                     (when (commandp command) (call-interactively command)))))
                       (set-buffer-modified-p nil)
                       (org-edit-src-abort)))))
              (set-buffer-modified-p nil)
              (kill-buffer)))
          (list mode (file-exists-p mark)))
      (delete-directory directory t))))

(defconst wgn-markdown-test--latex-ways
  '((edit "hello") (environment "{equation}") (fill "hello") (comment "hello") (tab "hello")
    (normal-mode "hello") (C-c-C-n "hello"))
  "The ways to reach a latex block's mode, and where.")

(ert-deftest wgn-markdown-org-latex-edits-in-emacs-latex-mode ()
  ;; AUCTeX's LaTeX-mode loads style/NAME.el for \usepackage{NAME}
  ;; from the directories of the buffer it parses; latex-mode loads none.
  ;; This config sets TeX-parse-self, without which LaTeX-mode loads none.
  (skip-unless (and (require 'tex-site nil t) (require 'tex nil t)))
  (let ((TeX-parse-self t))
    (pcase-dolist (`(,how ,search) wgn-markdown-test--latex-ways)
      (ert-info ((symbol-name how))
        (pcase-let ((`(,mode ,marked) (wgn-markdown-test--latex how search)))
          (should-not marked)
          (when mode
            (should (eq mode 'latex-mode))))))))

(ert-deftest wgn-markdown-org-latex-edits-in-auctex-when-asked ()
  (skip-unless (and (require 'tex-site nil t) (require 'tex nil t)))
  (let ((wgn/org-src-auctex t)
        (TeX-parse-self t))
    (should (eq (car (wgn-markdown-test--latex 'edit "hello" t)) 'LaTeX-mode))
    ;; The ways in which LaTeX-mode loads no style file beside the
    ;; Org file, when nothing steers it there.
    (pcase-dolist (`(,how ,search) (seq-take wgn-markdown-test--latex-ways 5))
      (ert-info ((symbol-name how))
        (should-not (cadr (wgn-markdown-test--latex how search t)))))))

(ert-deftest wgn-markdown-org-texinfo-edits-in-emacs-texinfo-mode ()
  (wgn-markdown-test--org "#+begin_src texinfo\n@node Top\n#+end_src\n"
    (forward-char 2)
    (should (eq (car (wgn-markdown-test--edit #'org-edit-src-code)) 'texinfo-mode))
    (when (and (require 'tex-site nil t) (eq (wgn/org-src--remap 'texinfo-mode) 'Texinfo-mode))
      (let ((wgn/org-src-auctex t))
        (should (eq (car (wgn-markdown-test--edit #'org-edit-src-code)) 'Texinfo-mode))))))

(ert-deftest wgn-markdown-org-edit-mode-setup-reads-no-block-variables ()
  ;; Some global minor modes, such as jinx's, read local variables
  ;; while the edit buffer's mode is set up.
  (let ((reader (lambda () (hack-local-variables 'ignore-mode)))
        (enable-local-variables :all))
    (add-hook 'after-change-major-mode-hook reader)
    (unwind-protect
        (wgn-markdown-test--org
            "#+begin_src emacs-lisp\n(f)\n;; Local Variables:\n;; fill-column: 33\n;; End:\n#+end_src\n"
          (forward-char 2)
          (let ((org-src-window-setup 'current-window))
            (save-window-excursion
              (org-edit-src-code)
              (unwind-protect (should-not (local-variable-p 'fill-column))
                (org-edit-src-abort)))))
      (remove-hook 'after-change-major-mode-hook reader))))

(ert-deftest wgn-markdown-org-html-export-picks-no-mode-from-the-text ()
  ;; A Local Variables paragraph the Org file's own visit does not reach,
  ;; being more than 3000 characters from its end.
  (require 'ox-html)
  (wgn-markdown-test--keeping-off (global-hl-line-mode)
    (let ((text (concat "* h\nwords\n\nLocal Variables:\nmode: global-hl-line\nEnd:\n"
                        (mapconcat (lambda (_) "# padding padding padding padding padding\n")
                                   (number-sequence 1 100) "")))
          (enable-local-variables :all))
      (org-export-string-as text 'html)
      (should-not global-hl-line-mode)
      (wgn-markdown-test--org text
        (let ((org-export-show-temporary-export-buffer nil))
          (unwind-protect
              (progn
                (org-html-export-as-html)
                (should-not global-hl-line-mode))
            (when (get-buffer "*Org HTML Export*")
              (kill-buffer "*Org HTML Export*"))))))))

(ert-deftest wgn-markdown-org-html-export-picks-no-mode-from-its-first-line ()
  ;; interpreter-mode-alist reads #! lines whatever the local variables.
  (require 'ox-html)
  (let* ((called nil)
         (spy (lambda (&rest _) (push (buffer-name) called))))
    (advice-add 'perl-mode :before spy)
    (unwind-protect
        (let ((text "#+HTML_DOCTYPE: #!/usr/bin/env perl\n* h\nwords\n"))
          (org-export-string-as text 'html)
          (wgn-markdown-test--org text
            (let ((org-export-show-temporary-export-buffer nil))
              (with-current-buffer (org-html-export-as-html)
                (unwind-protect (should-not (eq major-mode 'perl-mode))
                  (kill-buffer)))))
          (should-not called)
          ;; An ordinary export still gets the mode an .html file gets.
          (wgn-markdown-test--org "* h\nwords\n"
            (let ((org-export-show-temporary-export-buffer nil))
              (with-current-buffer (org-html-export-as-html)
                (unwind-protect (should (eq major-mode (wgn/fence--file-mode "x.html")))
                  (kill-buffer))))))
      (advice-remove 'perl-mode spy))))

(defvar wgn/org-src--editing)

(ert-deftest wgn-markdown-org-edit-leaves-other-buffers-their-modes ()
  ;; A file visited while Org edits a block, as from a timer during its
  ;; prompt, sets its mode and local variables as ever.
  (let* ((directory (file-name-as-directory (make-temp-file "wgn-markdown-visit" t)))
         (file (expand-file-name "visited.py" directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "# -*- fill-column: 42 -*-\nprint(1)\n"))
          (let ((wgn/org-src--editing t)
                (enable-local-variables :all))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect (should (equal (list major-mode fill-column) '(python-mode 42)))
                (kill-buffer)))))
      (delete-directory directory t))))

(declare-function wgn/org-src--async-init-file "wgn-markdown")
(declare-function wgn/fence--file-mode "wgn-markdown")

(ert-deftest wgn-markdown-org-async-export-child-is-guarded ()
  ;; An asynchronous export's Emacs loads only `org-export-async-init-file'.
  (let ((file (wgn/org-src--async-init-file)))
    (unwind-protect
        (with-temp-buffer
          (should (zerop (call-process
                          (expand-file-name invocation-name invocation-directory) nil t nil
                          "--batch" "-Q" "-l" file "--eval"
                          "(princ (list (and (advice-member-p 'wgn/fence--inert 'org-html-fontify-code) t) (and (advice-member-p 'wgn/org-src--html-by-name 'org-html-final-function) t)))")))
          (should (string-search "(t t)" (buffer-string))))
      (delete-file file))))

(defvar wgn/fence--source)

(ert-deftest wgn-markdown-org-async-init-is-new-and-private-each-time ()
  ;; A file found again by name could be anyone's.
  (let ((files (list (wgn/org-src--async-init-file) (wgn/org-src--async-init-file))))
    (unwind-protect
        (progn
          (should-not (equal (car files) (cadr files)))
          (dolist (file files)
            (should (= (file-modes file) #o600))))
      (mapc #'delete-file files))))

(ert-deftest wgn-markdown-org-async-export-deletes-its-init ()
  (require 'ox-html)
  (let* ((commands nil)
         (spy (lambda (process) (push (process-command process) commands) process))
         (org-export-show-temporary-export-buffer nil))
    (advice-add 'start-process :filter-return spy)
    (unwind-protect
        (wgn-markdown-test--org "* h\nwords\n"
          (org-html-export-as-html t))
      (advice-remove 'start-process spy))
    (let* ((command (car commands))
           (init (cadr (member "-l" command)))
           (process (cl-find-if (lambda (p) (equal (process-command p) command)) (process-list))))
      (should (string-search "wgn-markdown-async-" init))
      (with-timeout (120 (ert-fail "The export did not end"))
        (while (and process (process-live-p process))
          (accept-process-output process 0.2)))
      ;; The sentinel runs once the process has ended.
      (accept-process-output nil 0.5)
      (should-not (file-exists-p init))
      ;; The guarded Emacs exported.
      (should (string-search "words" (with-current-buffer "*Org HTML Export*" (buffer-string)))))
    (when (get-buffer "*Org HTML Export*")
      (kill-buffer "*Org HTML Export*"))))

(ert-deftest wgn-markdown-org-async-init-refuses-a-source-it-cannot-read ()
  (let ((first ";;; wgn-markdown.el --- Code blocks\n")
        (last ";;; wgn-markdown.el ends here\n"))
    (dolist (text (list "(defun f ())\n"
                        (concat first first "(defun f ())\n" last)
                        (concat first "(defun f ())\n" last last)
                        (concat first "(defun f (\n" last)
                        (concat "  " first "(defun f ())\n" last)))
      (ert-info (text)
        (let* ((source (make-temp-file "wgn-markdown-source" nil ".el" text))
               (wgn/fence--source source))
          (unwind-protect
              (should-error (wgn/org-src--async-init-file) :type 'user-error)
            (delete-file source)))))))

(declare-function org-latex-export-as-latex "ox-latex")
(declare-function org-beamer-export-as-latex "ox-beamer")
(declare-function org-koma-letter-export-as-latex "ox-koma-letter")

(defun wgn-markdown-test--latex-export (export)
  "Call EXPORT on an Org file beside AUCTeX style files that leave a mark.
Return the export buffer's mode and whether any style file loaded, as
\(MODE MARKED)."
  (let* ((directory (file-name-as-directory (make-temp-file "wgn-markdown-tex" t)))
         (mark (expand-file-name "MARK" directory))
         (org-export-show-temporary-export-buffer nil)
         mode)
    (unwind-protect
        (progn
          (dolist (file '("style/evil.el" "auto/evil.el"))
            (make-directory (file-name-directory (expand-file-name file directory)) t)
            (with-temp-file (expand-file-name file directory)
              (insert (format "(write-region %S nil %S t)\n" file mark))))
          (with-temp-file (expand-file-name "doc.org" directory)
            (insert "#+LATEX_HEADER: \\usepackage{evil}\n* h\nwords\n"))
          (setq TeX-style-hook-list nil)
          (with-current-buffer (find-file-noselect (expand-file-name "doc.org" directory))
            (unwind-protect
                (let ((buffer (funcall export)))
                  (with-current-buffer buffer (setq mode major-mode))
                  (kill-buffer buffer))
              (kill-buffer)))
          (list mode (file-exists-p mark)))
      (delete-directory directory t))))

(ert-deftest wgn-markdown-org-latex-export-buffers-use-emacs-latex-mode ()
  ;; AUCTeX's LaTeX-mode in an export buffer parses the exported text
  ;; from the Org file's directory, as for a block's edit buffer.
  (skip-unless (and (require 'tex-site nil t) (require 'tex nil t)
                    (require 'ox-beamer nil t) (require 'ox-koma-letter nil t)))
  (let ((TeX-parse-self t))
    (dolist (export (list #'org-latex-export-as-latex #'org-beamer-export-as-latex
                          #'org-koma-letter-export-as-latex))
      (ert-info ((symbol-name export))
        (should (equal (wgn-markdown-test--latex-export export) '(latex-mode nil)))
        (let ((wgn/org-src-auctex t))
          (should (eq (car (wgn-markdown-test--latex-export export)) 'LaTeX-mode)))))))

;;; markdown.el ends here
