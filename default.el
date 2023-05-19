;;; default.el --- Wesley's Emacs Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; URL: https://git.sr.ht/~wgn/emacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an Emacs configuration which is intended to be used in
;; conjunction with Nix. It contains template variables wrapped with @
;; which need to be substituted with real values using something
;; similar to the substituteAll function provided by nixpkgs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(let ((gc-cons-threshold most-positive-fixnum))
  (eval-when-compile
    (require 'use-package))

  (use-package no-littering
    :ensure t)

  ;; Revert buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode t)

  ;; Required for :bind in use-package.
  (use-package bind-key
    :ensure t)

  ;; Mode line settings.
  (line-number-mode t)
  (column-number-mode t)
  (size-indication-mode t)

  ;; Show line numbers at the beginning of each line.
  (global-display-line-numbers-mode +1)

  ;; Configure whitespace preferences.
  (use-package whitespace
    :hook
    ((text-mode prog-mode) . (lambda ()
                               (add-hook 'before-save-hook 'whitespace-cleanup nil t)
                               (whitespace-mode +1)))

    :config
    (setq whitespace-style '(face tabs empty trailing)))

  ;; Color scheme.
  (require-theme 'modus-themes)
  (modus-themes-load-theme 'modus-vivendi-tinted)

  ;; Syntax highlighting.
  (use-package treesit
    :config
    (setq treesit-font-lock-level 4))

  ;; Highlight TODO comments.
  (use-package hl-todo
    :ensure t

    :config
    (global-hl-todo-mode +1))

  ;; Enable y/n answers.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers.
  (setq enable-recursive-minibuffers t)

  (use-package vertico
    :ensure t

    :config
    (vertico-mode +1))

  ;; Use the `orderless' completion style.
  (use-package orderless
    :ensure t

    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles partial-completion))
                                     (eglot (styles orderless)))))

  ;; Provides search and navigation based on completing-read.
  (use-package consult
    :ensure t
    :demand t

    :bind
    ;; C-c bindings (mode-specific-map)
    (("C-c M-x" . consult-mode-command)
     ("C-c h" . consult-history)
     ("C-c k" . consult-kmacro)
     ("C-c m" . consult-man)
     ("C-c i" . consult-info)
     ([remap Info-search] . consult-info)
     ;; C-x bindings (ctl-x-map)
     ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
     ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
     ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
     ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
     ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
     ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
     ;; Custom M-# bindings for fast register access
     ("M-#" . consult-register-load)
     ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
     ("C-M-#" . consult-register)
     ;; Other custom bindings
     ("M-y" . consult-yank-pop)                ;; orig. yank-pop
     ;; M-g bindings (goto-map)
     ("M-g e" . consult-compile-error)
     ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
     ("M-g g" . consult-goto-line)             ;; orig. goto-line
     ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
     ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
     ("M-g m" . consult-mark)
     ("M-g k" . consult-global-mark)
     ("M-g i" . consult-imenu)
     ("M-g I" . consult-imenu-multi)
     ;; M-s bindings (search-map)
     ("M-s d" . consult-find)
     ("M-s D" . consult-locate)
     ("M-s g" . consult-grep)
     ("M-s G" . consult-git-grep)
     ("M-s r" . consult-ripgrep)
     ("M-s l" . consult-line)
     ("M-s L" . consult-line-multi)
     ("M-s k" . consult-keep-lines)
     ("M-s u" . consult-focus-lines)
     ;; Isearch integration
     ("M-s e" . consult-isearch-history)
     :map isearch-mode-map
     ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
     ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
     ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
     ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
     ;; Minibuffer history
     :map minibuffer-local-map
     ("M-s" . consult-history)                 ;; orig. next-matching-history-element
     ("M-r" . consult-history)))               ;; orig. previous-matching-history-element

  ;; Convenient jumping between windows.
  (use-package ace-window
    :ensure t

    :bind
    (([remap other-window] . ace-window)
     ("s-w" . ace-window)))

  (use-package perspective
    :ensure t

    :custom
    (persp-mode-prefix-key (kbd "C-c M-p"))

    :config
    (persp-mode)

    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  (use-package dired
    :custom
    (ls-lisp-use-insert-directory-program nil)

    :config
    (require 'ls-lisp))

  ;; More intuitive deletion
  (delete-selection-mode +1)

  (use-package easy-kill
    :ensure t

    :bind
    (([remap kill-ring-save] . easy-kill)
     ([remap mark-sexp] . easy-mark)))

  (use-package browse-kill-ring
    :ensure t

    :bind
    (("M-y" . browse-kill-ring)))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :custom
    (savehist-additional-variables '(search-ring regexp-search-ring))
    (savehist-autosave-interval 60)

    :config
    (savehist-mode +1))

  ;; Remember your location in a file when saving files.
  (use-package saveplace
    :config
    (save-place-mode +1))

  ;; Save recent files.
  (use-package recentf
    :custom
    (recentf-max-saved-items 500)
    (recentf-max-menu-items 15)
    (recentf-auto-cleanup 'never)

    :config
    (recentf-mode +1))

  ;; Supercharge your undo/redo with undo-tree.
  (use-package undo-tree
    :ensure t

    :custom
    (undo-tree-auto-save-history t)

    :config
    (global-undo-tree-mode +1))

  (use-package magit
    :ensure t

    :bind
    (("C-x g" . magit)))

  (setq indent-tabs-mode nil)   ;; Don't use tabs to indent,
  (setq tab-width 8)            ;; but maintain correct appearance.

  ;; Newline at end of file.
  (setq require-final-newline t)

  ;; Maintain balanced parens.
  (use-package elec-pair
    :hook (prog-mode . electric-pair-local-mode))

  (use-package flyspell
    :hook
    ((text-mode . flyspell-mode)
     (prog-mode . flyspell-prog-mode))

    :custom
    (ispell-program-name "@aspell@")
    (ispell-extra-args '("--sug-mode=ultra")))

  ;; More logical movement behavior
  (use-package mwim
    :ensure t

    :bind
    (("C-a" . mwim-beginning)
     ("C-e" . mwim-end)))

  ;; Convenient jumping to text.
  (use-package avy
    :ensure t

    :bind
    (("C-:" . avy-goto-char)
     ("s-," . avy-goto-char)
     ("C-'" . avy-goto-char-2)
     ("M-g f" . avy-goto-line)
     ("M-g w" . avy-goto-word-1)
     ("M-g e" . avy-goto-word-0)
     ("s-." . avy-goto-word-or-subword-1)
     ("C-c v" . avy-goto-word-or-subword-1)))

  (use-package corfu
    :ensure t
    :demand t

    :bind
    (:map corfu-map
          ("<escape>". corfu-quit)
          ("<return>" . corfu-insert)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous))

    :custom
    (tab-always-indent 'complete)
    (completion-cycle-threshold nil)      ; Always show candidates in menu
    (corfu-auto nil)
    (corfu-separator ?\s)
    (corfu-quit-no-match 'separator)
    (corfu-preview-current 'insert)       ; Preview current candidate?
    (corfu-preselect-first t)             ; Preselect first candidate?

    :init
    (global-corfu-mode +1)

    :config
    (corfu-popupinfo-mode +1))

  (use-package dabbrev
    :bind
    (("M-/" . dabbrev-completion)
     ("C-M-/" . dabbrev-expand)))

  (use-package kind-icon
    :ensure t

    :custom
    (kind-icon-default-face 'corfu-default)

    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (use-package corfu-terminal
    :ensure t

    :config
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))

  (use-package cape
    :ensure t

    :config
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; NOTE: The order matters!
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))

  (use-package tempel
    :ensure t

    :config
    ;; Setup completion at point
    (defun tempel-setup-capf ()
      ;; Add the Tempel Capf to `completion-at-point-functions'.
      ;; `tempel-expand' only triggers on exact matches. Alternatively use
      ;; `tempel-complete' if you want to see all matches, but then you
      ;; should also configure `tempel-trigger-prefix', such that Tempel
      ;; does not trigger too often when you don't expect it. NOTE: We add
      ;; `tempel-expand' *before* the main programming mode Capf, such
      ;; that it will be tried first.
      (setq completion-at-point-functions
            (cons #'tempel-expand
                  completion-at-point-functions)))

    (add-hook 'prog-mode-hook 'tempel-setup-capf)
    (add-hook 'text-mode-hook 'tempel-setup-capf)

    ;; Make the Tempel templates available to Abbrev, either locally or
    ;; globally. `expand-abbrev' is bound to C-x '.
    (global-tempel-abbrev-mode +1))

  (use-package wgrep
    :ensure t

    :bind
    (:map grep-mode-map
          ("C-c C-p" . wgrep-change-to-wgrep-mode)))

  (use-package selected
    :ensure t

    :config
    (selected-global-mode +1))

  (use-package multiple-cursors
    :ensure t

    :bind
    (:map selected-keymap
          ("C-x c" . mc/edit-lines)))

  (use-package visual-regexp
    :ensure t

    :bind
    (("C-M-%" . vr/query-replace)))

  (use-package eshell
    :bind
    (("C-x m" . eshell)
     ("C-x M" . (lambda () (interactive) (eshell t)))))

  (use-package eglot
    :commands
    (eglot
     eglot-ensure)

    :bind
    (:map eglot-mode-map
          ("C-c C-l ." . xref-find-definitions)
          ("C-c C-l ?" . xref-find-references)
          ("C-c C-l r" . eglot-rename)
          ("C-c C-l i" . eglot-find-implementation)
          ("C-c C-l d" . eldoc)
          ("C-c C-l e" . eglot-code-actions))

    :config
    (defun eglot-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-super-capf
                         #'eglot-completion-at-point
                         #'tempel-expand
                         #'cape-dabbrev
                         #'cape-file))))
    (add-hook 'eglot-managed-mode-hook #'eglot-capf)

    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (use-package chatgpt-shell
    :ensure t

    :commands
    (chatgpt-shell)

    :config
    (setq chatgpt-shell-openai-key (lambda ()
                                     (nth 0 (process-lines "@pass@" "show" "openai-key")))))

  ;; Displays available keybindings in a pop-up.
  (use-package which-key
    :ensure t

    :config
    (which-key-mode +1))

  ;; Enable rich annotations using the Marginalia package.
  (use-package marginalia
    :ensure t

    :config
    (marginalia-mode +1))

  (use-package helpful
    :ensure t

    :bind
    (("C-h f" . helpful-callable)
     ("C-h v" . helpful-variable)
     ("C-h k" . helpful-key)
     ("C-h x" . helpful-command)
     ("C-c C-d" . helpful-at-point)))

  (use-package discover-my-major
    :ensure t

    :bind
    (("C-h M-m" . discover-my-major)
     ("C-h M-S-M" . discover-my-mode)))

  ;; Send HTTP requests from emacs.
  (use-package restclient
    :ensure t
    :mode "\\.http\\'")

  ;; Golang support.
  (use-package go-mode
    :ensure t
    :mode "\\.go\\'"

    :custom
    (gofmt-command "@goimports@")

    :init
    ;; Open go files with tree-sitter support.
    (add-to-list 'major-mode-remap-alist
                 '(go-mode . go-ts-mode))

    ;; Set up syntax highlighting for go-ts-mode.
    (defun go-ts-mode-highlighting-setup ()
      (whitespace-toggle-options '(tabs))
      (treesit-font-lock-recompute-features))
    (add-hook 'go-ts-mode-hook #'go-ts-mode-highlighting-setup)

    ;; Set up eglot for go-ts-mode.
    (defun go-ts-mode-eglot-setup ()
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     '((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode) . ("@gopls@")))
        (add-hook 'before-save-hook #'eglot-format-buffer t t))
      (eglot-ensure))
    (add-hook 'go-ts-mode-hook #'go-ts-mode-eglot-setup)

    :config
    ;; CamelCase aware editing operations.
    (subword-mode +1)

    :bind
    (:map go-mode-map
          ("C-c a" . go-test-current-project)
          ("C-c m" . go-test-current-file)
          ("C-c ." . go-test-current-test)
          ("C-c b" . go-run)
          ("C-h f" . godoc-at-point)))

  ;; Python support.
  (use-package python
    :init
    ;; Open python files with tree-sitter support.
    (add-to-list 'major-mode-remap-alist
                 '(python-mode . python-ts-mode))

    ;; Set up syntax highlighting for python-ts-mode.
    (defun python-ts-mode-highlighting-setup ()
      (treesit-font-lock-recompute-features))
    (add-hook 'python-ts-mode-hook #'python-ts-mode-highlighting-setup)

    ;; Set up eglot for python-ts-mode.
    (defun python-ts-mode-eglot-setup ()
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     '((python-mode python-ts-mode) . ("@pylsp@")))
        (add-hook 'before-save-hook #'eglot-format-buffer t t))
      (eglot-ensure))
    (add-hook 'python-ts-mode-hook #'python-ts-mode-eglot-setup))

  ;; Nix support.
  (use-package nix-mode
    :ensure t
    :mode "\\.nix\\'")

  ;; YAML support.
  (use-package yaml-mode
    :ensure t

    :mode
    (("\\.yaml\\'" . yaml-mode)
     ("\\.yml\\'" . yaml-mode)))

  ;; Send HTTP requests from org-mode.
  (use-package ob-restclient
    :ensure t

    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))))

(provide 'init)
;;; init.el ends here
