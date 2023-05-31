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

  ;; Avoid putting files in weird places.
  (use-package no-littering
    :ensure t

    :config
    (no-littering-theme-backups))

  ;; Keybindings.
  ;; Required for :bind in use-package.
  (use-package bind-key
    :ensure t)

  ;; Whitespace preferences.
  (use-package whitespace
    :hook
    ((text-mode prog-mode) . (lambda ()
                               (add-hook 'before-save-hook 'whitespace-cleanup nil t)
                               (whitespace-mode +1)))

    :custom
    (whitespace-style '(face tabs empty trailing)))

  ;; Syntax highlighting.
  (use-package treesit
    :custom
    (treesit-font-lock-level 4))

  ;; Highlight TODO comments.
  (use-package hl-todo
    :ensure t

    :config
    (global-hl-todo-mode +1))

  ;; Completion engine based on `completing-read'.
  (use-package vertico
    :ensure t

    :config
    (vertico-mode +1))

  ;; Completion style that allows for multiple regular expressions.
  (use-package orderless
    :ensure t

    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles partial-completion))
                                     (eglot (styles orderless)))))

  ;; Provides search and navigation based on `completing-read'.
  (use-package consult
    :ensure t
    :demand t

    :bind
    (;; C-c bindings in `mode-specific-map'
     ("C-c M-x" . consult-mode-command)
     ("C-c h" . consult-history)
     ("C-c k" . consult-kmacro)
     ("C-c m" . consult-man)
     ("C-c i" . consult-info)
     ([remap Info-search] . consult-info)
     ;; C-x bindings in `ctl-x-map'
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
     ;; M-g bindings in `goto-map'
     ("M-g e" . consult-compile-error)
     ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
     ("M-g g" . consult-goto-line)             ;; orig. goto-line
     ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
     ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
     ("M-g m" . consult-mark)
     ("M-g k" . consult-global-mark)
     ("M-g i" . consult-imenu)
     ("M-g I" . consult-imenu-multi)
     ;; M-s bindings in `search-map'
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
     ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    :custom
    (xref-show-xrefs-function #'consult-xref)
    (xref-show-definitions-function #'consult-xref))

  ;; Move between windows.
  (use-package window
    :bind
    (("M-o" . other-window)))

  ;; Divide buffers and window configurations into workspaces.
  (use-package perspective
    :ensure t

    :custom
    (persp-mode-prefix-key (kbd "C-c M-p"))

    :config
    (persp-mode)

    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  ;; Open directory as buffer.
  (use-package dired
    :custom
    (ls-lisp-use-insert-directory-program nil)

    :config
    (require 'ls-lisp))

  ;; Easily mark and kill regions.
  (use-package easy-kill
    :ensure t

    :bind
    (([remap kill-ring-save] . easy-kill)
     ([remap mark-sexp] . easy-mark)))

  ;; Browse through killed regions.
  (use-package browse-kill-ring
    :ensure t

    :bind
    (("M-y" . browse-kill-ring)))

  ;; Persist history over Emacs restarts.
  (use-package savehist
    :custom
    (savehist-additional-variables '(search-ring regexp-search-ring))
    (savehist-autosave-interval 60)

    :config
    (savehist-mode +1))

  ;; Remember your location in a file.
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

  ;; Undo history as a tree.
  (use-package undo-tree
    :ensure t

    :custom
    (undo-tree-auto-save-history t)

    :config
    (global-undo-tree-mode +1))

  ;; Git interface.
  (use-package magit
    :ensure t

    :bind
    (("C-x g" . magit)))

  ;; Maintain balanced parentheses.
  (use-package elec-pair
    :hook
    (prog-mode . electric-pair-local-mode))

  ;; Spell checxing and correction.
  (use-package spell-fu
    :ensure t

    :custom
    (ispell-program-name "@aspell@")

    :config
    (spell-fu-global-mode))

  ;; More convenient options for cursor movement.
  (use-package mwim
    :ensure t

    :bind
    (("C-a" . mwim-beginning)
     ("C-e" . mwim-end)))

  ;; In-buffer completion with `completion-in-region'.
  (use-package corfu
    :ensure t

    :bind
    (:map corfu-map
          ("<escape>". corfu-quit)
          ("<return>" . corfu-insert)
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ("SPC" . corfu-insert-separator))

    :custom
    (tab-always-indent 'complete)         ; Show completions with tab key.
    (completion-cycle-threshold nil)      ; Always show candidates in menu.
    (corfu-auto nil)                      ; Don't auto-complete.
    (corfu-preview-current 'insert)       ; Preview current candidate.
    (corfu-preselect-first t)             ; Pre-select first candidate.

    :init
    (global-corfu-mode +1)

    :config
    (defun corfu-enable-in-minibuffer ()
      "Enable corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        (setq-local corfu-echo-delay nil
                    corfu-popupinfo-delay nil)
        (corfu-mode +1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

    (defun corfu-move-to-minibuffer ()
      "Transfer corfu completions to the minibuffer."
      (interactive)
      (when completion-in-region--data
        (let ((completion-extra-properties corfu--extra)
              completion-cycle-threshold completion-cycling)
          (apply #'consult-completion-in-region completion-in-region--data))))
    (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

    ;; Show docs in popup.
    ;; NOTE: Does not currently work with terminal Emacs.
    ;;       https://github.com/minad/corfu/issues/248
    (corfu-popupinfo-mode +1))

  ;; Show icons in corfu completion popup.
  (use-package kind-icon
    :ensure t

    :custom
    (kind-icon-default-face 'corfu-default)

    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Allows corfu completion popup to work in terminal Emacs.
  (use-package corfu-terminal
    :ensure t

    :config
    (unless (display-graphic-p)
      (corfu-terminal-mode +1)))

  ;; Extra `completion-at-point-functions'.
  (use-package cape
    :ensure t

    :config
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; NOTE: The order matters!
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block)
    (add-to-list 'completion-at-point-functions #'cape-history)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-sgml)
    (add-to-list 'completion-at-point-functions #'cape-rfc1345)
    (add-to-list 'completion-at-point-functions #'cape-abbrev)
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    (add-to-list 'completion-at-point-functions #'cape-line))

  ;; Make templates available via `completion-at-point-functions'.
  (use-package tempel
    :ensure t

    :hook
    ((prog-mode text-mode) . (lambda ()
                               (setq-local completion-at-point-functions
                                           (cons #'tempel-expand
                                                 completion-at-point-functions)))))

  ;; Pre-made templates for tempel.
  (use-package tempel-collection
    :ensure t)

  ;; Contextual actions based on what is near point.
  (use-package embark
    :ensure t

    :bind
    (("C-." . embark-act)
     ("M-." . embark-dwim)        ; orig. `xref-find-definitions'.
     ("C-h B" . embark-bindings)) ; orig. `describe-bindings'.

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  ;; Integration between Embark and Consult.
  (use-package embark-consult
    :ensure t

    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  ;; Editable grep buffers.
  (use-package wgrep
    :ensure t

    :bind
    (:map grep-mode-map
          ("C-c C-p" . wgrep-change-to-wgrep-mode)))

  ;; Creates a minor mode for when the point is in a selection.
  (use-package selected
    :ensure t

    :config
    (selected-global-mode +1))

  ;; Edit text with multiple cursors.
  (use-package multiple-cursors
    :ensure t

    :bind
    (:map selected-keymap
          ("C-x c" . mc/edit-lines)))

  ;; Visual `query-replace' with regular expressions.
  (use-package visual-regexp
    :ensure t

    :bind
    (("C-M-%" . vr/query-replace)))

  ;; Shell written in Emacs Lisp.
  (use-package eshell
    :bind
    (("C-x m" . eshell)
     ("C-x M" . (lambda () (interactive) (eshell t)))))

  ;; Language server integration.
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
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  ;; Interface for talking to ChatGPT.
  (use-package chatgpt-shell
    :ensure t

    :commands
    (chatgpt-shell))

  ;; Displays available keybindings in a pop-up.
  (use-package which-key
    :ensure t

    :config
    (which-key-mode +1))

  ;; Rich annotations in the minibuffer completion.
  (use-package marginalia
    :ensure t

    :config
    (marginalia-mode +1))

  ;; More helpful documentation for Emacs Lisp.
  (use-package helpful
    :ensure t

    :bind
    (("C-h f" . helpful-callable)
     ("C-h v" . helpful-variable)
     ("C-h k" . helpful-key)
     ("C-h x" . helpful-command)
     ("C-c C-d" . helpful-at-point)))

  ;; Send HTTP requests from Emacs.
  (use-package restclient
    :ensure t
    :mode "\\.http\\'")

  ;; Basic Golang support.
  ;;
  ;; HACK: This is built-in to Emacs, but there is also a third-party
  ;; go-mode which we will install next.
  (use-package go-ts-mode
    :mode
    (("\\.go\\'" . go-ts-mode)
     ("go\\.mod\\'" . go-mod-ts-mode))

    :init
    ;; Set up syntax highlighting for go-ts-mode.
    (defun go-ts-mode-highlighting-setup ()
      (whitespace-toggle-options '(tabs))
      (treesit-font-lock-recompute-features))
    (add-hook 'go-ts-mode-hook #'go-ts-mode-highlighting-setup)
    (add-hook 'go-mod-ts-mode-hook #'go-ts-mode-highlighting-setup)

    ;; Set up eglot for go-ts-mode.
    (defun go-ts-mode-eglot-setup ()
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     '((go-ts-mode go-mod-ts-mode) . ("@gopls@")))
        (add-hook 'before-save-hook #'eglot-format-buffer t t))
      (eglot-ensure))
    (add-hook 'go-ts-mode-hook #'go-ts-mode-eglot-setup)

    :config
    ;; CamelCase aware editing operations.
    (subword-mode +1))

  ;; Extra Golang support.
  ;;
  ;; HACK: We won't actually use this go-mode other than for importing
  ;; some extra functions to augment the built-in go-ts-mode.
  (use-package go-mode
    :ensure t

    :mode
    ;; HACK: The built-in go-ts-mode does not support go.work files,
    ;; so we actually do use the third-party go-mode for this file
    ;; type.
    ("go\\.work\\'" . go-dot-work-mode)

    :hook
    (go-dot-work-mode . (lambda ()
                          (require 'go-ts-mode)))
    (go-ts-mode . (lambda ()
                    (require 'go-mode)
                    (defun go-ts-mode-run-gofmt ()
                      "Use the `gofmt' function from go-mode inside go-ts-mode."
                      (interactive)
                      (gofmt))
                    (add-hook 'before-save-hook #'go-ts-mode-run-gofmt t t)))

    :custom
    (gofmt-command "@gofumpt@")

    :bind
    (:map go-ts-mode-map
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
    :mode "\\.nix\\'"

    :init
    ;; Set up eglot for nix-mode.
    (defun nix-mode-eglot-setup ()
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     '(nix-mode . ("@nil@")))
        (add-hook 'before-save-hook #'eglot-format-buffer t t))
      (eglot-ensure))
    (add-hook 'nix-mode-hook #'nix-mode-eglot-setup))

  ;; YAML support.
  (use-package yaml-ts-mode
    :mode
    (("\\.yaml\\'" . yaml-ts-mode)
     ("\\.yml\\'" . yaml-ts-mode))

    :init
    ;; Set up eglot for yaml-mode.
    (defun yaml-ts-mode-eglot-setup ()
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     '(yaml-ts-mode . ("@yamlls@" "--stdio")))
        (add-hook 'before-save-hook #'eglot-format-buffer t t)
        (setq-local eglot-workspace-configuration
                    '(:yaml (:format (:enable t
                                      :singleQuote t)
                             :validate t
                             :keyOrdering :json-false
                             :hover t
                             :completion t
                             :suggest (:parentSkeletonSelectedFirst t)
                             :schemaStore (:enable t
                                           :url "https://www.schemastore.org/api/json/catalog.json")))))
      (eglot-ensure))
    (add-hook 'yaml-ts-mode-hook #'yaml-ts-mode-eglot-setup))

  ;; Send HTTP requests from org-mode.
  (use-package ob-restclient
    :ensure t

    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t))))

  ;; Markdown support.
  (use-package markdown-mode
    :ensure t

    :mode
    (("README\\.md\\'" . gfm-mode)
     ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

    :bind
    (:map markdown-mode-map
          ("C-c C-e" . markdown-do))

    :custom
    (markdown-command "@multimarkdown@"))

  ;; Generate table of contents in markdown files.
  (use-package markdown-toc
    :ensure t

    :hook
    ((markdown-mode gfm-mode) . markdown-toc-mode))

  ;; Don't assume double spaces at the end of a sentence.
  (setq-default sentence-end-double-space nil)

  ;; Show line numbers at the beginning of each line.
  (global-display-line-numbers-mode +1)

  ;; Revert buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode +1)

  ;; Color scheme.
  (require-theme 'modus-themes)
  (modus-themes-load-theme 'modus-vivendi-tinted)

  ;; Enable y/n answers.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Allow minibuffer commands while in the minibuffer.
  (setq-default enable-recursive-minibuffers t)

  ;; More intuitive deletion.
  (delete-selection-mode +1)

  ;; Place newline at end of file.
  (setq-default require-final-newline t)

  ;; Don't use tabs to indent.
  (setq-default indent-tabs-mode nil)

  ;; Maintain correct appearance of tabs.
  (setq-default tab-width 8)

  ;; Hide commands in M-x which do not work in the current mode.
  (setq-default read-extended-command-predicate #'command-completion-default-include-p)

  ;; Mode line settings.
  (line-number-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1))

(provide 'init)
;;; init.el ends here
