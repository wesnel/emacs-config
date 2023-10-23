;;; default.el --- Wesley's Emacs Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Wesley Nelson

;; Author: Wesley Nelson <wgn@wesnel.dev>
;; Maintainer: Wesley Nelson <wgn@wesnel.dev>
;; URL: https://git.sr.ht/~wgn/emacs-config

;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is an Emacs configuration which is intended to be used in
;; conjunction with Nix.  It contains template variables wrapped with
;; @ which need to be substituted with real values using something
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

(eval-when-compile
  (require 'use-package))

;; Miscellaneous Emacs configuration.
(use-package emacs
  :custom
  ;; Disable package archives.
  (package-archives nil)
  (use-short-answers t)
  (initial-buffer-choice t)
  ;; Place newline at end of file.
  (require-final-newline t)
  ;; Don't use tabs to indent.
  (indent-tabs-mode nil)
  ;; Maintain correct appearance of tabs.
  (tab-width 8)
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Configure the time display on the mode line.
  (display-time-day-and-date t)
  (display-time-24hr-format t)
  (display-time-use-mail-icon t)

  :init
  ;; Remove some UI elements.
  (menu-bar-no-scroll-bar)
  (menu-bar-no-window-divider)

  ;; Show line numbers at the beginning of each line.
  (global-display-line-numbers-mode +1)

  ;; Revert buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode +1)

  ;; More intuitive deletion.
  (delete-selection-mode +1)

  ;; Mode line settings.
  (line-number-mode +1)
  (column-number-mode +1)
  (size-indication-mode +1)
  (display-time))

;; Use user shell $PATH.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :demand t

    :commands
    (exec-path-from-shell-initialize)

    :defines
    (exec-path-from-shell-variables)

    :config
    (dolist (var '("SSH_AUTH_SOCK"
                   "SSH_AGENT_PID"
                   "GPG_AGENT_INFO"
                   "GPG_TTY"
                   "LANG"
                   "LC_CTYPE"
                   "NIX_SSL_CERT_FILE"
                   "NIX_PATH"
                   "PASSWORD_STORE_DIR"
                   "PASSWORD_STORE_KEY"
                   "PASSWORD_STORE_SIGNING_KEY"))
          (add-to-list 'exec-path-from-shell-variables var)

        (exec-path-from-shell-initialize))))

;; Store secrets in Emacs.
(use-package auth-source
  :custom
  (auth-source-pass-filename (getenv "PASSWORD_STORE_DIR"))

  :init
  (auth-source-pass-enable))

;; Avoid putting files in weird places.
(use-package no-littering
  :ensure t
  :demand t

  :functions
  (no-littering-theme-backups)

  :config
  (no-littering-theme-backups))

;; Required for :bind in use-package.
(use-package bind-key
  :ensure t)

;; Required for :diminish in use-package.
(use-package diminish
  :ensure t)

;; Whitespace preferences.
(use-package whitespace
  :diminish whitespace-mode

  :preface
  (defun wgn/set-up-whitespace ()
    (add-hook 'before-save-hook #'whitespace-cleanup nil t)
    (whitespace-toggle-options '(lines newline-mark)))

  :hook
  ((text-mode prog-mode) . wgn/set-up-whitespace))

;; Support for CamelCase.
(use-package subword
  :diminish subword-mode)

;; Navigate code based on an outline.
(use-package outline
  :diminish outline-minor-mode

  :hook
  ((text-mode prog-mode) . outline-minor-mode))

;; Syntax highlighting.
(use-package treesit
  :commands
  (treesit-font-lock-recompute-features)

  :custom
  (treesit-font-lock-level 4))

;; Highlight TODO comments.
(use-package hl-todo
  :ensure t

  :commands
  (global-hl-todo-mode)

  :init
  (global-hl-todo-mode +1))

;; Completion engine based on `completing-read'.
(use-package vertico
  :ensure t

  :commands
  (vertico-mode)

  :init
  (vertico-mode +1))

;; Completion style that allows for multiple regular expressions.
(use-package orderless
  :ensure t

  :custom
  ;; Allow minibuffer commands while in the minibuffer.
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                   (bookmark (styles . (basic substring)))
                                   (library (styles . (basic substring)))
                                   (embark-keybinding (styles . (basic substring)))
                                   (imenu (styles . (basic substring orderless)))
                                   (consult-location (styles . (basic substring orderless)))
                                   (kill-ring (styles . (emacs22 orderless)))
                                   (eglot (styles . (emacs22 substring orderless))))))

;; Provides search and navigation based on `completing-read'.
(use-package consult
  :ensure t

  :defines
  (consult-preview-key)

  :commands
  (consult-find
   consult-locate
   consult-grep
   consult-git-grep
   consult-ripgrep
   consult-line
   consult-line-multi
   consult-keep-lines
   consult-focus-lines
   consult-isearch-history
   consult-outline
   consult-mark
   consult-global-mark
   consult-imenu
   consult-imenu-multi
   consult-complex-command
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   consult-bookmark
   consult-project-buffer
   consult-register-load
   consult-register-store
   consult-register
   consult-register-window
   consult-yank-pop
   consult-compile-error
   consult-flymake
   consult-goto-line
   consult-mode-command
   consult-history
   consult-kmacro
   consult-man)

  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . #'consult-mode-command)
   ("C-c h" . #'consult-history)
   ("C-c k" . #'consult-kmacro)
   ("C-c m" . #'consult-man)
   ("C-c i" . #'consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . #'consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . #'consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . #'consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . #'consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . #'consult-register-load)
   ("M-'" . #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . #'consult-register)
   ;; Other custom bindings
   ("M-y" . #'consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . #'consult-compile-error)
   ("M-g f" . #'consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . #'consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . #'consult-goto-line)           ;; orig. goto-line
   ("M-g o" . #'consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . #'consult-mark)
   ("M-g k" . #'consult-global-mark)
   ("M-g i" . #'consult-imenu)
   ("M-g I" . #'consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . #'consult-find)
   ("M-s D" . #'consult-locate)
   ("M-s g" . #'consult-grep)
   ("M-s G" . #'consult-git-grep)
   ("M-s r" . #'consult-ripgrep)
   ("M-s l" . #'consult-line)
   ("M-s L" . #'consult-line-multi)
   ("M-s k" . #'consult-keep-lines)
   ("M-s u" . #'consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . #'consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . #'consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . #'consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . #'consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . #'consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . #'consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . #'consult-history))                ;; orig. previous-matching-history-element

  :custom
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")

  :init
  (advice-add #'register-preview :override #'consult-register-window))

;; Move between windows.
(use-package window
  :bind
  (("M-o" . #'other-window)))

;; Open directory as buffer.
(use-package dired
  :custom
  (ls-lisp-use-insert-directory-program nil)

  :preface
  (defun wgn/fix-dired-ls ()
    (require 'ls-lisp))

  :init
  (add-hook 'dired-mode-hook #'wgn/fix-dired-ls))

;; Easily mark and kill regions.
(use-package easy-kill
  :ensure t

  :commands
  (easy-kill
   easy-mark)

  :bind
  (([remap kill-ring-save] . #'easy-kill)
   ([remap mark-sexp] . #'easy-mark)))

;; Persist history over Emacs restarts.
(use-package savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)

  :init
  (savehist-mode +1))

;; Remember your location in a file.
(use-package saveplace
  :init
  (save-place-mode +1))

;; Save recent files.
(use-package recentf
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)

  :init
  (recentf-mode +1))

;; Undo history as a tree.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode

  :custom
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)

  :commands
  (global-undo-tree-mode)

  :custom
  (undo-tree-auto-save-history t)

  :init
  (global-undo-tree-mode +1))

;; Project management.
(use-package project
  :commands
  (project-switch-commands
   project-prefixed-buffer-name
   project-remember-project))

;; Git interface.
(use-package magit
  :ensure t

  :preface
  (defun wgn/add-clone-to-project-list ()
    (project-remember-project (project-current)))

  :commands
  (magit
   magit-project-status)

  :defines
  (magit-post-clone-hook)

  :bind
  (("C-x g" . #'magit))

  :init
  (with-eval-after-load 'project
    ;; Add newly-cloned repositories to the project list.
    (add-hook 'magit-post-clone-hook #'wgn/add-clone-to-project-list)

    ;; Add the ability to open magit in a project.
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))

;; Automatically manage parentheses in lisps.
(use-package parinfer-rust-mode
  :ensure t
  :hook lisp-data-mode

  :custom
  (parinfer-rust-library "@parinfer@"))

;; Show matching parentheses.
(use-package paren
  :custom
  (show-paren-context-when-offscreen t))

;; Focused presentation mode built on top of outline.
(use-package logos
  :ensure t

  :commands
  (logos-narrow-dwim
   logos-forward-page-dwim
   logos-backward-page-dwim
   logos-focus-mode)

  :custom
  (logos-outlines-are-pages t)

  :bind
  (([remap narrow-to-region] . #'logos-narrow-dwim)
   ([remap forward-page] . #'logos-forward-page-dwim)
   ([remap backward-page] . #'logos-backward-page-dwim)
   ("<f9>" . #'logos-focus-mode)))

;; Spell checxing and correction.
(if (executable-find "enchant-2")

    ;; HACK: enchant and jinx are difficult a difficult combination to
    ;;       install with nix, so we instead check to see if they are
    ;;       available on the system elsehow.
    (use-package jinx
      :ensure t

      :custom
      (ispell-program-name "enchant-2")

      :commands
      (jinx-correct
       jinx-languages)

      :bind
      (("M-$" . #'jinx-correct)
       ("C-M-$" . #'jinx-languages))

      :hook
      (emacs-startup . global-jinx-mode))

  ;; HACK: If enchant is not available on the system, then fall back
  ;;       to aspell and spell-fu. This combination is easier to
  ;;       install with nix.
  (use-package spell-fu
    :ensure t

    :commands
    (spell-fu-global-mode)

    :custom
    (ispell-program-name "@aspell@")

    :init
    (spell-fu-global-mode)))

;; Avoid need for modifier keys.
(use-package devil
  :ensure t

  :commands
  (global-devil-mode)

  :init
  (global-devil-mode +1))

;; More convenient options for cursor movement.
(use-package mwim
  :ensure t

  :commands
  (mwim-beginning
   mwim-end)

  :bind
  (([remap move-beginning-of-line] . #'mwim-beginning)
   ([remap move-end-of-line] . #'mwim-end)))

(when (display-graphic-p)
  (use-package doc-view
    :init
    (add-to-list 'exec-path "@gs@")
    (add-to-list 'exec-path "@mupdf@")))

;; In-buffer completion with `completion-in-region'.
(use-package corfu
  :ensure t

  :defines
  (corfu-continue-commands
   corfu-map)

  :commands
  (corfu-mode
   global-corfu-mode
   corfu-insert-separator
   corfu-popupinfo-mode)

  :preface
  (defun corfu-enable-in-minibuffer ()
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode +1)))

  :bind
  (:map corfu-map
        ("SPC" . #'corfu-insert-separator))

  :custom
  (tab-always-indent 'complete)         ; Show completions with tab key.
  (completion-cycle-threshold nil)      ; Always show candidates in menu.
  (corfu-auto nil)                      ; Don't auto-complete.
  (corfu-preview-current 'insert)       ; Preview current candidate.
  (corfu-preselect-first t)             ; Pre-select first candidate.

  :init
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  ;; Show docs in popup.
  ;; NOTE: Does not currently work with terminal Emacs.
  ;;       https://github.com/minad/corfu/issues/248
  (corfu-popupinfo-mode +1)
  (global-corfu-mode +1))

;; Allows corfu completion popup to work in terminal Emacs.
(unless (display-graphic-p)
  (use-package corfu-terminal
    :ensure t

    :commands
    (corfu-terminal-mode)

    :init
    (corfu-terminal-mode +1)))

;; Extra `completion-at-point-functions'.
(use-package cape
  :ensure t

  :commands
  (cape-dabbrev
   cape-file
   cape-elisp-block
   cape-history
   cape-keyword
   cape-tex
   cape-sgml
   cape-rfc1345
   cape-abbrev
   cape-dict
   cape-symbol
   cape-line)

  :functions
  (cape-wrap-buster)

  :init
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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

  :commands
  (tempel-expand)

  :preface
  (defun wgn/add-tempel-to-completion-at-point ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))

  :hook
  ((prog-mode text-mode) . wgn/add-tempel-to-completion-at-point))

;; Pre-made templates for tempel.
(use-package tempel-collection
  :ensure t)

;; Contextual actions based on what is near point.
(use-package embark
  :ensure t

  :preface
  (defun wgn/embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun wgn/embark-hide-which-key-indicator (fn &rest args)
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'wgn/embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  :functions
  (embark--truncate-target
   embark-completing-read-prompter)

  :commands
  (embark-act
   embark-dwim
   embark-bindings)

  :defines
  (embark-indicators)

  :custom
  (embark-indicators '(wgn/embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :init
  (advice-add #'embark-completing-read-prompter
              :around #'wgn/embark-hide-which-key-indicator)

  :bind
  (("C-." . #'embark-act)
   ("M-." . #'embark-dwim)         ; orig. `xref-find-definitions'.
   ("C-h B" . #'embark-bindings))) ; orig. `describe-bindings'.

;; Integration between Embark and Consult.
(use-package embark-consult
  :ensure t

  :hook
  ((embark-collect-mode completion-list-mode) . consult-preview-at-point-mode))

(use-package grep
  :defines
  (grep-mode-map))

;; Editable grep buffers.
(use-package wgrep
  :ensure t

  :commands
  (wgrep-change-to-wgrep-mode)

  :bind
  (:map grep-mode-map
   ("C-c C-p" . #'wgrep-change-to-wgrep-mode)))

;; Creates a minor mode for when the point is in a selection.
(use-package selected
  :ensure t
  :diminish selected-minor-mode

  :commands
  (selected-global-mode)

  :defines
  (selected-keymap)

  :init
  (selected-global-mode +1))

;; Edit text with multiple cursors.
(use-package multiple-cursors
  :ensure t

  :commands
  (mc/mark-all-like-this)

  :bind
  (:map selected-keymap
   ("C-x c" . #'mc/mark-all-like-this)))

;; Structured editing and navigation based on tree-sitter.
(use-package combobulate
  :ensure t

  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;; Visual `query-replace' with regular expressions.
(use-package visual-regexp
  :ensure t

  :bind
  (("C-M-%" . vr/query-replace)))

;; Shell written in Emacs Lisp.
(use-package eshell
  :preface
  (defun wgn/disable-line-numbers ()
    (display-line-numbers-mode -1))

  :init
  (add-hook 'eshell-mode-hook #'wgn/disable-line-numbers))

;; Terminal emulator.
(use-package vterm
  :ensure t

  :commands
  (vterm
   vterm-other-window)

  :preface
  (defun wgn/project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer)
        (vterm t))))

  :init
  (add-hook 'vterm-mode-hook #'wgn/disable-line-numbers)

  (with-eval-after-load 'project
    (define-key project-prefix-map "t" #'wgn/project-vterm)
    (add-to-list 'project-switch-commands '(wgn/project-vterm "Vterm") t)))

;; Error checking.
(use-package flymake
  :hook
  (prog-mode . flymake-mode))

;; Language server integration.
(use-package eglot
  :preface
  (defun wgn/apply-eglot-format ()
    (unless (or (and (fboundp 'magit-rebase-in-progress-p)
                     (magit-rebase-in-progress-p))
                (and (fboundp 'magit-merge-in-progress-p)
                     (magit-merge-in-progress-p)))
      (eglot-format-buffer)))

  :defines
  (eglot-server-programs
   eglot-workspace-configuration)

  :commands
  (eglot
   eglot-ensure
   eglot-rename
   eglot-find-implementation
   eglot-code-actions
   eglot-format-buffer)

  :bind
  (:map eglot-mode-map
   ("C-c C-l ." . #'xref-find-definitions)
   ("C-c C-l ?" . #'xref-find-references)
   ("C-c C-l r" . #'eglot-rename)
   ("C-c C-l i" . #'eglot-find-implementation)
   ("C-c C-l d" . #'eldoc)
   ("C-c C-l e" . #'eglot-code-actions)))

;; Integration between eglot and consult.
(use-package consult-eglot
  :ensure t)

;; Interface for talking to ChatGPT.
;;
;; NOTE: You need to store your OpenAI credentials under auth-source.
(use-package chatgpt-shell
  :ensure t

  :commands
  (chatgpt-shell)

  :preface
  (defun wgn/get-openai-key ()
    (let* ((default (notmuch-user-primary-email))
           (user (completing-read (format "Choose user (default %s):" default)
                                  (notmuch-user-emails)
                                  nil
                                  nil
                                  nil
                                  nil
                                  default)))
      (setq-local chatgpt-shell-openai-key (auth-source-pick-first-password
                                            :host "openai.com"
                                            :user user))))

  :init
  (add-hook 'chatgpt-shell-mode-hook #'wgn/get-openai-key))

;; Displays available keybindings in a pop-up.
(use-package which-key
  :ensure t
  :diminish which-key-mode

  :functions
  (which-key--show-keymap
   which-key--hide-popup-ignore-command)

  :commands
  (which-key-mode
   which-key-enable-devil-support)

  :init
  (which-key-mode +1)
  (which-key-enable-devil-support))

;; Rich annotations in the minibuffer completion.
(use-package marginalia
  :ensure t

  :commands
  (marginalia-mode)

  :init
  (marginalia-mode +1))

;; More helpful documentation for Emacs Lisp.
(use-package helpful
  :ensure t

  :commands
  (helpful-callable
   helpful-variable
   helpful-key
   helpful-command
   helpful-at-point)

  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-h x" . #'helpful-command)
   ("C-c C-d" . #'helpful-at-point)))

;; Open code from Emacs in the web browser.
(use-package elsewhere
  :ensure t

  :commands
  (elsewhere-open
   elsewhere-build-url))

;; Send HTTP requests from Emacs.
(use-package restclient
  :ensure t
  :mode "\\.http\\'")

;; Basic Golang support.
;;
;; HACK: This is built-in to Emacs, but there is also a third-party
;;       go-mode which we will install next.
(use-package go-ts-mode
  :mode
  ("go\\.mod\\'" . go-mod-ts-mode)

  :preface
  (defun wgn/go-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((go-ts-mode go-mod-ts-mode) . ("@gopls@")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Open go files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(go-mode . go-ts-mode))

  ;; Set up eglot for go-ts-mode.
  (add-hook 'go-ts-mode-hook #'wgn/go-ts-mode-eglot-setup)

  ;; CamelCase aware editing operations.
  (add-hook 'go-ts-mode-hook #'subword-mode))

;; Extra Golang support.
;;
;; HACK: We won't actually use this go-mode other than for importing
;;       some extra functions to augment the built-in go-ts-mode.
(use-package go-mode
  :ensure t

  :commands
  (gofmt)

  :preface
  (defun wgn/apply-gofmt ()
    (unless (or (and (fboundp 'magit-rebase-in-progress-p)
                     (magit-rebase-in-progress-p))
                (and (fboundp 'magit-merge-in-progress-p)
                     (magit-merge-in-progress-p)))
      (gofmt)))

  :mode
  ;; HACK: The built-in go-ts-mode does not support go.work files, so
  ;;       we actually do use the third-party go-mode for this file
  ;;       type.
  ("go\\.work\\'" . (lambda ()
                      (require 'go-ts-mode)
                      (go-dot-work-mode)))

  :hook
  (go-ts-mode . (lambda ()
                  (require 'go-mode)
                  (add-hook 'before-save-hook #'wgn/apply-gofmt t t)))

  :custom
  (go-command "@go@")
  (godoc-command "@godoc@")
  (gofmt-command "@gofumpt@")
  (godef-command "@godef@")
  (godoc-and-godef-command "@godoc@"))

;; Run Golang tests.
(use-package gotest
  :ensure t

  :commands
  (go-test-current-file
   go-test-current-test
   go-test-current-project
   go-test-current-benchmark
   go-run))

;; Support for the Python Black code formatter.
(use-package python-black
  :ensure t

  :commands
  (python-black-on-save-mode-enable-dwim
   python-black-buffer
   python-black-region
   python-black-statement
   python-black-partial-dwim
   python-black-org-mode-block)

  :hook
  (python-base-mode . python-black-on-save-mode-enable-dwim)

  :custom
  (python-black-command "@black@")
  (python-black-macchiato-command "@macchiato@"))

;; Python support.
;;
;; NOTE: `:pylsp' needs to be configured using a directory-local
;;       variable. For example, to enable `:pydocstyle', type:
;;
;;       M-x add-dir-local-variable RET
;;       python-ts-mode RET
;;       eglot-workspace-configuration RET
;;       (:pylsp (:plugins (:pydocstyle (:enabled t)))) RET
;;
;;       Then, save the file which is generated inside the root of
;;       your project folder.
(use-package python
  :defer t

  :preface
  (defun wgn/python-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((python-mode python-ts-mode) . ("@pylsp@")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Open python files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))

  ;; Set up eglot for python-ts-mode.
  (add-hook 'python-ts-mode-hook #'wgn/python-ts-mode-eglot-setup))

;; Nix support.
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"

  :preface
  (defun wgn/nix-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(nix-mode . ("@nil@")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Set up eglot for nix-mode.
  (add-hook 'nix-mode-hook #'wgn/nix-mode-eglot-setup))

;; YAML support.
(use-package yaml-ts-mode
  :mode
  (("\\.yaml\\'" . yaml-ts-mode)
   ("\\.yml\\'" . yaml-ts-mode))

  :preface
  (defun wgn/yaml-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(yaml-ts-mode . ("@yamlls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Set up eglot for yaml-mode.
  (add-hook 'yaml-ts-mode-hook #'wgn/yaml-ts-mode-eglot-setup))

;; Support for HTML with embedded JS and CSS.
(use-package mhtml-mode
  :defer t

  :preface
  (defun wgn/mhtml-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(mhtml-mode . ("@htmlls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Set up eglot for mhtml-mode.
  (add-hook 'mhtml-mode-hook #'wgn/mhtml-mode-eglot-setup))

;; JSX and TSX support.
(use-package tsx-ts-mode
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode))

  :preface
  (defun wgn/tsx-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(tsx-ts-mode . ("@tsxls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Open JSX files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(js-jsx-mode . tsx-ts-mode))

  ;; Set up eglot for tsx-ts-mode.
  (add-hook 'tsx-ts-mode-hook #'wgn/tsx-ts-mode-eglot-setup))

;; Javascript support.
(use-package js-ts-mode
  :mode "\\.js\\'"

  :preface
  (defun wgn/js-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(js-ts-mode . ("@tsxls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  (defun wgn/js-ts-mode-fix-auto-mode-alist ()
    (setq auto-mode-alist
          (assoc-delete-all "\\.jsx\\'" auto-mode-alist))
    (add-to-list 'auto-mode-alist
                 '("\\.jsx\\'" . tsx-ts-mode)))

  :init
  ;; Set up eglot for js-ts-mode.
  (add-hook 'js-ts-mode-hook #'wgn/js-ts-mode-eglot-setup)

  ;; HACK: `js-ts-mode' modifies `auto-mode-alist' when it is
  ;;       loaded. In particular, it configures JSX files to be opened
  ;;       with `js-ts-mode'. This takes precedence over my own
  ;;       configuration, which is for JSX files to be opened with
  ;;       `tsx-ts-mode'. The consequence of this is confusing
  ;;       behavior:
  ;;
  ;;       - Open a JSX file.
  ;;           -> the JSX file will be in `tsx-ts-mode'.
  ;;       - Open a JS file and then a JSX file.
  ;;           -> the JSX file will be in `js-ts-mode'.
  ;;
  ;;       Hence, we need to re-affirm our JSX configuration after
  ;;       `js-ts-mode' has loaded.
  (add-hook 'js-ts-mode-hook #'wgn/js-ts-mode-fix-auto-mode-alist))

;; Typescript support.
(use-package typescript-ts-mode
  :mode "\\.ts\\'"

  :preface
  (defun wgn/typescript-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(typescript-ts-mode . ("@tsxls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Set up eglot for typescript-ts-mode.
  (add-hook 'typescript-ts-mode-hook #'wgn/typescript-ts-mode-eglot-setup))

;; CSS support.
(use-package css-ts-mode
  :defer t

  :preface
  (defun wgn/css-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(css-ts-mode . ("@cssls@" "--stdio")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  ;; Open CSS files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(css-mode . css-ts-mode))

  ;; Set up eglot for css-ts-mode.
  (add-hook 'css-ts-mode-hook #'wgn/css-ts-mode-eglot-setup))

;; Note taking, time tracking, etc.
(use-package org
  :ensure t

  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

;; Send HTTP requests from org-mode.
(use-package ob-restclient
  :ensure t

  :preface
  (defun wgn/set-up-ob-restclient ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t))))

  :init
  (add-hook 'org-mode-hook #'wgn/set-up-ob-restclient))

;; TeX support.
(use-package auctex
  :ensure t

  :mode
  (("\\.tex\\'" . LaTeX-mode))

  :preface
  (defun wgn/latex-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode) . ("@texlab@")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  ;; Don't start the Emacs server when correlating sources.
  (TeX-source-correlate-start-server nil)
  ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-electric-sub-and-superscript t)
  ;; Just save, don't ask before each compilation.
  (TeX-save-query nil)

  :init
  ;; Set up eglot for TeX-mode.
  (add-hook 'LaTeX-mode-hook #'wgn/latex-mode-eglot-setup))

;; Markdown support.
(use-package markdown-mode
  :ensure t

  :commands
  (markdown-do)

  :defines
  (markdown-mode-map)

  :mode
  (("README\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)
   ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

  :bind
  (:map markdown-mode-map
   ("C-c C-e" . #'markdown-do))

  :custom
  (markdown-command "@multimarkdown@"))

;; Generate table of contents in markdown files.
(use-package markdown-toc
  :ensure t

  :hook
  ((markdown-mode gfm-mode) . markdown-toc-mode))

;; Reading EPUB files.
(use-package nov
  :ensure t

  :mode
  ("\\.epub\\'" . nov-mode))

;; Convert buffers to HTML (including org-mode).
(use-package htmlize
  :ensure t

  :commands
  (htmlize-buffer
   htmlize-file
   htmlize-many-files
   htmlize-many-files-dired))

;; Pretty styling in org-mode buffers.
(use-package org-modern
  :ensure t

  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

;; Indicate the git diff in the margin.
(use-package diff-hl
  :ensure t

  :commands
  (diff-hl-margin-mode
   global-diff-hl-mode)

  :init
  (diff-hl-margin-mode)
  (global-diff-hl-mode))

;; Color scheme.
(use-package modus-themes
  :ensure t

  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)

  :init
  (load-theme 'modus-vivendi-tinted :no-confirm))

;; Use system dark mode settings for theme.
(when (memq system-type '(darwin))
  (use-package auto-dark
    :ensure t
    :diminish auto-dark-mode

    :commands
    (auto-dark-mode)

    :custom
    (auto-dark-allow-osascript t)
    (auto-dark-dark-theme 'modus-vivendi-tinted)
    (auto-dark-light-theme 'modus-operandi-tinted)

    :init
    (auto-dark-mode +1)))

;; Highlight the current line.
(use-package hl-line
  :hook
  ((text-mode prog-mode) . hl-line-mode))

;; Email client.
;;
;; NOTE: set the following variables with customize:
;; - `user-mail-address'
;; - `user-full-name'
;; - `mml-secure-openpgp-signers'
;; - `sendmail-program'
;; - `message-sendmail-extra-arguments'
(use-package notmuch
  :ensure t

  :functions
  (notmuch-user-emails
   notmuch-user-primary-email)

  :commands
  (notmuch-hello
   notmuch-search)

  :custom
  (mail-specify-envelope-from t)
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (notmuch-crypto-process-mime t)
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-secure-smime-sign-with-sender t)
  (notmuch-search-oldest-first nil)
  ;; Determines whether or not to show the mail icon in the mode line.
  (display-time-mail-function
   (lambda ()
     (replace-regexp-in-string
      "\n" " "
      (shell-command-to-string
       "notmuch count tag:unread and tag:inbox"))))
  ;; Determines what happens when you click the mail icon in the mode line.
  (read-mail-command
   (lambda ()
     (interactive)
     (notmuch-search "tag:unread and tag:inbox")))

  :init
  (add-hook 'message-setup-hook #'mml-secure-message-sign)

  :bind
  (("C-c m" . #'notmuch-hello)))

;; Transient interface for notmuch commands.
(use-package notmuch-transient
  :ensure t)

;; Link between org-mode and notmuch buffers.
(use-package ol-notmuch
  :ensure t)

;; Adds various transient interfaces.
(use-package tray
  :ensure t)

;; Monitor and act upon system processes.
(use-package proced
  :commands
  (proced)

  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)

  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))

;; GitHub interface.
;;
;; NOTE: set the following variables with customize:
;; - `consult-gh-default-orgs-list'
(use-package consult-gh
  :ensure t

  :commands
  (consult-gh-orgs
   consult-gh-repo-clone
   consult-gh-search-repos
   consult-gh-search-issues)

  :custom
  (consult-gh-show-preview t)
  (consult-gh-issue-action #'consult-gh--issue-view-action)
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-file-action #'consult-gh--files-view-action)

  :init
  (add-to-list 'exec-path "@gh@"))

;; Embark integration for consult-gh.
(use-package consult-gh-embark
  :after
  (consult-gh embark))

;; Functions for interacting with pass.
(use-package password-store
  :ensure t

  :custom
  (password-store-executable "@pass@")

  :commands
  (password-store-edit
   password-store-get
   password-store-get-field
   password-store-clear
   password-store-copy
   password-store-copy-field
   password-store-init
   password-store-insert
   password-store-generate
   password-store-generate-no-symbols
   password-store-remove
   password-store-rename
   password-store-version
   password-store-url))

;; Interface for interacting with pass.
(use-package pass
  :ensure t

  :commands
  (pass)

  :bind
  (("C-c p p" . #'pass)))

;; Support for hledger for accounting.
;;
;; NOTE: set the following variables with customize:
;; - `hledger-jfile'
(use-package hledger-mode
  :ensure t

  :mode
  (("\\.journal\\'" . hledger-mode))

  :commands
  (hledger-run-command
   hledger-jentry
   hledger-backward-entry
   hledger-next-or-new-entry)

  :defines
  (hledger-mode-map)

  :bind
  (("C-c j" . #'hledger-run-command)
   :map hledger-mode-map
   ("C-c e" . #'hledger-jentry)
   ("M-p" . #'hledger-backward-entry)
   ("M-n" . #'hledger-next-or-new-entry))

  :init
  (add-to-list 'exec-path "@hledger@"))

(use-package flymake-hledger
  :ensure t

  :hook
  ((hledger-mode . flymake-hledger-enable)))

(provide 'default)

;;; default.el ends here
