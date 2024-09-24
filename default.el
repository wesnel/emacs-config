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

(require 'package)

(eval-when-compile
  (require 'use-package))

;;;; Miscellaneous Emacs configuration.
(use-package emacs
  :custom
  ;; Place newline at end of file.
  (require-final-newline t)
  ;; Don't use tabs to indent.
  (indent-tabs-mode nil)
  ;; TAB key indents and then completes.
  (tab-always-indent 'complete)
  ;; Slight override to the above: the first press of TAB key
  ;; completes unless the next character is part of a word.
  (tab-first-completion 'word)
  ;; Maintain correct appearance of tabs.
  (tab-width 8)
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Configure the time display on the mode line.
  (display-time-day-and-date t)
  (display-time-24hr-format t)
  (display-time-use-mail-icon t)
  ;; Allow minibuffer command while in a minibuffer command.
  (enable-recursive-minibuffers t)

  :init
  ;; Remove some UI elements.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

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

;;;; Add MELPA to package archives.
(use-package package
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

;;;; Use user shell $PATH.
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :demand t

    :commands
    (exec-path-from-shell-initialize)

    :defines
    (exec-path-from-shell-arguments
     exec-path-from-shell-variables)

    :custom
    (exec-path-from-shell-arguments nil)

    :config
    (dolist (var '("ARTIFACTORY_PYPI_PASSWORD"
                   "ARTIFACTORY_PYPI_USERNAME"
                   "GITHUB_TOKEN"
                   "GPG_AGENT_INFO"
                   "GPG_TTY"
                   "LANG"
                   "LC_CTYPE"
                   "NIX_PATH"
                   "NIX_SSL_CERT_FILE"
                   "NVM_BIN"
                   "NVM_DIR"
                   "PASSWORD_STORE_DIR"
                   "PASSWORD_STORE_KEY"
                   "PASSWORD_STORE_SIGNING_KEY"
                   "POETRY_HTTP_BASIC_SHIPT_RESOLVE_PASSWORD"
                   "POETRY_HTTP_BASIC_SHIPT_RESOLVE_USERNAME"
                   "SSH_AGENT_PID"
                   "SSH_AUTH_SOCK"))
          (add-to-list 'exec-path-from-shell-variables var)

        (exec-path-from-shell-initialize))))

;;;; Store secrets in Emacs.
(use-package auth-source
  :custom
  (auth-source-pass-filename (getenv "PASSWORD_STORE_DIR"))

  :init
  (auth-source-pass-enable))

;;;; Avoid putting files in weird places.
(use-package no-littering
  :ensure t
  :demand t

  :functions
  (no-littering-theme-backups)

  :config
  (no-littering-theme-backups))

;;;; Required for :bind in use-package.
(use-package bind-key
  :ensure t)

;;;; Required for :diminish in use-package.
(use-package diminish
  :ensure t)

;;;; Whitespace preferences.
(use-package whitespace
  :diminish whitespace-mode

  :preface
  (defun wgn/clean-up-whitespace ()
    (add-hook 'before-save-hook #'whitespace-cleanup nil t))
  (defun wgn/view-whitespace ()
    (whitespace-toggle-options '(lines newline-mark)))

  :hook
  ((prog-mode . wgn/clean-up-whitespace)
   ((prog-mode text-mode) . wgn/view-whitespace)))

;;;; Support for CamelCase.
(use-package subword
  :diminish subword-mode)

;;;; Simplify repeated calling of related commands.
(use-package repeat
  :diminish repeat-mode

  :init
  (repeat-mode +1))

;;;; Navigate code based on an outline.
(use-package outline
  :diminish outline-minor-mode

  :hook
  ((text-mode prog-mode) . outline-minor-mode))

;;;; Syntax highlighting.
(use-package treesit
  :defines
  (treesit-language-source-alist)

  :commands
  (treesit-font-lock-recompute-features)

  :custom
  (treesit-font-lock-level 4))

;;;; Highlight TODO comments.
(use-package hl-todo
  :ensure t

  :commands
  (global-hl-todo-mode)

  :init
  (global-hl-todo-mode +1))

;;;; Completion style that allows for multiple regular expressions.
(use-package orderless
  :ensure t

  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (basic partial-completion orderless)))
                                   (bookmark (styles . (basic substring)))
                                   (library (styles . (basic substring)))
                                   (imenu (styles . (basic substring orderless)))
                                   (kill-ring (styles . (emacs22 orderless)))
                                   (eglot (styles . (emacs22 substring orderless))))))

;;;; Move between windows.
(use-package window
  :bind
  (("M-o" . #'other-window)))

;;;; Open directory as buffer.
(use-package dired
  :custom
  (ls-lisp-use-insert-directory-program nil)

  :preface
  (defun wgn/fix-dired-ls ()
    (require 'ls-lisp))

  :init
  (add-hook 'dired-mode-hook #'wgn/fix-dired-ls))

;;;; Easily mark and kill regions.
(use-package easy-kill
  :ensure t

  :commands
  (easy-kill
   easy-mark)

  :bind
  (([remap kill-ring-save] . #'easy-kill)
   ([remap mark-sexp] . #'easy-mark)))

;;;; Persist history over Emacs restarts.
(use-package savehist
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)

  :init
  (savehist-mode +1))

;;;; Remember your location in a file.
(use-package saveplace
  :init
  (save-place-mode +1))

;;;; Save recent files.
(use-package recentf
  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)

  :init
  (recentf-mode +1))

;;;; Undo history as a tree.
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

;;;; Find identifier by reference in many major modes.
(use-package xref
  :defines
  (xref-search-program-alist
   xref-search-program))

;;;; Project management.
(use-package project
  :commands
  (project-switch-commands
   project-prefixed-buffer-name
   project-remember-project)

  :preface
  (defun wgn/patch-ripgrep-in-xref-search ()
    (setq xref-search-program-alist
          (add-to-list 'xref-search-program-alist
                       '(ripgrep . "xargs -0 @rg@ <C> --null -nH --no-heading --no-messages -g '!*/' -e <R>"))
          xref-search-program 'ripgrep))

  :init
  (add-hook 'emacs-startup-hook #'wgn/patch-ripgrep-in-xref-search))

;;;; Git interface.
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

;;;; Support for git-delta in magit.
(use-package magit-delta
  :ensure t

  :hook
  (magit-mode . magit-delta-mode)

  :custom
  (magit-delta-delta-executable "@delta@"))

;;;; Dockerfile support.
(use-package dockerfile-ts-mode
  :mode "\\Dockerfile\\'"

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile.git" "v0.2.0"))))

;;;; Janet support.
(use-package janet-mode
  :ensure t
  :mode "\\.janet\\'"

  :defines
  (janet-mode-map))

;;;; Interactive Janet development.
(use-package ijanet-mode
  :ensure t

  ;; TODO: If using a non-Nix configuration, you'll need to point this
  ;;       towards your local git clone of ijanet-mode.  In this case,
  ;;       you will also need to remove the `:ensure' line from above.
  ; :load-path ("~/git/github.com/serialdev/ijanet-mode")

  :commands
  (ijanet
   ijanet-eval-buffer
   ijanet-eval-line
   ijanet-eval-region)

  :bind
  (:map janet-mode-map
   ("C-c C-p" . #'ijanet)
   ("C-c C-b" . #'ijanet-eval-buffer)
   ("C-c C-l" . #'ijanet-eval-line)
   ("C-c C-r" . #'ijanet-eval-region))

  :custom
  (ijanet-program "@janet@"))

;;;; Clojure support.
(use-package clojure-mode
  :ensure t

  :commands
  (clojure-mode
   clojurescript-mode
   clojurec-mode)

  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode)
   ("\\.cljc\\'" . clojurec-mode)))

;;;; Interactive Clojure development.
(use-package cider
  :ensure t

  :commands
  (cider-jack-in)

  :custom
  (cider-lein-command "@lein@")
  (cider-clojure-cli-command "@clojure@")
  (cider-boot-command "@boot@"))

;;;; Interactive Lisp development.
(use-package slime
  :ensure t

  :commands
  (slime
   slime-eval-buffer)

  :custom
  (inferior-lisp-program "@sbcl@"))

;;;; Automatically manage parentheses in Lisps.
(use-package parinfer-rust-mode
  :ensure t

  :commands
  (parinfer-rust-mode)

  :preface
  (defun wgn/enable-parinfer ()
    (electric-pair-local-mode -1)
    (parinfer-rust-mode +1))

  :hook
  ((lisp-data-mode
    janet-mode
    clojure-mode
    clojurescript-mode)
   . wgn/enable-parinfer)

  :custom
  (parinfer-rust-library @parinfer@))

;;;; Show matching parentheses.
(use-package paren
  :custom
  (show-paren-context-when-offscreen t)

  :init
  (show-paren-mode +1))

;;;; Insert matching parentheses (and brackets, braces, etc).
(use-package elec-pair
  :hook
  (prog-mode . electric-pair-local-mode)

  :custom
  (electric-pair-open-newline-between-pairs t))

;;;; Focused presentation mode built on top of outline.
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

;;;; Spell checxing and correction.
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

;;;; Avoid need for modifier keys.
(use-package devil
  :ensure t

  :commands
  (global-devil-mode)

  :init
  (global-devil-mode +1))

(when (display-graphic-p)
  (use-package doc-view
    :init
    (add-to-list 'exec-path "@gs@")
    (add-to-list 'exec-path "@mupdf@")))

(use-package grep
  :defines
  (grep-mode-map))

;;;; Editable grep buffers.
(use-package wgrep
  :ensure t

  :commands
  (wgrep-change-to-wgrep-mode)

  :bind
  (:map grep-mode-map
   ("C-c C-p" . #'wgrep-change-to-wgrep-mode)))

;;;; Creates a minor mode for when the point is in a selection.
(use-package selected
  :ensure t
  :diminish selected-minor-mode

  :commands
  (selected-global-mode)

  :defines
  (selected-keymap)

  :init
  (selected-global-mode +1))

;;;; Edit text with multiple cursors.
(use-package multiple-cursors
  :ensure t

  :commands
  (mc/mark-all-like-this
   mc/add-cursor-on-click)

  :bind
  (("C-S-<mouse-1>" . #'mc/add-cursor-on-click)
   :map selected-keymap
   ("C-x c" . #'mc/mark-all-like-this)))

;;;; Structured editing and navigation based on tree-sitter.
(use-package combobulate
  :ensure t

  ;; TODO: If using a non-Nix configuration, you'll need to point this
  ;;       towards your local git clone of combobulate.  In this case,
  ;;       you will also need to remove the `:ensure' line from above.
  ; :load-path ("~/git/github.com/mickeynp/combobulate")

  :custom
  (combobulate-key-prefix "C-c o")

  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;;;; Create scratch buffers of any major mode.
(use-package scratch
  :ensure t

  :commands
  (scratch)

  :custom
  (scratch-use-singleton-buffer nil)

  :bind
  (("C-c s" . #'scratch)))

;;;; Visual `query-replace-regexp'.
(use-package visual-regexp
  :ensure t

  :commands
  (vr/query-replace)

  :bind
  (("C-M-%" . #'vr/query-replace)))

;;;; Shell written in Emacs Lisp.
(use-package eshell
  :preface
  (defun wgn/disable-line-numbers ()
    (display-line-numbers-mode -1))

  :init
  (add-hook 'eshell-mode-hook #'wgn/disable-line-numbers))

;;;; Terminal emulator.
(use-package vterm
  :ensure t

  :commands
  (vterm
   vterm-other-window)

  :custom
  (vterm-max-scrollback 100000)

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

;;;; Error checking.
(use-package flymake
  :hook
  (prog-mode . flymake-mode)

  :custom
  (checkdoc-package-keywords-flag t))

;;;; Language server integration.
(use-package eglot
  :ensure t

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

;;;; Debug adapter integration.
(use-package dape
  :ensure t

  :defines
  (dape-configs)

  :commands
  (dape
   dape-breakpoint-global-mode
   dape-breakpoint-load
   dape-breakpoint-save)

  :hook
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load))

  :config
  (dape-breakpoint-global-mode)

  (add-hook 'dape-on-start-hooks
            (lambda ()
              (save-some-buffers t t))))

;;;; Coding copilot with local model.
(use-package emacs-copilot
  :ensure t

  ;; TODO: If using a non-Nix configuration, you'll need to point this
  ;;       towards your local git clone of emacs-copilot.  In this
  ;;       case, you will also need to remove the `:ensure' line from
  ;;       above.
  ; :load-path ("~/git/github.com/jart/emacs-copilot")

  :bind
  (:map prog-mode-map
   ("C-c C-k" . #'copilot-complete))

  :commands
  (copilot-complete)

  :defines
  (copilot-bin))

;;;; Interface for talking to ChatGPT.
;;
;; NOTE: If you want to actually use ChatGPT, you need to store your
;;       OpenAI credentials under auth-source.
;;
;; TODO: Integrate this with a local model.
(use-package org-ai
  :ensure t

  :commands
  (org-ai-mode
   org-ai-global-mode)

  :hook
  (org-mode . org-ai-mode)

  :init
  (org-ai-global-mode))

;;;; Displays available keybindings in a pop-up.
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

;;;; Completion using the built-in *Completions* buffer.
(use-package minibuffer
  :preface
  (defun wgn/sort-by-alpha-length (elems)
    (sort elems (lambda (c1 c2)
                  (or (string-version-lessp c1 c2)
                      (< (length c1) (length c2))))))

  (defun wgn/sort-by-history (elems)
    (if-let ((hist (and (not (eq minibuffer-history-variable t))
                        (symbol-value minibuffer-history-variable))))
        (minibuffer--sort-by-position hist elems)
      (wgn/sort-by-alpha-length elems)))

  (defun wgn/completion-category ()
    (when-let ((window (active-minibuffer-window)))
      (with-current-buffer (window-buffer window)
        (completion-metadata-get
         (completion-metadata (buffer-substring-no-properties
                               (minibuffer-prompt-end)
                               (max (minibuffer-prompt-end) (point)))
                              minibuffer-completion-table
                              minibuffer-completion-predicate)
         'category))))

  (defun wgn/sort-multi-category (elems)
    (pcase (wgn/completion-category)
      ('nil elems) ; no sorting
      ('kill-ring elems)
      (_ (wgn/sort-by-history elems))))

  :custom
  (completions-format 'one-column)
  (completions-header-format nil)
  (completions-max-height 20)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (completions-sort #'wgn/sort-multi-category)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;;;; Rich annotations in the minibuffer completion.
(use-package marginalia
  :ensure t

  :commands
  (marginalia-mode)

  :init
  (marginalia-mode +1))

;;;; Perform actions on point.
(use-package embark
  :ensure t

  :preface
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
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

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using completing-read."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  :defines
  (embark-indicators)

  :commands
  (embark-act
   embark-dwim
   embark-bindings
   embark-prefix-help-command
   embark-eldoc-first-target
   embark-completing-read-prompter
   embark--truncate-target)

  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Replace the key help with a completing-read interface:
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc:
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  :config
  ;; Hide the mode line of the Embark live/completions buffers:
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;; More helpful documentation for Emacs Lisp.
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

;;;; Open code from Emacs in the web browser.
(use-package elsewhere
  :ensure t

  ;; TODO: If using a non-Nix configuration, you'll need to point this
  ;;       towards your local git clone of elsewhere.  In this case,
  ;;       you will also need to remove the `:ensure' line from above.
  ; :load-path ("~/git/github.com/wesnel/elsewhere")

  :commands
  (elsewhere-open
   elsewhere-build-url))

;;;; Send HTTP requests from Emacs.
(use-package restclient
  :ensure t
  :mode "\\.http\\'")

;;;; Basic Golang support.
;;
;; HACK: This is built-in to Emacs, but there is also a third-party
;;       go-mode which we will install next.
(use-package go-ts-mode
  :mode
  ("go\\.mod\\'" . go-mod-ts-mode)

  :preface
  (defun wgn/go-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'exec-path "@go@")
      (add-to-list 'eglot-server-programs
                   '((go-ts-mode go-mod-ts-mode) . ("@gopls@")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(go "https://github.com/tree-sitter/tree-sitter-go.git" "v0.20.0"))
    (add-to-list 'treesit-language-source-alist
                 '(gomod "https://github.com/camdencheek/tree-sitter-go-mod.git" "v1.0.2")))

  ;; Open go files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(go-mode . go-ts-mode))

  ;; Set up eglot for go-ts-mode.
  (add-hook 'go-ts-mode-hook #'wgn/go-ts-mode-eglot-setup)

  ;; CamelCase aware editing operations.
  (add-hook 'go-ts-mode-hook #'subword-mode)

  :config
  ;; Configure dape for delve:
  (add-to-list
   'dape-configs
   `(delve-unit-test
     modes (go-mode go-ts-mode)
     ensure dape-ensure-command
     fn dape-config-autoport
     command "@dlv@"
     command-args ("dap" "--listen" "127.0.0.1::autoport")
     command-cwd dape-cwd-fn
     port :autoport
     :type "debug"
     :request "launch"
     :mode (lambda ()
             (if (string-suffix-p "_test.go" (buffer-name))
                 "test"
               "debug"))
     :cwd dape-cwd-fn
     :program (lambda ()
                (if (string-suffix-p "_test.go" (buffer-name))
                    (concat "./" (file-relative-name default-directory (funcall dape-cwd-fn)))
                  (funcall dape-cwd-fn)))
     :args (lambda ()
             (require 'which-func)
             (if (string-suffix-p "_test.go" (buffer-name))
                 (when-let* ((test-name (which-function))
                             (test-regexp (concat "^" test-name "$")))
                   (if test-name `["-test.run" ,test-regexp]
                     (error "No test selected"))))
             []))))

;;;; Extra Golang support.
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

;;;; Run Golang tests.
(use-package gotest
  :ensure t

  :commands
  (go-test-current-file
   go-test-current-test
   go-test-current-project
   go-test-current-benchmark
   go-run))

;;;; Python support.
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

  :custom
  (python-indent-def-block-scale 1)

  :preface
  (defun wgn/python-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((python-mode python-ts-mode) .
                     ,(eglot-alternatives '(("@asdf@" "exec" "poetry" "run" "pylsp")
                                            ("@poetry@" "run" "pylsp")
                                            "@pylsp@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(python "https://github.com/tree-sitter/tree-sitter-python.git")))

  ;; Open python files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))

  ;; Set up eglot for python-ts-mode.
  (add-hook 'python-ts-mode-hook #'wgn/python-ts-mode-eglot-setup))

;;;; Nix support.
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

;;;; YAML support.
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
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

  ;; Set up eglot for yaml-mode.
  (add-hook 'yaml-ts-mode-hook #'wgn/yaml-ts-mode-eglot-setup))

;;;; Terraform support.
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\(vars\\)?\\'"

  :preface
  (defun wgn/terraform-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(terraform-mode . ("@terraformls@" "serve")))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format t t))
    (eglot-ensure))

  :custom
  (terraform-indent-level 4)

  :init
  ;; Set up eglot for terraform-mode.
  (add-hook 'terraform-mode-hook #'wgn/terraform-mode-eglot-setup))

;;;; Support for HTML with embedded JS and CSS.
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
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))))

  ;; Set up eglot for mhtml-mode.
  (add-hook 'mhtml-mode-hook #'wgn/mhtml-mode-eglot-setup))

;;;; JSX and TSX support.
(use-package tsx-ts-mode
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.jsx\\'" . tsx-ts-mode))

  :preface
  (defun wgn/tsx-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      ;; FIXME: Need to figure out how to change indent size.
      ; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   '(tsx-ts-mode . ("@tsxls@" "--stdio"))))

    (eglot-ensure))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))))

  ;; Open JSX files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(js-jsx-mode . tsx-ts-mode))

  ;; Set up eglot for tsx-ts-mode.
  (add-hook 'tsx-ts-mode-hook #'wgn/tsx-ts-mode-eglot-setup))

;;;; Javascript support.
(use-package js-ts-mode
  :mode "\\.js\\'"

  :preface
  (defun wgn/js-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      ;; FIXME: Need to figure out how to change indent size.
      ; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   '(js-ts-mode . ("@tsxls@" "--stdio"))))

    (eglot-ensure))

  (defun wgn/js-ts-mode-fix-auto-mode-alist ()
    (setq auto-mode-alist
          (assoc-delete-all "\\.jsx\\'" auto-mode-alist))
    (add-to-list 'auto-mode-alist
                 '("\\.jsx\\'" . tsx-ts-mode)))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))))

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

;;;; Typescript support.
(use-package typescript-ts-mode
  :mode "\\.ts\\'"

  :preface
  (defun wgn/typescript-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      ;; FIXME: Need to figure out how to change indent size.
      ; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   '(typescript-ts-mode . ("@tsxls@" "--stdio"))))

    (eglot-ensure))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))))

  ;; Set up eglot for typescript-ts-mode.
  (add-hook 'typescript-ts-mode-hook #'wgn/typescript-ts-mode-eglot-setup))

;;;; JSON support.
(use-package json-ts-mode
  :mode "\\.json\\'"

  :preface
  (defun wgn/json-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   '(json-ts-mode . ("@jsonls@" "--stdio"))))

    (eglot-ensure))

  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.21.0"))))

  ;; Open JSON files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(js-json-mode . json-ts-mode))

  ;; Set up eglot for json-ts-mode.
  (add-hook 'json-ts-mode-hook #'wgn/json-ts-mode-eglot-setup))

;;;; CSS support.
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
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))))

  ;; Open CSS files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(css-mode . css-ts-mode))

  ;; Set up eglot for css-ts-mode.
  (add-hook 'css-ts-mode-hook #'wgn/css-ts-mode-eglot-setup))

;;;; Note taking, time tracking, etc.
(use-package org
  :ensure t

  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

;;;; Send HTTP requests from org-mode.
(use-package ob-restclient
  :ensure t

  :preface
  (defun wgn/set-up-ob-restclient ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t))))

  :init
  (add-hook 'org-mode-hook #'wgn/set-up-ob-restclient))

;;;; TeX support.
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

;;;; Markdown support.
(use-package markdown-mode
  :ensure t

  :commands
  (markdown-do)

  :defines
  (markdown-mode-map
   markdown-fontify-code-blocks-natively)

  :mode
  (("README\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)
   ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

  :bind
  (:map markdown-mode-map
   ("C-c C-e" . #'markdown-do))

  :custom
  (markdown-command "@multimarkdown@")
  (markdown-fontify-code-blocks-natively t))

;;;; Generate table of contents in markdown files.
(use-package markdown-toc
  :ensure t

  :hook
  ((markdown-mode gfm-mode) . markdown-toc-mode))

;;;; Reading EPUB files.
(use-package nov
  :ensure t

  :init
  (add-hook 'nov-mode-hook #'wgn/disable-line-numbers)

  :mode
  ("\\.epub\\'" . nov-mode))

;;;; Convert buffers to HTML (including org-mode).
(use-package htmlize
  :ensure t

  :commands
  (htmlize-buffer
   htmlize-file
   htmlize-many-files
   htmlize-many-files-dired))

;;;; Pretty styling in org-mode buffers.
(use-package org-modern
  :ensure t

  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda)))

;;;; Indicate the git diff in the margin.
(use-package git-gutter
  :ensure t

  :commands
  (global-git-gutter-mode)

  :init
  (global-git-gutter-mode +1))

;;;; Color scheme.
(use-package modus-themes
  :ensure t

  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)

  :init
  (load-theme 'modus-vivendi-tinted :no-confirm))

;;;; Use system dark mode settings for theme.
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

;;;; Highlight the current line.
(use-package hl-line
  :hook
  ((text-mode prog-mode) . hl-line-mode))

(use-package mml-sec
  :commands
  (mml-secure-message-sign))

;;;; Email client.
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

;;;; Link between org-mode and notmuch buffers.
(use-package ol-notmuch
  :ensure t)

;;;; Monitor and act upon system processes.
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

;;;; Functions for interacting with pass.
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

;;;; Interface for interacting with pass.
(use-package pass
  :ensure t

  :commands
  (pass)

  :bind
  (("C-c p p" . #'pass)))

;;;; Support for hledger for accounting.
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
