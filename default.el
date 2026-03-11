;;; default.el --- Wesley's Emacs Config  -*- lexical-binding: t; -*-

;; URL: https://git.sr.ht/~wgn/emacs-config/

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
  ;; Do not allow the cursor in the minibuffer prompt.
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Enable mouse-based context menu.
  (context-menu-mode t)
  ;; Mail configuration.
  (smtpmail-default-smtp-server "smtp.fastmail.com")
  (smtpmail-smtp-service 587)
  (message-send-mail-function #'message-use-send-mail-function)
  (send-mail-function #'smtpmail-send-it)
  (mail-user-agent 'gnus-user-agent)

  :init
  ;; NOTE: For development, you will want to trust this file, wherever it is.
  ;; (add-to-list 'trusted-content "~/git/github.com/wesnel/emacs-config/default.el")

  ;; Use Trash for deleting files.
  (setq delete-by-moving-to-trash t)
  (when (memq system-type '(darwin))
    (setq trash-directory "~/.Trash"))

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
  (display-time)

  ;; Change font.
  ;; TODO: Make font configurable with nix.
  (set-face-attribute 'default nil :family "Iosevka Nerd Font Mono")
  ;; Enable rendering SF symbols on macOS.
  (when (memq system-type '(darwin))
    (set-fontset-font t nil "SF Pro Display" nil 'append))

  ;; Change startup frame size.
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 120))

  ;; Add transparency.
  ;; TODO: Make a fork of the `auto-dark' package which toggles transparency
  ;;       based on system transparency preference settings.
  (when (display-graphic-p)
    (add-to-list 'default-frame-alist '(alpha-background . 50))
    (set-frame-parameter nil 'alpha-background 50)))

(use-package server
  :commands
  (server-running-p
   server-start)

  :init
  ;; Start server.
  (unless (server-running-p)
    (server-start)))

(use-package mml-sec
  :commands
  (mml-secure-message-sign)

  :custom
  (mm-verify-option 'always)
  (mm-decrypt-option 'always)
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-secure-smime-sign-with-sender t)

  :init
  (add-hook 'message-setup-hook #'mml-secure-message-sign))

;;;; Add MELPA to package archives.
(use-package package
  :init
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

;;;; Note taking, time tracking, etc.
;;
;; http://doc.norang.ca/org-mode.html
(use-package org
  :ensure t

  :bind
  (("C-c l" . #'org-store-link)
   ("C-c a" . #'org-agenda)
   ("C-c c" . #'org-capture))

  :commands
  (org-heading-components)

  :defines
  (org-mode-map)

  :preface
  (defun wgn/verify-refile-target ()
    "Exclude TODO keywords with a DONE state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (defun wgn/org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with / RET"
    (and (cond
          ((string= tag "hold")
           t))
         (concat "-" tag)))

  :config
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-protocol)

  :custom
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-use-fast-todo-selection t)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-agenda-files '("~/org"))
  (org-directory "~/org")
  (org-default-notes-file "~/org/refile.org")
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-target-verify-function #'wgn/verify-refile-target)
  (org-indirect-buffer-display 'current-window)
  (org-agenda-auto-exclude-function #'wgn/org-auto-exclude-function)
  (org-enforce-todo-dependencies t)
  (org-startup-indented t)
  (org-cycle-separator-lines 0)
  (org-reverse-note-order nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-deadline-warning-days 30)
  (org-use-speed-commands t)
  (org-blank-before-new-entry
   '((heading)
     (plain-list-item . auto)))
  (org-refile-targets
   '((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9)))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)))
  (org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("HOLD" ("WAITING") ("HOLD" . t))
     (done ("WAITING") ("HOLD"))
     ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
     ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  (org-capture-templates
   `(("t" "todo" entry (file ,org-default-notes-file)
      "* TODO %?\n%U\n%a\n")
     ("r" "respond" entry (file ,org-default-notes-file)
      "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
     ("n" "note" entry (file ,org-default-notes-file)
      "* %? :NOTE:\n%U\n%a\n")
     ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
      "* %?\n%U\n")
     ("w" "org-protocol" entry (file "~/git/org/refile.org")
      "* TODO Review %c\n%U\n" :immediate-finish t)
     ("h" "Habit" entry (file ,org-default-notes-file)
      "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
  (org-tag-alist
   '(("WAITING" . ?w)
     ("HOLD" . ?h)
     ("PERSONAL" . ?P)
     ("WORK" . ?W)
     ("NOTE" . ?n)
     ("CANCELLED" . ?c))))

;;;; Better styling for `org-mode' buffers.
(use-package org-modern
  :ensure t

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)

  :config
  ;; TODO: Make font configurable with nix.
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka Nerd Font Mono"))

;;;; Better indenting behavior for `org-modern'.
(use-package org-modern-indent
  :ensure t

  :hook
  (org-mode . org-modern-indent-mode))

;;;; CSL citation processor.
(use-package citeproc
  :ensure t)

;;;; Store encrypted information in org files.
(use-package org-crypt
  :commands
  (org-crypt-use-before-save-magic)

  :init
  (org-crypt-use-before-save-magic)

  :custom
  (org-tags-exclude-from-inheritance '("crypt")))

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
                   "EDITOR"
                   "GITHUB_TOKEN"
                   "GPG_AGENT_INFO"
                   "GPG_TTY"
                   "JAVA_HOME"
                   "LANG"
                   "LC_CTYPE"
                   "NIX_PATH"
                   "NIX_SSL_CERT_FILE"
                   "NVM_BIN"
                   "NVM_DIR"
                   "PASSWORD_STORE_DIR"
                   "PASSWORD_STORE_KEY"
                   "PASSWORD_STORE_SIGNING_KEY"
                   ;; TODO: Remove Shipt-specific config.
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

;;;; Automatically encrypt and decrypt files.
(use-package epa-file
  :commands
  (epa-file-enable)

  :init
  (epa-file-enable))

;;;; Avoid putting files in weird places.
(use-package no-littering
  :ensure t

  :functions
  (no-littering-theme-backups)

  :init
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
   (prog-mode . wgn/view-whitespace)))

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

;;;; Open directory as buffer.
(use-package dired
  :custom
  (ls-lisp-use-insert-directory-program nil)
  (dired-kill-when-opening-new-dired-buffer t)

  :defines
  (dired-mode-map)

  :preface
  (defun wgn/fix-dired-ls ()
    (require 'ls-lisp))

  :init
  (add-hook 'dired-mode-hook #'wgn/fix-dired-ls))

;;;; Apply shell functions to current buffer or files marked in `dired'.
(use-package dwim-shell-command
  :ensure t

  :commands
  (dwim-shell-command)

  :bind
  (([remap shell-command] . #'dwim-shell-command)
   :map dired-mode-map
   ([remap dired-do-async-shell-command] . #'dwim-shell-command)
   ([remap dired-do-shell-command] . #'dwim-shell-command)
   ([remap dired-smart-shell-command] . #'dwim-shell-command))

  :config
  (require 'dwim-shell-commands))

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
  :commands
  (savehist-mode)

  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60)

  :init
  (savehist-mode +1))

;;;; Remember your location in a file.
(use-package saveplace
  :commands
  (save-place-mode)

  :init
  (save-place-mode +1))

;;;; Save recent files.
(use-package recentf
  :commands
  (recentf-mode)

  :custom
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)

  :init
  (recentf-mode +1))

;;;; Undo history as a tree.
(use-package vundo
  :ensure t

  :commands
  (vundo))

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

;;;; Show hint of full path in headerline.
(use-package breadcrumb
  :ensure t

  :commands
  (breadcrumb-mode)

  :init
  (breadcrumb-mode +1))

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

;;;; Aggregate project TODO comments in magit.
(use-package magit-todos
  :ensure t

  :commands
  (magit-todos-mode)

  :init
  (magit-todos-mode +1))

;;;; Dockerfile support.
(use-package dockerfile-ts-mode
  :mode "\\Dockerfile\\'")

;;;; Janet support.
(use-package janet-mode
  :ensure t
  :mode "\\.janet\\'"

  :defines
  (janet-mode-map))

;;;; Interactive Janet development.
(use-package ijanet-mode
  :ensure t

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
   ("C-c C-r" . #'ijanet-eval-region)))

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
  (cider-jack-in))

;;;; Interactive Lisp development.
(use-package slime
  :ensure t

  :commands
  (slime
   slime-eval-buffer))

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

;;;; Balanced window margins.
(use-package olivetti
  :ensure t

  :commands
  (olivetti-mode)

  :defines
  (olivetti-mode-map)

  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

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

  :defines
  (logos-focus-mode-map)

  :bind
  (([remap narrow-to-region] . #'logos-narrow-dwim)
   ([remap forward-page] . #'logos-forward-page-dwim)
   ([remap backward-page] . #'logos-backward-page-dwim)
   ("<f9>" . #'logos-focus-mode)
   :map logos-focus-mode-map
   ([remap right-char] . #'logos-forward-page-dwim)
   ([remap next-line] . #'logos-forward-page-dwim)
   ([remap left-char] . #'logos-backward-page-dwim)
   ([remap previous-line] . #'logos-backward-page-dwim))

  :config
  (setq-default
   logos-hide-cursor t
   logos-hide-mode-line t
   logos-hide-header-line t
   logos-hide-buffer-boundaries t
   logos-hide-fringe t
   logos-variable-pitch t
   logos-buffer-read-only t
   logos-scroll-lock t
   logos-olivetti t))

;;;; Spell checxing and correction.
(use-package jinx
  :ensure t
  :diminish jinx-mode

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

;;;; Structured editing and navigation based on tree-sitter.
(use-package combobulate
  :ensure t

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

  :defines
  (vterm-mode-map)

  :custom
  (vterm-max-scrollback 100000)

  :bind
  (:map vterm-mode-map
   ([return] . nil)
   :map vterm-copy-mode-map
   ([return] . nil))

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
  :ensure t

  :preface
  ;; HACK: bug#79930
  (defun next-error-this-buffer-no-select (&optional n)
    "Move point to the next error in the current buffer and highlight match.
 Prefix arg N says how many error messages to move forwards (or
 backwards, if negative).
 Finds and highlights the source line like \\[next-error], but does not
 select the source buffer."
    (interactive "p")
    (next-error-select-buffer (current-buffer))
    (next-error-no-select n))
  (defun previous-error-this-buffer-no-select (&optional n)
    "Move point to the previous error in the current buffer and highlight match.
 Prefix arg N says how many error messages to move forwards (or
 backwards, if negative).
 Finds and highlights the source line like \\[previous-error], but does not
 select the source buffer."
    (interactive "p")
    (next-error-select-buffer (current-buffer))
    (previous-error-no-select n))

  :hook
  (prog-mode . flymake-mode)

  :custom
  (checkdoc-package-keywords-flag t))

;;;; Snippets.
(use-package yasnippet
  :ensure t

  :commands
  (yas-minor-mode
   yas-reload-all)

  :hook
  (prog-mode . (lambda ()
                 (yas-reload-all)
                 (yas-minor-mode +1))))

;;;; Documentation in echo area.
(use-package eldoc
  :diminish eldoc-mode

  :commands
  (eldoc-documentation-compose-eagerly)

  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

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
   eglot-alternatives
   eglot-ensure
   eglot-rename
   eglot-find-implementation
   eglot-code-actions
   eglot-format-buffer)

  :custom
  (eglot-code-action-indications '(eldoc-hint))

  :bind
  (:map eglot-mode-map
   ("C-c C-l ." . #'xref-find-definitions)
   ("C-c C-l ?" . #'xref-find-references)
   ("C-c C-l r" . #'eglot-rename)
   ("C-c C-l i" . #'eglot-find-implementation)
   ("C-c C-l e" . #'eglot-code-actions)))

;;;; Pop-up window instead of echo area for Eldoc documentation
(when (display-graphic-p)
  (use-package eldoc-box
    :ensure t

    :custom
    (eldoc-box-clear-with-C-g t)

    :commands
    (eldoc-box-help-at-point)

    :hook
    (eglot-managed-mode
     . (lambda ()
         (bind-key "C-c C-l d" #'eldoc-box-help-at-point 'eglot-mode-map)))))

;;;; Debug adapter integration.
;;
;; TODO: Evaluate whether I prefer this over the built-in Grand Unified Debugger (GUD).
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

  (add-hook 'dape-start-hook
            (lambda ()
              (save-some-buffers t t))))

;;;; Improved `completing-read' functions.
(use-package consult
  :ensure t

  :commands
  (consult-bookmark
   consult-buffer
   consult-buffer-other-frame
   consult-buffer-other-tab
   consult-buffer-other-window
   consult-compile-error
   consult-complex-command
   consult-find
   consult-flymake
   consult-focus-lines
   consult-git-grep
   consult-global-mark
   consult-goto-line
   consult-grep
   consult-history
   consult-imenu
   consult-imenu-multi
   consult-info
   consult-isearch-history
   consult-keep-lines
   consult-kmacro
   consult-line
   consult-line-multi
   consult-locate
   consult-man
   consult-mark
   consult-mode-command
   consult-outline
   consult-project-buffer
   consult-register
   consult-register-load
   consult-register-store
   consult-ripgrep
   consult-xref
   consult-yank-pop)

  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . #'consult-mode-command)
   ("C-c h" . #'consult-history)
   ("C-c k" . #'consult-kmacro)
   ("C-c m" . #'consult-man)
   ("C-c i" . #'consult-info)
   ([remap Info-search] . #'consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . #'consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . #'consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . #'consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
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
   ("M-s d" . #'consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . #'consult-locate)
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
   ("M-r" . #'consult-history)))               ;; orig. previous-matching-history-element

;;;; Cross-references.
(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;;;; Custom `completion-at-point' completion.
(use-package corfu
  :ensure t

  :commands
  (global-corfu-mode)

  :init
  (global-corfu-mode +1))

;;;; Completion using the built-in *Completions* buffer.
(use-package minibuffer
  :custom
  (completions-format 'one-column)
  (completions-header-format nil)
  (completions-max-height 20)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;;;; Custom minibuffer-based completion.
(use-package vertico
  :ensure t

  :commands
  (vertico-mode)

  :init
  (vertico-mode +1))

;;;; Rich annotations in the minibuffer completion.
(use-package marginalia
  :ensure t

  :commands
  (marginalia-mode)

  :init
  (marginalia-mode +1))

;;;; Integration between embark and consult.
(use-package embark-consult
  :ensure t)

;;;; Perform actions on point.
(use-package embark
  :ensure t

  :defines
  (embark-indicators
   embark-highlight-indicator
   embark-isearch-highlight-indicator
   embark-general-map)

  :commands
  (embark-act
   embark-dwim
   embark-export
   embark-bindings
   embark-prefix-help-command
   embark-completing-read-prompter
   embark--truncate-target)

  :bind
  (("C-." . #'embark-act)
   ("M-." . #'embark-dwim)
   ("C-h B" . #'embark-bindings)
   :map minibuffer-mode-map
   ("M-." . #'embark-export))

  :init
  ;; Replace the key help with a completing-read interface:
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Set up indicator strategy:
  (setq embark-indicators '(embark-highlight-indicator
                            embark-isearch-highlight-indicator)))

;;;; LLM integration.
(use-package gptel
  :ensure t

  :commands
  (gptel-send
   gptel
   gptel-rewrite
   gptel-menu
   gptel-add
   gptel-add-file
   gptel-org-set-topic
   gptel-org-set-properties
   gptel-make-anthropic
   gptel-make-ollama
   gptel-make-gh-copilot
   gptel-tools)

  :bind
  (("C-c RET" . #'gptel-send)
   ("C-c <return>" . #'gptel-send))

  :config
  ;; TODO: Configure `gptel' backends based on Nix config.
  (require 'gptel-integrations))

;;;; Agent integration for `gptel'.
(use-package gptel-agent
  :ensure t

  :commands
  (gptel-agent
   gptel-agent-update)

  :config
  (gptel-agent-update))

;;;; Wrapper around `comint-mode'.
(use-package shell-maker
  :ensure t

  :functions
  (shell-maker-submit))

;;;; Shell for interacting with LLMs.
(use-package chatgpt-shell
  :ensure t

  :commands
  (chatgpt-shell
   chatgpt-shell-describe-code
   chatgpt-shell-describe-image
   chatgpt-shell-eshell-summarize-last-command-output
   chatgpt-shell-eshell-whats-wrong-with-last-command
   chatgpt-shell-fix-error-at-point
   chatgpt-shell-generate-unit-test
   chatgpt-shell-prompt
   chatgpt-shell-prompt-appending-kill-ring
   chatgpt-shell-prompt-compose
   chatgpt-shell-proofread-paragraph-or-region
   chatgpt-shell-quick-insert
   chatgpt-shell-refactor-code
   chatgpt-shell-send-and-review-region
   chatgpt-shell-send-region
   chatgpt-shell-write-git-commit))

;;;; Support for ACP.
(use-package acp
  :ensure t

  :functions
  (acp-make-client))

;;;; Shell for interacting with LLM agents.
;;
;; TODO: Investigate the use of VMs to isolate the agent .
;;
;; One potential issue will be that I don't use API keys for billing,
;; which means that authenticating from within the VM will be hard.
;;
;; Here are some VM runners to consider:
;;
;; - https://github.com/superhq-ai/shuru
;; - https://github.com/lima-vm/lima
;;
;; Here are some things to try as I build out this VM idea:
;;
;; - The project files could be copied into a new VM using
;;   `agent-shell-container-command-runner'.
;;
;; - MCP servers running in the host system could be made accessible
;;   to the VM via their ports.
;;
;; - Tools and skills in the host system should be somehow copied or
;;   otherwise made accessible to the VM.
;;
;; - The Emacs `agent-shell-to-go' package could be used by the host
;;   Emacs process in order to enable mobile command of the agents.
;;
;; - If manual editing within the VM is needed, then the host Emacs
;;   process could use `agent-shell-path-resolver-function' to access
;;   the VM files via TRAMP.
;;
;; - `agent-shell-text-file-capabilities' should be set to nil in
;;   order to prevent accidental modifications outside the VM.
;;
;; - For parallel work, "checkpoints" of multiple VM states could
;;   await later manual review.
;;
;; - An agent "skill" could use emacsclient in the host system to open
;;   ediff buffers between the state of the code in the host and the
;;   modified version of the code in the VM.  The changes could then
;;   be manually accepted into the host.
(use-package agent-shell
  :ensure t

  :functions
  (agent-shell-cwd
   agent-shell-interrupt)

  :defines
  (agent-shell-mode-map)

  :commands
  (agent-shell)

  :bind
  (:map agent-shell-mode-map
   ("RET" . #'newline)
   ("C-c C-c" . #'shell-maker-submit)
   ("C-c C-k" . #'agent-shell-interrupt)))

;;;; Notifications for `agent-shell'.
(use-package agent-shell-attention
  :ensure t

  :commands
  (agent-shell-attention-mode)

  :functions
  (agent-shell-attention-render-active
   agent-shell-attention-notify-default
   agent-shell-attention--clear-busy
   agent-shell-attention--handle-success
   agent-shell-attention--handle-failure
   agent-shell-attention--mark-busy
   agent-shell-attention--clear-buffer
   agent-shell-attention--around-send-command)

  :custom
  (agent-shell-attention-notify-function #'agent-shell-attention-notify-default)
  (agent-shell-attention-render-function #'agent-shell-attention-render-active)
  (agent-shell-attention-show-zeros t)

  :init
  (agent-shell-attention-mode +1)

  :config
  ;; The upstream `agent-shell-attention--around-send-command' uses
  ;; `cl-letf*' to temporarily monkey-patch `acp-send-request' so
  ;; that the request callbacks can be decorated with attention
  ;; tracking.  This mechanism is fragile and can silently fail under
  ;; native compilation or deferred compilation, causing the pending
  ;; count to stay at zero.
  ;;
  ;; Replace it with a more robust approach: use a dynamic variable
  ;; to communicate the buffer context, and permanently advise
  ;; `acp-send-request' to decorate callbacks when the variable is
  ;; bound.
  (defvar wgn/agent-shell-attention--requesting-buffer nil
    "Buffer that initiated the current `acp-send-request' call.
Bound dynamically by the around-advice on `agent-shell--send-command'.")

  (advice-remove #'agent-shell--send-command
                 #'agent-shell-attention--around-send-command)

  (defun wgn/agent-shell-attention--around-send-command (orig-fn &rest args)
    "Advice around `agent-shell--send-command'.
Marks the buffer as busy and sets a dynamic variable so the
`acp-send-request' advice can decorate the callbacks."
    (let* ((shell-buffer (plist-get args :shell-buffer))
           (buffer (and (bufferp shell-buffer) shell-buffer)))
      (if (not (buffer-live-p buffer))
          (apply orig-fn args)
        (agent-shell-attention--clear-buffer buffer)
        (agent-shell-attention--mark-busy buffer)
        (condition-case err
            (let ((wgn/agent-shell-attention--requesting-buffer buffer))
              (apply orig-fn args))
          (error
           (agent-shell-attention--clear-busy buffer)
           (signal (car err) (cdr err)))))))

  (advice-add #'agent-shell--send-command :around
              #'wgn/agent-shell-attention--around-send-command)

  (defun wgn/agent-shell-attention--around-acp-send (orig-fn &rest args)
    "Advice around `acp-send-request'.
When called in the context of an agent-shell command (indicated by
`wgn/agent-shell-attention--requesting-buffer'), wrap the :on-success
and :on-failure callbacks with attention tracking."
    (let ((buffer wgn/agent-shell-attention--requesting-buffer))
      (if (not (buffer-live-p buffer))
          (apply orig-fn args)
        (let* ((on-success (plist-get args :on-success))
               (on-failure (plist-get args :on-failure)))
          (setq args
                (plist-put args :on-success
                           (lambda (response)
                             (agent-shell-attention--clear-busy buffer)
                             (agent-shell-attention--handle-success
                              buffer response)
                             (when on-success
                               (funcall on-success response)))))
          (setq args
                (plist-put args :on-failure
                           (lambda (error raw-message)
                             (agent-shell-attention--clear-busy buffer)
                             (agent-shell-attention--handle-failure
                              buffer error raw-message)
                             (when on-failure
                               (funcall on-failure error raw-message)))))
          (apply orig-fn args)))))

  (advice-add #'acp-send-request :around
              #'wgn/agent-shell-attention--around-acp-send))

;;;; Convenient LLM-based quick lookup of thing at point.
(use-package gptel-quick
  :ensure t

  :commands
  (gptel-quick)

  :bind
  (:map embark-general-map
   ("?" . #'gptel-quick)))

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

;;;; Use project-local dependencies
(use-package envrc
  :ensure t

  :hook
  (after-init . envrc-global-mode)

  :custom
  (envrc-direnv-executable "@direnv@"))

;;;; Open code from Emacs in the web browser.
(use-package elsewhere
  :ensure t

  :commands
  (elsewhere-open
   elsewhere-build-url))

;;;; Send HTTP requests from Emacs.
(use-package verb
  :ensure t

  :defines
  (verb-command-map)

  :config
  ;; TODO: Is there a way to use `:bind-keymap' for this?
  (define-key org-mode-map (kbd "C-c r") verb-command-map))

;;;; Flymake support for Golang linting.
(use-package flymake-golangci
  :ensure t

  :commands
  (flymake-golangci-load-backend))

;;;; Basic Golang support.
;;
;; HACK: This is built-in to Emacs, but there is also a third-party
;;       go-mode which we will install next.
;;
;; NOTE: To enable snippet support in a project, type:
;;
;;       M-x add-dir-local-variable RET
;;       go-ts-mode RET
;;       eglot-workspace-configuration RET
;;       (:TextDocumentClientCapabilities (:completion (:completionItem (:snippetSupport t)))) RET
;;
;;       Then, save the file which is generated inside the root of
;;       your project folder.
(use-package go-ts-mode
  :mode
  ("go\\.mod\\'" . go-mod-ts-mode)

  :preface
  (defun wgn/go-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((go-mode go-ts-mode) .
                     ,(eglot-alternatives '("gopls"
                                            "@gopls@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (add-hook 'eglot-managed-mode-hook #'flymake-golangci-load-backend nil t)
    (eglot-ensure))

  :init
  ;; Open go files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(go-mode . go-ts-mode))

  ;; Set up eglot for go-ts-mode.
  (add-hook 'go-ts-mode-hook #'wgn/go-ts-mode-eglot-setup)

  ;; CamelCase aware editing operations.
  (add-hook 'go-ts-mode-hook #'subword-mode)

  :config
  (with-eval-after-load 'dape
    (add-to-list
     'dape-configs
     `(delve-unit-test
       modes (go-mode go-ts-mode)
       ensure dape-ensure-command
       fn dape-config-autoport
       ;; HACK: Expects dlv to be on $PATH.
       command "dlv"
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
               [])))))

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
                  (add-hook 'before-save-hook #'wgn/apply-gofmt nil t))))

;;;; Run Golang tests.
(use-package gotest
  :ensure t

  :commands
  (go-test-current-file
   go-test-current-test
   go-test-current-project
   go-test-current-benchmark
   go-run))

;;;; Support for Go in Emacs' built-in Grand Unified Debugger (GUD).
;;
;; TODO: Evaluate whether I prefer this or the `dape' integration.
(use-package go-dlv
  :ensure t

  :commands
  (dlv
   dlv-current-func))

;;;; Rust support.
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"

  :preface
  (defun wgn/rust-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((rust-mode rust-ts-mode) .
                     ,(eglot-alternatives '("rust-analyzer"
                                            "@rustanalyzer@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :custom
  (rust-mode-treesitter-derive t)

  :init
  ;; Open Rust files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(rust-mode . rust-ts-mode))

  ;; Set up eglot for rust-ts-mode.
  (add-hook 'rust-ts-mode-hook #'wgn/rust-ts-mode-eglot-setup))

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
                     ,(eglot-alternatives '(("poetry" "run" "pylsp")
                                            "pylsp"
                                            "@pylsp@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Open python files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode))

  ;; Set up eglot for python-ts-mode.
  (add-hook 'python-ts-mode-hook #'wgn/python-ts-mode-eglot-setup))

;;;; C# support.
(use-package csharp-ts-mode
  :mode "\\.cs\\'"

  :preface
  (defun wgn/csharp-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((csharp-mode csharp-ts-mode) .
                     ,(eglot-alternatives '(("OmniSharp" "--languageserver")
                                            ("OmniSharp" "--languageserver")))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Open go files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(csharp-mode . csharp-ts-mode))

  ;; Set up eglot for csharp-ts-mode.
  (add-hook 'csharp-ts-mode-hook #'wgn/csharp-ts-mode-eglot-setup)

  ;; CamelCase aware editing operations.
  (add-hook 'csharp-ts-mode-hook #'subword-mode))

;;;; Kotlin support.
(use-package kotlin-ts-mode
  :ensure t
  :mode "\\.kt\\'"

  :preface
  (defun wgn/kotlin-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((kotlin-mode kotlin-ts-mode) .
                     ,(eglot-alternatives
                       '("kotlin-language-server"
                         "@kotlinlsp@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Set up eglot for kotlin-ts-mode.
  (add-hook 'kotlin-ts-mode-hook #'wgn/kotlin-ts-mode-eglot-setup))

;;;; Java support.
(use-package java-ts-mode
  :defer t

  :preface
  (defun wgn/java-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Open java files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(java-mode . java-ts-mode))

  ;; Set up eglot for java-ts-mode.
  (add-hook 'java-ts-mode-hook #'wgn/java-ts-mode-eglot-setup))

;;;; Nix support.
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"

  :preface
  (defun wgn/nix-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((nix-mode) .
                     ,(eglot-alternatives
                       '("nil"
                         "@nil@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Set up eglot for nix-mode.
  (add-hook 'nix-mode-hook #'wgn/nix-mode-eglot-setup))

;;;; YAML support.
;;
;; NOTE: To load a custom YAML schema in a project, create a
;;       .dir-locals.el file including the following contents:
;;
;; ((yaml-ts-mode
;;   . ((eglot-workspace-configuration
;;       . (:yaml (:format (:enable t)
;;                 :validate t
;;                 :hover t
;;                 :completion t
;;                 :schemaStore (:enable t)
;;                 :schemas (https://example.com/yaml-schema
;;                           ["filename.yml" "filename.yaml"])))))))
(use-package yaml-ts-mode
  :mode
  (("\\.yaml\\'" . yaml-ts-mode)
   ("\\.yml\\'" . yaml-ts-mode))

  :preface
  (defun wgn/yaml-ts-mode-eglot-setup ()
    (setq-local tab-width 2)
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((yaml-mode yaml-ts-mode) .
                     ,(eglot-alternatives
                       '(("yaml-language-server" "--stdio")
                         ("@yamlls@" "--stdio")))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
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
                   `((terraform-mode) .
                     ,(eglot-alternatives
                       '(("terraform-ls" "serve")
                         ("@terraformls@" "serve")))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
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
                   `((mhtml-mode) .
                     ,(eglot-alternatives
                       '(("vscode-html-language-server" "--stdio")
                         ("html-languageserver" "--stdio")
                         ("@htmlls@" "--stdio")))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
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
      ;; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   `((tsx-mode tsx-ts-mode) .
                     ,(eglot-alternatives
                       '(("rass ts")
                         ("typescript-language-server" "--stdio")
                         ("@tsxls@" "--stdio"))))))
    (eglot-ensure))

  :init
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
      ;; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   `((js-mode js-ts-mode) .
                     ,(eglot-alternatives
                       '(("rass ts")
                         ("typescript-language-server" "--stdio")
                         ("@tsxls@" "--stdio"))))))
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

;;;; Typescript support.
(use-package typescript-ts-mode
  :mode "\\.ts\\'"

  :preface
  (defun wgn/typescript-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      ;; FIXME: Need to figure out how to change indent size.
      ;; (add-hook 'before-save-hook #'wgn/apply-eglot-format t t)
      (add-to-list 'eglot-server-programs
                   `((typescript-mode typescript-ts-mode) .
                     ,(eglot-alternatives
                       '(("rass ts")
                         ("typescript-language-server" "--stdio")
                         ("@tsxls@" "--stdio"))))))
    (eglot-ensure))

  :init
  ;; Set up eglot for typescript-ts-mode.
  (add-hook 'typescript-ts-mode-hook #'wgn/typescript-ts-mode-eglot-setup))

;;;; JSON support.
(use-package json-ts-mode
  :mode "\\.json\\'"

  :preface
  (defun wgn/json-ts-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((json-mode json-ts-mode) .
                     ,(eglot-alternatives
                       '(("vscode-json-language-server" "--stdio")
                         ("vscode-json-languageserver" "--stdio")
                         ("json-languageserver" "--stdio")
                         ("@jsonls@" "--stdio"))))))
    (eglot-ensure))

  :init
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
                   `((css-mode css-ts-mode) .
                     ,(eglot-alternatives
                       '(("vscode-css-language-server" "--stdio")
                         ("css-languageserver" "--stdio")
                         ("@cssls@" "--stdio")))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
    (eglot-ensure))

  :init
  ;; Open CSS files with tree-sitter support.
  (add-to-list 'major-mode-remap-alist
               '(css-mode . css-ts-mode))

  ;; Set up eglot for css-ts-mode.
  (add-hook 'css-ts-mode-hook #'wgn/css-ts-mode-eglot-setup))

;;;; TeX support.
(use-package auctex
  :ensure t

  :mode
  (("\\.tex\\'" . LaTeX-mode))

  :preface
  (defun wgn/latex-mode-eglot-setup ()
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `((LaTeX-mode tex-mode context-mode texinfo-mode bibtex-mode) .
                     ,(eglot-alternatives
                       '("digestif"
                         "texlab"
                         "@texlab@"))))
      (add-hook 'before-save-hook #'wgn/apply-eglot-format nil t))
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

;;;; Reading documents.
(use-package reader
  :ensure t

  :commands
  (reader-mode)

  :mode
  (("\\.epub\\'" . reader-mode)
   ("\\.pdf\\'" . reader-mode)
   ("\\.mobi\\'" . reader-mode)))

;;;; Convert buffers to HTML (including org-mode).
(use-package htmlize
  :ensure t

  :commands
  (htmlize-buffer
   htmlize-file
   htmlize-many-files
   htmlize-many-files-dired))

;;;; Indicate the git diff in the margin.
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode

  :commands
  (global-git-gutter-mode)

  :init
  (global-git-gutter-mode +1))

;;;; Color scheme.
(use-package doom-themes
  :ensure t

  :commands
  (doom-themes-visual-bell-config
   doom-themes-org-config
   doom-themes-set-faces)

  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :init
  (if (memq system-type '(darwin))
      (use-package auto-dark
        :ensure t
        :diminish auto-dark-mode

        :commands
        (auto-dark-mode)

        :custom
        (auto-dark-themes '((doom-solarized-dark) (doom-solarized-light)))
        (auto-dark-dark-theme 'doom-solarized-dark)
        (auto-dark-light-theme 'doom-solarized-light)

        :preface
        (defun wgn/enable-auto-dark ()
          (auto-dark-mode)
          ;; Only run it once.
          (remove-hook 'server-after-make-frame-hook #'wgn/enable-auto-dark)
          (remove-hook 'after-init-hook #'wgn/enable-auto-dark))

        :init
        (if (daemonp)
            (add-hook 'server-after-make-frame-functions #'wgn/enable-auto-dark)
          (add-hook 'after-init-hook #'wgn/enable-auto-dark)))
    (load-theme 'doom-solarized-dark :no-confirm))

  (custom-set-faces
   ;; Fix annoyingly bright tab character:
   '(whitespace-tab ((t (:background unspecified)))))

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;;; Highlight the current line.
(use-package hl-line
  :hook
  ((text-mode prog-mode) . hl-line-mode))

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

;;;; Contact database.
(use-package bbdb
  :ensure t

  :hook
  ((gnus-startup-hook . bbdb-instantiate-gnus))

  :commands
  (bbdb-complete-mail
   bbdb-initialize)

  :custom
  (bbdb/mail-auto-create-p t)
  (bbdb/news-auto-create-p t)

  :config
  (bbdb-initialize 'message 'gnus 'sendmail 'anniv))

;;;; Godbolt-like compiler explorer in Emacs.
(use-package rmsbolt
  :ensure t

  :commands
  (rmsbolt))

(provide 'default)

;;; default.el ends here
