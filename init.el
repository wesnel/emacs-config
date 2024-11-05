;;; init.el --- Wesley's Gnus Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Wesley Nelson

;; Author: Wesley Nelson <wgn@wgn.dev>
;; Maintainer: Wesley Nelson <wgn@wgn.dev>
;; URL: https://git.sr.ht/~wgn/emacs-config

;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; NOTE: ~/.authinfo.gpg must contain credentials for your email
;;       account(s).
;;
;; NOTE: Please set the following variables with customize:
;;; `user-mail-address' with your primary email address.
;;;; Can alternatively be set via the EMAIL environment variable.
;;;; This should match the email address of your PGP key.
;;; `user-full-name' with your full name.
;;;; Can alternatively be set via the NAME environment variable.
;;; `mml-secure-openpgp-signers' with the PGP key ID(s) that you want
;;; to use for signing/encryption.

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

(require 'gnus)
(require 'gnus-async)
(require 'gnus-dired)
(require 'gnus-msg)

;;;; Gnus source configuration:
;; TODO: Make configurable via Nix.
(setq gnus-select-method '(nnnil nil))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "fastmail"
                      (nnimap-address "imap.fastmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-inbox "nnimap+fastmail:INBOX")
                      (nnmail-expiry-target "nnimap+fastmail:Trash")
                      (nnmail-expiry-wait 90)))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnimap-inbox "nnimap+gmail:INBOX")
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)))
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gwene.org"))
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.io"))

(setq gnus-parameters '(("fastmail"
                         (gcc-self . "nnimap+fastmail:Sent"))
                        ("gmail"
                         (gcc-self . "nnimap+gmail:[Gmail]/Sent Mail")))
      gnus-posting-styles '((".*"
                             (signature-file "~/.signature"))
                            ("fastmail"
                             (address "wgn@wgn.dev")
                             ("X-Message-SMTP-Method" "smtp smtp.fastmail.com 587"))
                            ("gmail"
                             (address "wesley.nelson@shipt.com")
                             ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))))

;;;; Gnus general configuration:
(setq gnus-use-cache t
      gnus-asynchronous t
      gnus-use-header-prefetch t
      gnus-gcc-mark-as-read t
      gnus-use-trees t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number)
                                   gnus-thread-sort-by-score))
(add-hook 'dired-mode-hook #'gnus-dired-mode)
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

;;;; Appearance:
(setq gnus-sum-thread-tree-false-root " "
      gnus-sum-thread-tree-indent "  "
      gnus-sum-thread-tree-root "r "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-vertical "|"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf "╰─► "

      ;; │06-Jan│  Sender Name  │ Email Subject
      gnus-summary-line-format (concat "%0{%U%R%z%}"
                                       "%3{│%}" "%1{%d%}" "%3{│%}"
                                       "  "
                                       "%4{%-20,20f%}"
                                       "  "
                                       "%3{│%}"
                                       " "
                                       "%1{%B%}"
                                       "%s\n"))

;;;; Encryption/signing configuration:
(setq gnus-message-replysign 't
      gnus-message-replyencrypt 't
      gnus-buttonized-mime-types '("multipart/signed"))

(provide 'init)

;;; init.el ends here
