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
;;; `user-mail-address' with your email address.
;;;; Can alternatively be set via the EMAIL environment variable.
;;;; This should match the email address of your PGP key and the email
;;;; address of your IMAP account in ~/.authinfo.gpg.
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
(setq gnus-select-method '(nnimap "fastmail"
                                  (nnimap-address "imap.fastmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-inbox "INBOX")
                                  (nnmail-expiry-target "nnimap+fastmail:Archive")
                                  (nnmail-expiry-wait 90))
      gnus-parameters `(("fastmail"
                         (gcc-self . "nnimap+fastmail:Sent")))
      gnus-message-archive-group "nnimap+fastmail:Sent")
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.io"))

;;;; Gnus general configuration:
(setq gnus-use-cache t
      gnus-asynchronous t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-number))
      gnus-posting-styles '((".*" (signature-file "~/.signature"))))
(add-hook 'dired-mode-hook #'gnus-dired-mode)
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
(add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

;;;; Encryption/signing configuration:
(setq gnus-message-replysign 't
      gnus-message-replyencrypt 't
      gnus-buttonized-mime-types '("multipart/signed"))

(provide 'init)

;;; init.el ends here
