(require 'package)

(eval-when-compile
  (require 'use-package))

(use-package mml-sec
  :commands
  (mml-secure-message-sign))

(use-package message
  :commands
  (message-send-mail-with-sendmail))

(use-package smtpmail
  :commands
  (smtpmail-send-it))

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
;;; `smtpmail-smtp-server' with your IMAP server address.
;;;; Can alternatively be set via the SMTPSERVER environment variable.
;;;; This should match the server name of your IMAP account in
;;;; ~/.authinfo.gpg.
;;; `mml-secure-openpgp-signers' with the PGP key ID(s) that you want
;;; to use for signing/encryption.
(use-package gnus
  :custom
  (gnus-use-cache t)

  (gnus-message-replysign 't)
  (gnus-message-replyencrypt 't)
  (mm-verify-option 'always)
  (mm-decrypt-option 'always)
  (gnus-buttonized-mime-types '("multipart/signed"))
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-secure-smime-sign-with-sender t)

  (message-send-mail-function #'message-send-mail-with-sendmail)
  (send-mail-function #'smtpmail-send-it)

  (smtpmail-smtp-service 587)

  (gnus-select-method `(nnimap ,smtpmail-smtp-server
			       (nnimap-server-port imaps)))

  (mail-sources `((imap :server ,smtpmail-smtp-server
			:user   ,user-mail-address)))

  :init
  (add-hook 'message-setup-hook #'mml-secure-message-sign))
