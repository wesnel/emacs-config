;;; review-guide-send.el --- Where review feedback can go -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Find the agents and terminals working in a repository, so that
;; review feedback on it can be sent straight to whoever is making the
;; change.  Each kind of target comes from one function in
;; `review-guide-send-target-functions'; a function for a package that
;; is not loaded finds nothing.

;;; Code:

(require 'cl-lib)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell-project")
(declare-function agent-shell-insert "agent-shell")
(declare-function ghostel-paste-string "ghostel")

(defcustom review-guide-send-target-functions
  '(review-guide-send-agent-shell-targets
    review-guide-send-ghostel-targets)
  "Functions that find where feedback on a repository can go.
Each is called with the repository's directory and returns a list of
\(LABEL . SEND), where SEND is called with the feedback's text."
  :type '(repeat function)
  :group 'review-guide)

(defun review-guide-send--related-p (directory root)
  "Return non-nil when work in DIRECTORY is work on the repository at ROOT.
That is when DIRECTORY is inside ROOT, or is the top of a repository
that holds ROOT, such as the main checkout a worktree is nested in.  A
plain ancestor such as the home directory does not count."
  (and directory
       (or (file-in-directory-p directory root)
           (and (file-in-directory-p root directory)
                (file-exists-p (expand-file-name ".git" directory))))))

(defun review-guide-send--buffers (mode root)
  "Return the live buffers in MODE whose directory is related to ROOT."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p mode)
            (review-guide-send--related-p default-directory root))))
   (buffer-list)))

(defun review-guide-send-agent-shell-targets (root)
  "Return the agent-shell sessions working in ROOT.
Feedback is submitted as the session's next prompt."
  (when (fboundp 'agent-shell-buffers)
    (cl-loop for buffer in (agent-shell-buffers)
             when (review-guide-send--related-p
                   (with-current-buffer buffer (agent-shell-cwd)) root)
             collect (cons (format "agent-shell: %s" (buffer-name buffer))
                           (lambda (text)
                             (agent-shell-insert :text text :submit t
                                                 :shell-buffer buffer))))))

(defun review-guide-send-ghostel-targets (root)
  "Return the Ghostel terminals whose directory is in ROOT.
Feedback is pasted as one bracketed paste and not submitted, since
the program reading it is unknown; submit it in the terminal."
  (when (fboundp 'ghostel-paste-string)
    (mapcar (lambda (buffer)
              (cons (format "Paste into terminal: %s" (buffer-name buffer))
                    (lambda (text)
                      (with-current-buffer buffer
                        (ghostel-paste-string text))
                      (pop-to-buffer buffer))))
            (review-guide-send--buffers 'ghostel-mode root))))

(defun review-guide-send-targets (root)
  "Return where feedback on the repository at ROOT can go.
Each target is (LABEL . SEND).  Copying to the kill ring is always
offered, last."
  (append (mapcan (lambda (find) (funcall find root))
                  review-guide-send-target-functions)
          (list (cons "Copy to the kill ring" #'kill-new))))

(provide 'review-guide-send)
;;; review-guide-send.el ends here
