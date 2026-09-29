;;; wgn-eca.el --- ECA workspace host selection -*- lexical-binding: t; -*-

(defvar eca-custom-command nil)
(declare-function eca--session-workspace-folders "eca-util" (session))

(defun wgn/eca--call-on-workspace-host (call workspace &rest args)
  "Call CALL with ARGS on the host of WORKSPACE.
Use the configured command locally and resolve `eca' over TRAMP."
  (let* ((default-directory (or workspace default-directory))
         (eca-custom-command
          (unless (file-remote-p default-directory)
            eca-custom-command)))
    (apply call args)))

(defun wgn/eca--start-on-workspace-host (start session &rest args)
  "Call START for SESSION on the host of its first workspace."
  (apply #'wgn/eca--call-on-workspace-host
         start (car (eca--session-workspace-folders session)) session args))

(provide 'wgn-eca)
;;; wgn-eca.el ends here
