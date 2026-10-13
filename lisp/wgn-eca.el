;;; wgn-eca.el --- ECA workspace host selection -*- lexical-binding: t; -*-

(defvar eca-custom-command nil)
(declare-function eca--session-workspace-folders "eca-util" (session))

(defun wgn/eca--call-on-workspace-host (call workspace &rest args)
  "Call CALL with ARGS on the host of WORKSPACE.
Use the configured command locally. Over TRAMP, use the remote host's
`eca-sandbox' when it has one, and otherwise let ECA resolve `eca'."
  (let* ((default-directory (or workspace default-directory))
         (eca-custom-command
          (if (file-remote-p default-directory)
              (when-let* ((sandbox (executable-find "eca-sandbox" t)))
                (list sandbox))
            eca-custom-command)))
    (apply call args)))

(defun wgn/eca--start-on-workspace-host (start session &rest args)
  "Call START for SESSION on the host of its first workspace."
  (apply #'wgn/eca--call-on-workspace-host
         start (car (eca--session-workspace-folders session)) session args))

(declare-function eca "eca")

(defun wgn/eca-unsandboxed ()
  "Start an ECA session on this machine without the sandbox.
The session has every skill, including those that drive this Emacs
through emacsclient, and the same access to this machine as Emacs
itself. An ECA session already running for the workspace is reused as
it is, so stop it first."
  (interactive)
  (when (file-remote-p default-directory)
    (user-error "Unsandboxed sessions run only in local workspaces"))
  (let ((eca-custom-command nil))
    (call-interactively #'eca)))

(defvar wgn/eca-pr-loop-client nil
  "Directory holding the sandbox's `pr-loop' client, or nil when the
bot review loop is not set up on this machine.")

(defun wgn/eca-pr-loop ()
  "Start a sandboxed ECA session for the bot review loop.
The sandbox runs with `--pr-loop', which puts `sandbox-relay serve'
beside it and the `pr-loop' guard before every tool call; ordinary
sessions run without either. Choose the `pr-coordinator' agent in the
chat.

An ECA session already running for the workspace is reused as it is,
without the relay, so stop it first. The review-loop skill refuses to
run in a session that lacks it."
  (interactive)
  (cond
   ((not wgn/eca-pr-loop-client)
    (user-error "The bot review loop is not set up on this machine"))
   ((not eca-custom-command)
    (user-error "The bot review loop needs the local ECA sandbox"))
   ((file-remote-p default-directory)
    (user-error "The bot review loop runs only in local workspaces")))
  (let ((eca-custom-command
         (append eca-custom-command (list "--pr-loop" wgn/eca-pr-loop-client))))
    (call-interactively #'eca)))

(provide 'wgn-eca)
;;; wgn-eca.el ends here
