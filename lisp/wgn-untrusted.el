;;; wgn-untrusted.el --- Keep untrusted checkouts from running code -*- lexical-binding: t; -*-

(defcustom wgn/untrusted-directories
  (list (expand-file-name "pr-review/" (or (getenv "XDG_CACHE_HOME") "~/.cache")))
  "Directories whose files must not run code through Emacs.
Buffers under them start no language server, no direnv and no version
control, and ignore file-local and directory-local variables.

nix-config's `pr-loop review' clones pull requests under the default
entry, $XDG_CACHE_HOME/pr-review/.  Home Manager's
`home.programs.wgn.emacs.untrustedDirectories' sets this from outside
Emacs; give it the directory pr-loop uses."
  :type '(repeat directory)
  :group 'files)

(defun wgn/untrusted--under-p (path roots)
  "Return non-nil if PATH or one of its parents has an identifier in ROOTS."
  (let (found)
    (while (and path (not found))
      (setq found (member (file-attribute-file-identifier (file-attributes path))
                          roots))
      (let ((parent (file-name-directory (directory-file-name path))))
        (setq path (unless (equal parent path) parent))))
    found))

(defun wgn/untrusted-dir-p (file)
  "Return non-nil if FILE is in one of `wgn/untrusted-directories'.
Directories are compared by inode, and FILE is checked both as named
and with symlinks resolved: a symlink into an untrusted directory, or
out of one, still counts as inside it."
  (when-let* ((roots (delq nil (mapcar (lambda (dir)
                                         (file-attribute-file-identifier
                                          (file-attributes (file-truename dir))))
                                       wgn/untrusted-directories))))
    (let ((file (expand-file-name file)))
      (and (or (wgn/untrusted--under-p file roots)
               (wgn/untrusted--under-p (file-truename file) roots))
           t))))

(defun wgn/untrusted-buffer-p ()
  "Return non-nil if the current buffer is in an untrusted directory.
The visited file and `default-directory' can differ, through a symlink
or `cd', and each picks the project, direnv and .dir-locals.el, so
either being untrusted is enough."
  (or (and buffer-file-name (wgn/untrusted-dir-p buffer-file-name))
      (and default-directory (wgn/untrusted-dir-p default-directory))))

(defun wgn/untrusted--skip (&rest _)
  "Return non-nil, skipping the advised function, in untrusted buffers."
  (wgn/untrusted-buffer-p))

(defun wgn/untrusted--refuse-eglot (&rest _)
  "Refuse to start a language server for an untrusted buffer."
  ;; Refuse before `eglot' prompts for a server.
  (interactive (lambda (spec)
                 (wgn/untrusted--refuse-eglot)
                 (advice-eval-interactive-spec spec)))
  (when (wgn/untrusted-buffer-p)
    (user-error "Not starting a language server in untrusted %s"
                (abbreviate-file-name default-directory))))

(defun wgn/untrusted--without-local-variables (fn &rest args)
  "Call FN with ARGS, ignoring local variables in untrusted buffers."
  (let ((enable-local-variables
         (and (not (wgn/untrusted-buffer-p)) enable-local-variables)))
    (apply fn args)))

(defun wgn/untrusted--without-vc (&rest _)
  "Turn version control off in an untrusted buffer.
VC runs `git status' on a visited file, which runs the filters the
checkout's .gitattributes names, such as Git LFS from the user's own
configuration.  diff-hl asks VC for the backend, so it stays off too."
  (when (wgn/untrusted-buffer-p)
    (setq-local vc-handled-backends nil)))

;; So that a later change of major mode keeps VC off.  Only untrusted
;; buffers have a local value.
(put 'vc-handled-backends 'permanent-local t)
;; Before the mode and its hooks, and before `find-file-hook' runs
;; `vc-refresh-state'.
(advice-add 'set-auto-mode :before #'wgn/untrusted--without-vc)
;; Forge asks git for the repository's remotes on every visited file.
(advice-add 'forge-bug-reference-setup :before-until #'wgn/untrusted--skip)
(advice-add 'eglot :before #'wgn/untrusted--refuse-eglot)
(advice-add 'eglot-ensure :before-until #'wgn/untrusted--skip)
(advice-add 'envrc-mode :before-until #'wgn/untrusted--skip)
;; Between them these read every file-local and dir-local setting,
;; including a mode named on the -*- line.
(dolist (fn '(set-auto-mode hack-local-variables hack-dir-local-variables))
  (advice-add fn :around #'wgn/untrusted--without-local-variables))

(provide 'wgn-untrusted)
;;; wgn-untrusted.el ends here
