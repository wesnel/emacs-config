;;; eca.el --- ECA workspace host tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
;; A daemon started without TRAMP's autoloaded handler sees no remote paths.
(require 'tramp)
(load (expand-file-name "../lisp/wgn-eca.el"
                        (file-name-directory load-file-name)) nil t)

(ert-deftest wgn-eca-local-workspace-uses-configured-command ()
  (let ((default-directory "/tmp/")
        (eca-custom-command '("sandbox" "--image" "eca:latest")))
    (should (equal
             (wgn/eca--call-on-workspace-host
              (lambda (session callback)
                (list default-directory eca-custom-command session callback))
              "/tmp/project/" 'session 'callback)
             '("/tmp/project/" ("sandbox" "--image" "eca:latest")
               session callback)))
    (should (equal default-directory "/tmp/"))
    (should (equal eca-custom-command '("sandbox" "--image" "eca:latest")))))

(defmacro wgn-eca-test--with-remote-executables (executables &rest body)
  "Run BODY with `executable-find' finding only EXECUTABLES remotely.
EXECUTABLES maps a name to the local part of its remote path. Local
lookups, which TRAMP makes itself, are left to the real function."
  (declare (indent 1))
  (let ((local (make-symbol "local")))
    `(let ((,local (symbol-function 'executable-find)))
       (cl-letf (((symbol-function 'executable-find)
                  (lambda (command &optional remote)
                    (if (and remote (file-remote-p default-directory))
                        (alist-get command ,executables nil nil #'equal)
                      (funcall ,local command)))))
         ,@body))))

(ert-deftest wgn-eca-remote-workspace-from-local-chat ()
  (let ((default-directory "/tmp/")
        (eca-custom-command '("local-sandbox")))
    (wgn-eca-test--with-remote-executables nil
      (should (equal
               (wgn/eca--call-on-workspace-host
                (lambda () (list default-directory eca-custom-command))
                "/ssh:example:/srv/project/")
               '("/ssh:example:/srv/project/" nil))))
    (should (equal default-directory "/tmp/"))
    (should (equal eca-custom-command '("local-sandbox")))))

(ert-deftest wgn-eca-remote-workspace-uses-remote-sandbox ()
  (let ((default-directory "/tmp/")
        (eca-custom-command '("local-sandbox")))
    (wgn-eca-test--with-remote-executables
        '(("eca-sandbox" . "/home/me/.nix-profile/bin/eca-sandbox"))
      (should (equal
               (wgn/eca--call-on-workspace-host
                (lambda () eca-custom-command)
                "/ssh:example:/srv/project/")
               '("/home/me/.nix-profile/bin/eca-sandbox"))))
    (should (equal eca-custom-command '("local-sandbox")))))

(ert-deftest wgn-eca-local-workspace-from-remote-chat ()
  (let ((default-directory "/ssh:example:/srv/chat/")
        (eca-custom-command '("local-sandbox")))
    (should (equal
             (wgn/eca--call-on-workspace-host
              (lambda () (list default-directory eca-custom-command))
              "/tmp/project/")
             '("/tmp/project/" ("local-sandbox"))))
    (should (equal default-directory "/ssh:example:/srv/chat/"))))

;;; eca.el ends here
