;;; eca.el --- ECA workspace host tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(ert-deftest wgn-eca-remote-workspace-from-local-chat ()
  (let ((default-directory "/tmp/")
        (eca-custom-command '("local-sandbox")))
    (should (equal
             (wgn/eca--call-on-workspace-host
              (lambda () (list default-directory eca-custom-command))
              "/ssh:example:/srv/project/")
             '("/ssh:example:/srv/project/" nil)))
    (should (equal default-directory "/tmp/"))
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
