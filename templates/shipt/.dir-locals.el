;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((agent-shell-mode
  . ((agent-shell-github-command
      (append agent-shell-github-command
              '("--additional-mcp-config" "@mcp-config.json")))))
 (go-ts-mode
  . ((eval
      . (progn
          (with-eval-after-load 'agent-shell
            (add-to-list
             'agent-shell-mcp-servers
             `((name . "language-server")
               (command . "mcp-language-server")
               (args . ("--workspace" ,(directory-file-name
                                        (expand-file-name
                                         (project-root (project-current))))
                        "--lsp" "gopls"))
               (env . (((name . "LOG_LEVEL") (value . "info")))))))
          (with-eval-after-load 'mcp-hub
            (add-to-list
             'mcp-hub-servers
             `("language-server" .
               (:command
                "mcp-language-server"
                :args
                ("--workspace" ,(directory-file-name
                                 (expand-file-name
                                  (project-root (project-current))))
                 "--lsp" "gopls")
                :env
                (:LOG_LEVEL "info")))))
          (with-eval-after-load 'dape
            (add-to-list
             'dape-configs
             `(delve-unit-test
               modes (go-mode go-ts-mode)
               ensure dape-ensure-command
               fn dape-config-autoport
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
                       []))))))))
 (python-ts-mode
  . ((eglot-workspace-configuration
      . (:pylsp
         (:signature
          (:formatter "ruff"))
         (:plugins
          (:pydocstyle
           (:enabled t))
          (:pylint
           (:enabled t)))))))
 (nix-mode
  . ((eglot-workspace-configuration
      . (:nil (:formatting (:command ["alejandra"]))))))
 (yaml-ts-mode
  . ((eglot-workspace-configuration
      . (:yaml (:format (:enable t)
                        :validate t
                        :hover t
                        :completion t
                        :schemaStore (:enable t)
                        :schemas (https://infraspec-json-schema.gcp.shipttech.com/spec
                                  ["infraspec.yml" "infraspec.yaml"])))))))
