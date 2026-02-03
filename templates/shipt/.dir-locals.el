;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((python-ts-mode
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
      . (:nil
         (:formatting
          (:command ["alejandra"]))))))
 (yaml-ts-mode
  . ((eglot-workspace-configuration
      . (:yaml
         (:format
          (:enable t)
          :schemaStore
          (:enable t)
          :validate t
          :hover t
          :completion t
          :schemas
          (https://infraspec-json-schema.gcp.shipttech.com/spec
           ["infraspec.yml" "infraspec.yaml"])))))))
