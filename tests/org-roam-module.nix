# Checks the org-roam notes file that the Home Manager module writes.
# The module is evaluated with stand-ins for the Home Manager options it
# sets, not with Home Manager itself, which this flake does not depend on.
{
  lib,
  pkgs,
  runCommand,
  writeText,
  emacs-nox,
  module,
}: let
  file = ".emacs.d/etc/org-roam-notes.el";

  # Quotes and a backslash, which the file must write as an Elisp string.
  directory = "/n o\"t\\es";

  stubs = {
    options = {
      home.homeDirectory = lib.mkOption {type = lib.types.str;};
      home.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = {};
      };
      home.packages = lib.mkOption {
        type = lib.types.listOf lib.types.anything;
        default = [];
      };
      home.file = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = {};
      };
      sops = lib.mkOption {
        type = lib.types.anything;
        default = {};
      };
    };
  };

  files = settings:
    (lib.evalModules {
      modules = [
        stubs
        module
        {
          _module.args.pkgs = pkgs;
          home.homeDirectory = "/home/me";
          home.programs.wgn.emacs = {enable = true;} // settings;
        }
      ];
    })
    .config
    .home
    .file;

  refused = value: !(builtins.tryEval (builtins.hasAttr file (files {org-roam.directory = value;}))).success;

  machineFile = writeText "org-roam-notes.el" (files {org-roam.directory = directory;}).${file}.text;
in
  assert lib.assertMsg (!(builtins.hasAttr file (files {}))) "a null org-roam.directory still writes ${file}";
  assert lib.assertMsg (refused "") "org-roam.directory accepts \"\"";
  assert lib.assertMsg (refused "notes") "org-roam.directory accepts a relative path";
    runCommand "org-roam-module" {
      nativeBuildInputs = [emacs-nox];
      inherit directory machineFile;
    } ''
      export HOME="$TMPDIR"
      emacs -Q --batch --eval '
        (progn
          (defvar wgn/agent-plans-org-roam-directory nil)
          (load (getenv "machineFile") nil t)
          (unless (equal wgn/agent-plans-org-roam-directory (getenv "directory"))
            (message "%s sets %S, not %S" (getenv "machineFile")
                     wgn/agent-plans-org-roam-directory (getenv "directory"))
            (kill-emacs 1)))'
      touch "$out"
    ''
