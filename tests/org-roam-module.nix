# Checks the machine file that the Home Manager module writes, which
# holds org-roam's notes directory and agent-plan's config file.  The
# module is evaluated with stand-ins for the Home Manager options it
# sets, not with Home Manager itself, which this flake does not depend on.
{
  lib,
  pkgs,
  runCommand,
  writeText,
  emacs-nox,
  module,
}: let
  file = ".emacs.d/etc/agent-plans.el";

  # Quotes and a backslash, which the file must write as Elisp strings.
  directory = "/n o\"t\\es";
  configFile = "/c o\"n\\fig.json";

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

  refused = settings: !(builtins.tryEval (builtins.hasAttr file (files settings))).success;

  machineFile = name: settings: writeText name (files settings).${file}.text;

  # Each file is loaded on its own, and must set exactly what it names.
  cases = {
    both = {
      settings = {
        org-roam.directory = directory;
        agent-tools.configFile = configFile;
      };
      expected = {inherit directory configFile;};
    };
    directory-only = {
      settings = {org-roam.directory = directory;};
      expected = {
        inherit directory;
        configFile = null;
      };
    };
    config-file-only = {
      settings = {agent-tools.configFile = configFile;};
      expected = {
        directory = null;
        inherit configFile;
      };
    };
  };

  check = name: case: ''
    emacs -Q --batch --eval '
      (progn
        (defvar wgn/agent-plans-org-roam-directory nil)
        (defvar wgn/agent-plans-config-file nil)
        (load "${machineFile name case.settings}" nil t)
        (let ((want (list (getenv "directory_${name}")
                          (getenv "configFile_${name}")))
              (got (list wgn/agent-plans-org-roam-directory
                         wgn/agent-plans-config-file)))
          (unless (equal got want)
            (message "${name}: the machine file sets %S, not %S" got want)
            (kill-emacs 1))))'
  '';

  # What each variable should hold, in the environment of the check: a
  # variable left unset is nil to getenv.
  expect = name: variable: value:
    lib.optionalAttrs (value != null) {"${variable}_${name}" = value;};
in
  assert lib.assertMsg (!(builtins.hasAttr file (files {}))) "null settings still write ${file}";
  assert lib.assertMsg (refused {org-roam.directory = "";}) "org-roam.directory accepts \"\"";
  assert lib.assertMsg (refused {org-roam.directory = "notes";}) "org-roam.directory accepts a relative path";
  assert lib.assertMsg (refused {agent-tools.configFile = "";}) "agent-tools.configFile accepts \"\"";
  assert lib.assertMsg (refused {agent-tools.configFile = "config.json";}) "agent-tools.configFile accepts a relative path";
    runCommand "org-roam-module" ({
        nativeBuildInputs = [emacs-nox];
      }
      // lib.concatMapAttrs (name: case:
        expect name "directory" case.expected.directory
        // expect name "configFile" case.expected.configFile)
      cases) ''
      export HOME="$TMPDIR"
      ${lib.concatStrings (lib.mapAttrsToList check cases)}
      touch "$out"
    ''
