final: prev:

{
  emacs-config = let
    aspell = let
      pkg = final.aspellWithDicts (d: with d; [ en en-computers en-science ]);
    in "${pkg}/bin/aspell";

    goimports = let
      pkg = final.gotools;
    in "${pkg}/bin/goimports";

    gopls = let
      pkg = final.gopls;
    in "${pkg}/bin/gopls";

    pylsp = let
      pkg = final.python3.withPackages(p: with p; [
        jedi
        mccabe
        pycodestyle
        pyflakes
        python-lsp-server
        rope
        yapf
      ]);
    in "${pkg}/bin/pylsp";

    emacs-config = final.emacsWithPackagesFromUsePackage {
      package = final.emacsGit-nox;
      config = ../default.el;

      defaultInitFile = final.substituteAll {
        name = "default.el";
        src = ../default.el;

        inherit (final)
          pass;
        inherit
          aspell
          goimports
          gopls
          pylsp;
      };

      override = epkgs: epkgs // {
        chatgpt-shell = let
          rev = "60e3b05220acff858a5b6fc43b8fa49dd886548a";
          sha256 = "sha256-hXt2KClUvZa8M6AobUrpSBUtf4uk4WiLO/tHtc6eSuE=";

          packageRequires = with epkgs; [
            markdown-mode
          ];
        in epkgs.trivialBuild {
          pname = "chatgpt-shell";
          version = rev;

          src = final.fetchFromGitHub {
            owner = "xenodium";
            repo = "chatgpt-shell";

            inherit
              rev
              sha256;
          };

          inherit
            packageRequires;
        };

        devil = let
          rev = "98064ffed40a86def0049d87d8412a5049d14c31";
          sha256 = "sha256-yZPtYt/9QvAtMn/3lYpmctb+hB5WmyeTovjTqVU/nhQ=";
        in epkgs.trivialBuild {
          pname = "devil";
          version = rev;

          src = final.fetchFromGitHub {
            owner = "susam";
            repo = "devil";

            inherit
              rev
              sha256;
          };
        };
      };
    };
  in {
    inherit
      emacs-config;

    defaultPackage = emacs-config;
  };
}
