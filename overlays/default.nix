final: prev:

{
  wgn-emacs = let
    aspell = let
      pkg = final.aspellWithDicts (d: with d; [ en en-computers en-science ]);
    in "${pkg}/bin/aspell";

    goimports = let
      pkg = final.gotools;
    in "${pkg}/bin/goimports";

    gopls = let
      pkg = final.gopls;
    in "${pkg}/bin/gopls";

    multimarkdown = let
      pkg = final.multimarkdown;
    in "${pkg}/bin/multimarkdown";

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

    emacs-config = final.substituteAll {
      name = "default.el";
      src = ../default.el;

      inherit
        aspell
        goimports
        gopls
        multimarkdown
        pylsp;
    };

    wgn-emacs = final.emacsWithPackagesFromUsePackage {
      config = ../default.el;
      package = final.emacsGit-nox;
      defaultInitFile = emacs-config;

      override = eFinal: ePrev: {
        chatgpt-shell = let
          rev = "60e3b05220acff858a5b6fc43b8fa49dd886548a";
          sha256 = "sha256-hXt2KClUvZa8M6AobUrpSBUtf4uk4WiLO/tHtc6eSuE=";

          packageRequires = with eFinal; [
            markdown-mode
          ];
        in eFinal.trivialBuild {
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
        in eFinal.trivialBuild {
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

        no-littering = let
          rev = "ef02b6fcedd97f3ab039b51411fdaab7336d819b";
          sha256 = "sha256-a3vCZzBUtSJ2EA/wyRfkLpteByZoSUbagiQ8hyJjrsQ=";
        in ePrev.no-littering.overrideAttrs (old:

          {
            src = final.fetchFromGitHub {
              owner = "emacscollective";
              repo = "no-littering";

              inherit
                rev
                sha256;
            };
          });
      };
    };
  in {
    inherit
      emacs-config
      wgn-emacs;

    defaultPackage = wgn-emacs;
  };
}
