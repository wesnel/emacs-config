final: prev:

let
  aspell = let
    pkg = final.aspellWithDicts (d: with d; [ en en-computers en-science ]);
  in "${pkg}/bin/aspell";

  goimports = let
    pkg = final.gotools;
  in "${pkg}/bin/goimports";

  gopls = let
    pkg = final.gopls;
  in "${pkg}/bin/gopls";

  emacs-config = final.emacsWithPackagesFromUsePackage {
    package = final.emacsGit-nox;
    config = ./default.el;

    defaultInitFile = final.substituteAll {
      name = "default.el";
      src = ./default.el;

      inherit (final)
        pass;
      inherit
        aspell
        goimports
        gopls;
    };

    override = epkgs: epkgs // {
      chatgpt-shell = final.callPackage ./overrides/chatgpt-shell {
        inherit (final)
          fetchFromGitHub;
        inherit (epkgs)
          trivialBuild
          markdown-mode;
      };
    };
  };
in {
  emacs-config = {
    inherit emacs-config;

    defaultPackage = emacs-config;
  };
}
