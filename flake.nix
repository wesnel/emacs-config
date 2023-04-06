{
  description = "Wesley's Emacs/Nix Configuration";

  inputs = {
    emacs-overlay = {
      url = github:nix-community/emacs-overlay;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-unstable;
    };
  };

  outputs = inputs@
    { self
    , emacs-overlay
    , flake-utils
    , nixpkgs }: flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;

        overlays = [
          (import emacs-overlay)
        ];
      };

      aspell = let
        pkg = pkgs.aspellWithDicts (d: with d; [ en en-computers en-science ]);
      in "${pkg}/bin/aspell";

      goimports = let
        pkg = pkgs.gotools;
      in "${pkg}/bin/goimports";
    in {
      packages = rec {
        default = emacs-config;

        emacs-config = pkgs.emacsWithPackagesFromUsePackage {
          package = pkgs.emacsGit-nox;
          config = ./default.el;

          defaultInitFile = pkgs.substituteAll {
            name = "default.el";
            src = ./default.el;

            inherit (pkgs)
              pass;
            inherit
              aspell
              goimports;
          };

          override = epkgs: epkgs // {
            chatgpt-shell = pkgs.callPackage ./overrides/chatgpt-shell {
              inherit (pkgs) fetchFromGitHub;
              inherit (epkgs) trivialBuild markdown-mode;
            };
          };
        };
      };
    });
}
