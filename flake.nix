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
    , nixpkgs }:
      flake-utils.lib.simpleFlake {
        inherit self nixpkgs;

        preOverlays = [
          (import emacs-overlay)
        ];

        name = "emacs-config";
        overlay = ./overlays;
        systems = flake-utils.lib.defaultSystems;
      };
}
