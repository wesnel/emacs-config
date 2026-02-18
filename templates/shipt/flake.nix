{
  description = "FIXME";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }: let
    default = final: prev: {};
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        overlays = [
          default
        ];

        inherit
          system
          ;
      };
    in {
      packages = {};

      devShells = {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            alejandra
            delve
            go
            gopls
            gotools
            go-tools
            nil
          ];
        };
      };

      formatter = pkgs.alejandra;
    })
    // flake-utils.lib.eachDefaultSystemPassThrough (system: {
      overlays = {
        inherit
          default
          ;
      };

      nixosModules = {
        default = {...}: {};
      };
    });
}
