{
  description = "Wesley's Emacs/Nix Configuration";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };

  outputs = {
    emacs-overlay,
    nixpkgs,
    flake-utils,
    ...
  }: let
    default = final: prev:
      (import ./overlays final prev)
      // (import emacs-overlay final prev);
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
      packages = with pkgs; {
        inherit
          emacs-config
          emacs-config-dynamic
          wgn-emacs
          wgn-emacs-nox
          wgn-emacs-macport
          wgn-emacs-macport-cross
          wgn-emacs-unstable
          wgn-emacs-unstable-nox
          wgn-emacs-git
          wgn-emacs-git-nox
          ;

        default = wgn-emacs;
      };

      devShells = {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            alejandra
            nil
          ];
        };
      };

      formatter = pkgs.alejandra;
    })
    // flake-utils.lib.eachDefaultSystemPassThrough (system: {
      overlays = {
        inherit default;

        emacs = final: prev: {
          emacs =
            if final.stdenv.isDarwin
            then final.wgn-emacs-macport
            else final.wgn-emacs-unstable;

          parinfer-rust-emacs = prev.parinfer-rust-emacs.overrideAttrs (old: {
            # HACK: On Mac, the file has the extension ".dylib",
            #       but it needs to be ".so":
            postInstall = ''
              ${old.postInstall or ""}

              if [ -e $out/lib/libparinfer_rust.dylib ]
                then cp $out/lib/libparinfer_rust.dylib $out/lib/libparinfer_rust.so
              fi
            '';
          });
        };
      };

      homeManagerModules = {
        default = {
          config,
          lib,
          ...
        }: let
          cfg = config.home.programs.wgn.emacs;
        in {
          options = {
            home.programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable the Home Manager portion of Wesley's Emacs Configuration";
            };
          };

          config = lib.mkIf cfg.enable {
            home = {
              file.".emacs.d/early-init.el".source = ./early-init.el;
            };
          };
        };
      };

      nixosModules = {
        default = {
          config,
          lib,
          pkgs,
          ...
        }: let
          cfg = config.programs.wgn.emacs;
        in {
          options = {
            programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable the NixOS portion of Wesley's Emacs Configuration";

              package = lib.mkPackageOption pkgs "wgn-emacs-unstable-nox" {
                default = pkgs.emacs;
              };
            };
          };

          config = lib.mkIf cfg.enable {
            environment = {
              pathsToLink = [
                "/share/hunspell"
              ];

              systemPackages = let
                pkg = cfg.package;
              in
                (with pkgs; [
                  pkg
                  enchant2
                  nuspell
                ])
                ++ (with pkgs.hunspellDicts; [
                  en-us-large
                ]);
            };
          };
        };
      };
    });
}
