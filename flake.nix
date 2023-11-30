{
  description = "Wesley's Emacs/Nix Configuration";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };

  outputs =
    { self
    , emacs-overlay
    , nixpkgs
    , ... }:

    let

      # Load custom packages into nixpkgs.
      overlays = [
        (import ./overlays)
        (import emacs-overlay)

        (final:
          prev:

          {
            parinfer-rust = prev.parinfer-rust.overrideAttrs (old:

              {
                # HACK: On Mac, the file has the extension ".dylib",
                #       but it needs to be ".so":
                postInstall = ''
                  ${old.postInstall}

                  if [ -e $out/lib/libparinfer_rust.dylib ]
                    then cp $out/lib/libparinfer_rust.dylib $out/lib/libparinfer_rust.so
                  fi
                '';
              });
          })
      ];

      # System types to support.
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit
          overlays
          system;
      });

    in forAllSystems (system:

      let
        pkgs = nixpkgsFor.${system};
      in {
        packages = pkgs.wgn-emacs;

        nixosModules.default =
          { config
          , lib
          , ... }:

          let
            cfg = config.programs.wgn.emacs;
          in {
            options.programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable Wesley's Emacs Configuration";

              package = lib.mkPackageOption pkgs "wgn-emacs-unstable-nox" {
                default = self.packages.${system}.wgn-emacs-unstable-nox;
              };
            };

            config = lib.mkIf cfg.enable {
              home.file.".emacs.d/early-init.el".source = ./early-init.el;

              environment = {
                pathsToLink = [
                  "/share/hunspell"
                ];

                systemPackages = let
                  pkg = cfg.package;
                in (with pkgs; [
                  pkg
                  enchant2
                  nuspell
                ]) ++ (with pkgs.hunspellDicts; [
                  en-us-large
                ]);
              };
            };
          };
      });
}
