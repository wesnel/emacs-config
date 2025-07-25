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
      // (import emacs-overlay final prev)
      // {
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
          wgn-emacs-pgtk
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
      };

      homeManagerModules = {
        default = {
          config,
          lib,
          pkgs,
          ...
        }: let
          cfg = config.home.programs.wgn.emacs;
        in {
          options = {
            home.programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable the Home Manager portion of Wesley's Emacs Configuration";

              gnus = {
                enable = lib.mkEnableOption "Enable Wesley's Emacs Gnus Configuration with Home Manager";

                # TODO: Add options to customize IMAP server, etc.
              };

              llm = {
                enable = lib.mkEnableOption "Enable LLM integration for Emacs";
              };
            };
          };

          config = lib.mkIf cfg.enable {
            xdg.configFile = {
              "fish/conf.d/emacs-vterm.fish" = {
                enable = config.programs.fish.enable;
                source = "${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/vterm-${pkgs.emacsPackages.vterm.version}/etc/emacs-vterm.fish";
              };
            };

            services = lib.mkIf cfg.llm.enable {
              ollama = {
                enable = true;
                # TODO: Substitute the port into the Emacs configuration.
                port = 11434;
              };
            };

            home = {
              file = {
                ".emacs.d/etc/gnus/init.el" = lib.mkIf cfg.gnus.enable {
                  source =
                    pkgs.replaceVars ./init.el {};
                };

                ".emacs.d/early-init.el".source = ./early-init.el;
                ".emacs.d/etc/eshell/login".source = ./login.el;
              };
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
            fonts.packages = with pkgs.nerd-fonts; [
              comic-shanns-mono
            ];

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
