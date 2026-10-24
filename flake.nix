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

    emacs-tramp-rpc = {
      url = "github:ArthurHeymans/emacs-tramp-rpc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    emacs-overlay,
    nixpkgs,
    flake-utils,
    emacs-tramp-rpc,
  }: let
    default = final: prev:
      (import ./overlays final prev)
      // (import emacs-overlay final prev)
      // (emacs-tramp-rpc.overlays.default final prev);
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
          gondolin
          offline-run
          parinfer-rust-emacs
          wgn-emacs
          wgn-emacs-pgtk
          wgn-emacs-nox
          wgn-emacs-macport
          wgn-emacs-unstable
          wgn-emacs-unstable-nox
          wgn-emacs-git
          wgn-emacs-git-nox
          ;

        default = wgn-emacs;
      };

      checks =
        pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
          offline-run-image-tools = pkgs.offline-run.tests.image-tools;
        }
        // {
          org-roam-module = pkgs.callPackage ./tests/org-roam-module.nix {
            module = self.homeManagerModules.default;
          };
        }
        // pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          offline-run-sandbox = pkgs.offline-run.tests.sandbox;
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

      templates = {
        go = {
          path = ./templates/go;
        };
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
              };

              # These enable the bridges used by agent-shell. Configure
              # agent skills, MCP servers, settings and trusted projects
              # separately.
              copilot = {
                enable = lib.mkEnableOption "Enable Copilot integration for Emacs";
              };

              claude = {
                enable = lib.mkEnableOption "Enable Claude integration for Emacs";
              };

              codex = {
                enable = lib.mkEnableOption "Enable Codex integration for Emacs";
              };

              org-roam = {
                directory = lib.mkOption {
                  # Emacs would read "" or a relative path against its
                  # current directory.
                  type = lib.types.nullOr (lib.types.strMatching "/.*");
                  default = null;
                  example = "/home/me/git/notes";
                  description = ''
                    Absolute path of the notes repository that org-roam
                    indexes, agents' plans among its notes. Emacs reads it
                    from ~/.emacs.d/etc/agent-plans.el, which this writes.
                    With null, org-roam is not set up for the notes.
                  '';
                };
              };

              agent-tools = {
                configFile = lib.mkOption {
                  # Emacs would read "" or a relative path against its
                  # current directory.
                  type = lib.types.nullOr (lib.types.strMatching "/.*");
                  default = null;
                  example = "/home/me/.config/agent-tools/config.json";
                  description = ''
                    Absolute path of the config file the deployed
                    agent-tools read, where agent-plan finds the plans.
                    Emacs then reads exactly that file, ignoring
                    AGENT_TOOLS_CONFIG and XDG_CONFIG_HOME, as an
                    agent-tools build with its config file built in does.
                    Emacs reads it from ~/.emacs.d/etc/agent-plans.el,
                    which this writes. With null, Emacs follows the rule
                    agent-tools has without a built-in file.
                  '';
                };
              };
            };
          };

          config = lib.mkIf cfg.enable {
            home = {
              # default.el moves the server's socket out of the default
              # directory, so emacsclient has to be told where it is. Keep the
              # two in step.
              sessionVariables.EMACS_SOCKET_NAME = "${config.home.homeDirectory}/.emacs.d/var/server/socket/server";

              packages = with pkgs;
                [
                  direnv
                  multimarkdown
                  ripgrep
                ]
                ++ (lib.optional cfg.claude.enable claude-agent-acp)
                ++ (lib.optional cfg.codex.enable codex-acp)
                ++ (lib.optional cfg.copilot.enable copilot-language-server);

              file = {
                ".emacs.d/early-init.el".source = let
                  gnus =
                    if cfg.gnus.enable
                    then config.sops.templates.".gnus.el".path
                    else "~/.emacs.d/etc/gnus/init.el";
                in
                  pkgs.replaceVars ./early-init.el {
                    inherit
                      gnus
                      ;
                  };

                ".emacs.d/etc/eshell/login".source = ./login.el;

                # `wgn/agent-plans-machine-file' names this file.
                ".emacs.d/etc/agent-plans.el" = lib.mkIf (cfg.org-roam.directory != null || cfg.agent-tools.configFile != null) {
                  text =
                    ''
                      ;;; agent-plans.el --- This machine's notes and agent-tools config -*- lexical-binding: t; -*-
                    ''
                    + lib.optionalString (cfg.org-roam.directory != null) ''
                      (setq wgn/agent-plans-org-roam-directory ${builtins.toJSON cfg.org-roam.directory})
                    ''
                    + lib.optionalString (cfg.agent-tools.configFile != null) ''
                      (setq wgn/agent-plans-config-file ${builtins.toJSON cfg.agent-tools.configFile})
                    '';
                };
              };
            };

            sops = lib.mkIf cfg.gnus.enable {
              secrets = {
                email-personal = {};
                email-work = {};
              };

              # TODO: It would be nice to be able to edit this with syntax highlighting. Org tangle?
              templates.".gnus.el".content = ''
                ;;; .gnus.el --- Wesley's Gnus Config  -*- lexical-binding: t; -*-

                ;; URL: https://git.sr.ht/~wgn/emacs-config

                ;; Package-Requires: ((emacs "29.1"))

                ;; This file is not part of GNU Emacs.

                ;;; Commentary:

                ;; NOTE: auth-source must hold credentials for your email
                ;;       account(s): a 1Password Login item per account, its
                ;;       websites the IMAP and SMTP servers and its username
                ;;       the address,
                ;;       or ~/.authinfo.gpg where the 1Password CLI is absent.
                ;;
                ;; NOTE: Please set the following variables with customize:
                ;;; `user-mail-address' with your primary email address.
                ;;;; Can alternatively be set via the EMAIL environment variable.
                ;;;; This should match the email address of your PGP key.
                ;;; `user-full-name' with your full name.
                ;;;; Can alternatively be set via the NAME environment variable.
                ;;; `mml-secure-openpgp-signers' with the PGP key ID(s) that you want
                ;;; to use for signing/encryption.

                ;;; License:

                ;; This program is free software; you can redistribute it and/or
                ;; modify it under the terms of the GNU General Public License
                ;; as published by the Free Software Foundation; either version 3
                ;; of the License, or (at your option) any later version.
                ;;
                ;; This program is distributed in the hope that it will be useful,
                ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
                ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
                ;; GNU General Public License for more details.
                ;;
                ;; You should have received a copy of the GNU General Public License
                ;; along with this program. If not, see <https://www.gnu.org/licenses/>.

                ;;; Code:

                (require 'gnus)
                (require 'gnus-async)
                (require 'gnus-dired)
                (require 'gnus-msg)

                (setq mail-sources `((imap :server "imap.fastmail.com"
                                           :user "${config.sops.placeholder.email-personal}"
                                           :port 993)
                                     (imap :server "imap.gmail.com"
                                           :user "${config.sops.placeholder.email-work}"
                                           :port 993)))

                ;;;; Gnus source configuration:
                (setq gnus-select-method '(nnnil nil))
                (add-to-list 'gnus-secondary-select-methods
                             '(nnimap "fastmail"
                                      (nnimap-address "imap.fastmail.com")
                                      (nnimap-server-port 993)
                                      (nnimap-stream ssl)
                                      (nnimap-inbox "nnimap+fastmail:INBOX")
                                      (nnmail-expiry-target "nnimap+fastmail:Trash")
                                      (nnmail-expiry-wait 90)))
                (add-to-list 'gnus-secondary-select-methods
                             '(nnimap "gmail"
                                      (nnimap-address "imap.gmail.com")
                                      (nnimap-server-port 993)
                                      (nnimap-stream ssl)
                                      (nnimap-inbox "nnimap+gmail:INBOX")
                                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                                      (nnmail-expiry-wait 90)))
                (add-to-list 'gnus-secondary-select-methods
                             '(nntp "news.gwene.org"))
                (add-to-list 'gnus-secondary-select-methods
                             '(nntp "news.gmane.io"))

                (setq gnus-parameters '(("fastmail"
                                         (gcc-self . "nnimap+fastmail:Sent"))
                                        ("gmail"
                                         (gcc-self . "nnimap+gmail:[Gmail]/Sent Mail")))
                      gnus-posting-styles '((".*"
                                             (signature-file "~/.signature"))
                                            ("fastmail"
                                             (address "${config.sops.placeholder.email-personal}")
                                             ("X-Message-SMTP-Method" "smtp smtp.fastmail.com 587"))
                                            ("gmail"
                                             (address "${config.sops.placeholder.email-work}")
                                             ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))))

                ;;;; Gnus general configuration:
                (setq gnus-use-cache t
                      gnus-asynchronous t
                      gnus-use-header-prefetch t
                      gnus-gcc-mark-as-read t
                      gnus-use-trees t
                      gnus-thread-sort-functions '((not gnus-thread-sort-by-number)
                                                   gnus-thread-sort-by-score))
                (add-hook 'dired-mode-hook #'gnus-dired-mode)
                (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
                (add-hook 'gnus-select-group-hook #'gnus-group-set-timestamp)

                ;;;; Appearance:
                (setq gnus-sum-thread-tree-false-root " "
                      gnus-sum-thread-tree-indent "  "
                      gnus-sum-thread-tree-root "r "
                      gnus-sum-thread-tree-single-indent "◎ "
                      gnus-sum-thread-tree-vertical "|"
                      gnus-sum-thread-tree-leaf-with-other "├─► "
                      gnus-sum-thread-tree-single-leaf "╰─► "

                      ;; │06-Jan│  Sender Name  │ Email Subject
                      gnus-summary-line-format (concat "%0{%U%R%z%}"
                                                       "%3{│%}" "%1{%d%}" "%3{│%}"
                                                       "  "
                                                       "%4{%-20,20f%}"
                                                       "  "
                                                       "%3{│%}"
                                                       " "
                                                       "%1{%B%}"
                                                       "%s\n"))

                ;;;; Encryption/signing configuration:
                (setq gnus-message-replysign 't
                      gnus-message-replyencrypt 't
                      gnus-buttonized-mime-types '("multipart/signed"))

                (provide 'init)

                ;;; .gnus.el ends here
              '';
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

              package = lib.mkPackageOption pkgs "wgn-emacs-unstable-nox" {};
            };
          };

          config = lib.mkIf cfg.enable {
            fonts.packages = with pkgs.nerd-fonts; [
              iosevka
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
                  enchant_2
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
