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

    emacs-skills = {
      url = "github:xenodium/emacs-skills";
      flake = false;
    };
  };

  outputs = {
    self,
    emacs-overlay,
    emacs-skills,
    nixpkgs,
    flake-utils,
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
          claude-agent-acp
          mcp-cli
          parinfer-rust-emacs
          wgn-emacs
          wgn-emacs-pgtk
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
      };

      templates = {
        shipt = {
          path = ./templates/shipt;
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

          llm =
            cfg.claude.enable
            || cfg.copilot.enable;

          skills =
            {
              mcp-cli = ./skills/mcp-cli/SKILL.md;
              agent-shell-memory = ./skills/agent-shell-memory/SKILL.md;
              describe = builtins.readFile "${emacs-skills}/skills/describe/SKILL.md";
              dired = builtins.readFile "${emacs-skills}/skills/dired/SKILL.md";
              emacsclient = builtins.readFile "${emacs-skills}/skills/emacsclient/SKILL.md";
              file-links = builtins.readFile "${emacs-skills}/skills/file-links/SKILL.md";
              highlight = builtins.readFile "${emacs-skills}/skills/highlight/SKILL.md";
              open = builtins.readFile "${emacs-skills}/skills/open/SKILL.md";
              select = builtins.readFile "${emacs-skills}/skills/select/SKILL.md";
            }
            // lib.optionalAttrs pkgs.stdenv.isDarwin {
              trash = ./skills/trash/SKILL.md;
            };
        in {
          options = {
            home.programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable the Home Manager portion of Wesley's Emacs Configuration";

              gnus = {
                enable = lib.mkEnableOption "Enable Wesley's Emacs Gnus Configuration with Home Manager";
              };

              copilot = {
                enable = lib.mkEnableOption "Enable Copilot integration for Emacs";
              };

              claude = {
                enable = lib.mkEnableOption "Enable Claude integration for Emacs";
              };
            };
          };

          config = lib.mkIf cfg.enable {
            xdg.configFile = {
              "fish/conf.d/emacs-vterm.fish" = lib.mkIf config.programs.fish.enable {
                enable = true;
                source = "${pkgs.emacsPackages.vterm}/share/emacs/site-lisp/elpa/vterm-${pkgs.emacsPackages.vterm.version}/etc/emacs-vterm.fish";
              };

              # TODO: Is there a way to just make a symlink instead?
              "mcp/mcp_servers.json" = lib.mkIf llm {
                enable = true;
                source = config.xdg.configFile."mcp/mcp.json".source;
              };
            };

            programs = {
              claude-code = lib.mkIf cfg.claude.enable {
                enable = true;
                enableMcpIntegration = true;

                settings = {
                  includeCoAuthoredBy = false;
                };

                inherit skills;
              };

              mcp = lib.mkIf llm {
                enable = true;

                servers = {
                  docs-mcp-server = {
                    type = "stdio";
                    command = "${pkgs.nodejs}/bin/npx";
                    args = [
                      "-y"
                      "@arabold/docs-mcp-server@latest"
                    ];
                    env = {
                      DOCS_MCP_TELEMETRY = "false";
                    };
                  };

                  # TODO: Add additional MCP servers.
                  #
                  # Examples:
                  #
                  # - https://github.com/ProfessioneIT/lsp-mcp-server
                };
              };
            };

            home = {
              packages = with pkgs;
                []
                ++ (lib.optional llm mcp-cli)
                # TODO: Can nodejs instead be made implicitly available to mcp-cli at runtime?
                ++ (lib.optional llm nodejs)
                ++ (lib.optional cfg.claude.enable claude-agent-acp)
                ++ (lib.optional cfg.copilot.enable copilot-language-server)
                ++ (lib.optional cfg.copilot.enable github-copilot-cli);

              file =
                {
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
                }
                // lib.optionalAttrs cfg.copilot.enable (
                  lib.mapAttrs' (
                    name: content:
                      lib.nameValuePair ".copilot/skills/${name}.md" (
                        if lib.isPath content
                        then {source = content;}
                        else {text = content;}
                      )
                  )
                  skills
                );
            };

            sops = lib.mkIf cfg.gnus.enable {
              secrets = {
                email-fastmail = {};
                email-shipt = {};
              };

              # TODO: It would be nice to be able to edit this with syntax highlighting. Org tangle?
              templates.".gnus.el".content = ''
                ;;; .gnus.el --- Wesley's Gnus Config  -*- lexical-binding: t; -*-

                ;; URL: https://git.sr.ht/~wgn/emacs-config

                ;; Package-Requires: ((emacs "29.1"))

                ;; This file is not part of GNU Emacs.

                ;;; Commentary:

                ;; NOTE: ~/.authinfo.gpg must contain credentials for your email
                ;;       account(s).
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
                                           :user "${config.sops.placeholder.email-fastmail}"
                                           :port 993)
                                     (imap :server "imap.gmail.com"
                                           :user "${config.sops.placeholder.email-shipt}"
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
                                             (address "${config.sops.placeholder.email-fastmail}")
                                             ("X-Message-SMTP-Method" "smtp smtp.fastmail.com 587"))
                                            ("gmail"
                                             (address "${config.sops.placeholder.email-shipt}")
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

              package = lib.mkPackageOption pkgs "wgn-emacs-unstable-nox" {
                default = pkgs.emacs;
              };
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
