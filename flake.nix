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
    self,
    emacs-overlay,
    nixpkgs,
    flake-utils,
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
        in {
          options = {
            home.programs.wgn.emacs = {
              enable = lib.mkEnableOption "Enable the Home Manager portion of Wesley's Emacs Configuration";

              gnus = {
                enable = lib.mkEnableOption "Enable Wesley's Emacs Gnus Configuration with Home Manager";
              };

              llm = {
                enable = lib.mkEnableOption "Enable LLM integration for Emacs";
              };
            };
          };

          config =
            lib.mkIf cfg.enable {
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
                  ".emacs.d/early-init.el".source = ./early-init.el;
                  ".emacs.d/etc/eshell/login".source = ./login.el;
                };
              };
            }
            // lib.mkIf cfg.gnus.enable {
              sops = {
                secrets = {
                  name = {};
                  email = {};
                  email-fastmail = {};
                  email-shipt = {};
                };

                templates."init.el".content = ''
                  ;;; init.el --- Wesley's Gnus Config  -*- lexical-binding: t; -*-

                  ;; Copyright (C) 2024 ${config.sops.placeholder.name}

                  ;; Author: ${config.sops.placeholder.name} <${config.sops.placeholder.email}>
                  ;; Maintainer: ${config.sops.placeholder.name} <${config.sops.placeholder.email}>
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

                  ;;; init.el ends here
                '';
              };

              home.file.".emacs.d/etc/gnus/init.el".source = config.sops.templates."init.el".path;
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
