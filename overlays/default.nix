final: prev:

{
  wgn-emacs = let
    aspell = let
      pkg = final.aspellWithDicts (d: with d; [ en en-computers en-science ]);
    in "${pkg}/bin/aspell";

    godef = let
      pkg = final.godef;
    in "${pkg}/bin/godef";

    gofumpt = let
      pkg = final.gofumpt;
    in "${pkg}/bin/gofumpt";

    gopls = let
      pkg = final.gopls;
    in "${pkg}/bin/gopls";

    mujmap = let
      pkg = final.mujmap;
    in "${pkg}/bin/mujmap";

    multimarkdown = let
      pkg = final.multimarkdown;
    in "${pkg}/bin/multimarkdown";

    nil = let
      pkg = final.nil;
    in "${pkg}/bin/nil";

    parinfer = let
      pkg = final.parinfer-rust;
    in "${pkg}/lib/libparinfer_rust.so";

    pylsp = let
      pkg = final.python3.withPackages(p: with p; [
        jedi
        mccabe
        pycodestyle
        pyflakes
        python-lsp-server
        rope
        yapf
      ]);
    in "${pkg}/bin/pylsp";

    yamlls = let
      pkg = final.yaml-language-server;
    in "${pkg}/bin/yaml-language-server";

    extraEmacsPackages = ePkgs: with ePkgs; [
      treesit-grammars.with-all-grammars
    ];

    override = eFinal: ePrev: {
      combobulate = let
        rev = "e3370c97bcd1eb9b5dcba03014b246948c6e7126";
        sha256 = "sha256-4KNlyftCuniP4DDXDBsDQmB1KReYz3xzRzkr/awx9eA=";
      in eFinal.trivialBuild {
        pname = "combobulate";
        version = rev;

        src = final.fetchFromGitHub {
          owner = "mickeynp";
          repo = "combobulate";

          inherit
            rev
            sha256;
        };
      };

      elsewhere = let
        rev = "85300095abd8f227413f80b3185c96874fd05002";
        sha256 = "sha256-GdarA9ac2UvJPphkictyM/k1SXlxE0qhCPJzh96nv3A=";
      in eFinal.trivialBuild {
        pname = "elsewhere";
        version = rev;

        src = final.fetchFromGitHub {
          owner = "wesnel";
          repo = "elsewhere";

          inherit
            rev
            sha256;
        };
      };

      go-mode = let
        rev = "166dfb1e090233c4609a50c2ec9f57f113c1da72";
        sha256 = "sha256-SEqDeF5F/DQ5/NOi3n6mYhlkYg+VxY/EPAvxtt5wUG0=";
      in ePrev.go-mode.overrideAttrs (old:

        {
          src = final.fetchFromGitHub {
            owner = old.src.owner;
            repo = old.src.repo;

            inherit
              rev
              sha256;
          };
        });

      no-littering = let
        rev = "ef02b6fcedd97f3ab039b51411fdaab7336d819b";
        sha256 = "sha256-a3vCZzBUtSJ2EA/wyRfkLpteByZoSUbagiQ8hyJjrsQ=";
      in ePrev.no-littering.overrideAttrs (old:

        {
          src = final.fetchFromGitHub {
            owner = old.src.owner;
            repo = old.src.repo;

            inherit
              rev
              sha256;
          };
        });

      which-key = let
        rev = "6abe544835d67f92a633fbf2622b9a8b6eb10a3c";
        sha256 = "sha256-FzTdq3WXDzXJKXgSG9ctJLSCnsR9nnlV+wY3eNaEWYU=";
      in ePrev.which-key.overrideAttrs (old:

        {
          src = final.fetchFromGitHub {
            owner = "wesnel";
            repo = "emacs-which-key";

            inherit
              rev
              sha256;
          };
        });
    };

    # TODO: how can i make these configurable by an external user
    #       of this flake?
    mail_address = "wgn@wesnel.dev";
    mail_name = "Wesley Nelson";
    mail_maildir = "$HOME/Maildir";
    mail_keyid = "0x8AB4F50FF6C15D42";

    emacs-config = final.substituteAll {
      name = "default.el";
      src = ../default.el;

      inherit
        aspell
        godef
        gofumpt
        gopls
        mail_address
        mail_name
        mail_maildir
        mail_keyid
        mujmap
        multimarkdown
        nil
        parinfer
        pylsp
        yamlls;
    };

    wgn-emacs = final.emacsWithPackagesFromUsePackage {
      config = ../default.el;
      package = final.emacs-unstable;
      defaultInitFile = emacs-config;

      inherit
        extraEmacsPackages
        override;
    };

    wgn-emacs-nox = final.emacsWithPackagesFromUsePackage {
      config = ../default.el;
      package = final.emacs-unstable-nox;
      defaultInitFile = emacs-config;

      inherit
        extraEmacsPackages
        override;
    };
  in {
    inherit
      emacs-config
      wgn-emacs
      wgn-emacs-nox;

    defaultPackage = wgn-emacs-nox;

    shell = final.mkShell {
      buildInputs = [
        emacs-config
        wgn-emacs-nox
      ];
    };
  };
}
