final: prev:

{
  wgn-emacs = let

    aspell = let
      pkg = final.aspellWithDicts(d: with d; [
        en
        en-computers
        en-science
      ]);
    in "${pkg}/bin/aspell";

    cssls = let
      pkg = final.vscode-langservers-extracted;
    in "${pkg}/bin/vscode-css-language-server";

    gh = let
      pkg = final.gh;
    in "${pkg}/bin";

    go = let
      pkg = final.go;
    in "${pkg}/bin/go";

    godef = let
      pkg = final.godef;
    in "${pkg}/bin/godef";

    godoc = let
      pkg = final.go;
    in "${pkg}/bin/go doc";

    gofumpt = let
      pkg = final.gofumpt;
    in "${pkg}/bin/gofumpt";

    gopls = let
      pkg = final.gopls;
    in "${pkg}/bin/gopls";

    gs = let
      pkg = final.ghostscript;
    in "${pkg}/bin";

    hledger = let
      pkg = final.hledger;
    in "${pkg}/bin";

    htmlls = let
      pkg = final.vscode-langservers-extracted;
    in "${pkg}/bin/vscode-html-language-server";

    multimarkdown = let
      pkg = final.multimarkdown;
    in "${pkg}/bin/multimarkdown";

    mupdf = let
      pkg = final.mupdf;
    in "${pkg}/bin";

    nil = let
      pkg = final.nil;
    in "${pkg}/bin/nil";

    parinfer = let
      pkg = final.parinfer-rust;
    in "${pkg}/lib/libparinfer_rust.so";

    pass = let
      pkg = final.pass;
    in "${pkg}/bin/pass";

    pylsp = let

      pkg = final.python3.withPackages (p:

        with p; [
          jedi
          mccabe
          pycodestyle
          pyflakes
          python-lsp-server
          rope
          yapf
        ]);

    in "${pkg}/bin/pylsp";

    texlab = let
      pkg = final.texlab;
    in "${pkg}/bin/texlab";

    tsxls = let
      pkg = final.nodePackages.typescript-language-server;
    in "${pkg}/bin/typescript-language-server";

    yamlls = let
      pkg = final.yaml-language-server;
    in "${pkg}/bin/yaml-language-server";

    extraEmacsPackages = ePkgs: with ePkgs; [
      treesit-grammars.with-all-grammars
    ];

    override = eFinal: ePrev: {
      auctex = ePrev.auctex.overrideAttrs (old:

        {
          outputs = (old.outputs or [ ]) ++ [
            "out"
            "tex"
          ];

          buildInputs = (old.buildInputs or [ ]) ++ (with final; [
            ghostscript
            texlive.combined.scheme-full
          ]);

          preConfigure = ''
            ${old.preConfigure or ""}

            mkdir -p "$tex"
          '';

          configureFlags = (old.configureFlags or [ ]) ++ [
            "--with-lispdir=\${out}/share/emacs/site-lisp"
            "--with-texmf-dir=\${tex}"
          ];
        });

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

      consult-gh = let
        rev = "1fe876d9552b6ec6af257a4299a34eca99b40539";
        sha256 = "sha256-bi+qlNvNMXbS4cXbXt01txwD2NAyAqJGNKeOtdtj7tg=";
      in eFinal.trivialBuild {
        pname = "consult-gh";
        version = rev;

        packageRequires = with eFinal; [
          consult
          embark
        ];

        src = final.fetchFromGitHub {
          owner = "armindarvish";
          repo = "consult-gh";

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

    emacs-config = final.substituteAll {
      name = "default.el";
      src = ../default.el;

      inherit
        aspell
        cssls
        gh
        go
        godef
        godoc
        gofumpt
        gopls
        gs
        hledger
        htmlls
        multimarkdown
        mupdf
        nil
        parinfer
        pass
        pylsp
        texlab
        tsxls
        yamlls;
    };

    wgn-emacs-unstable = final.emacsWithPackagesFromUsePackage {
      config = emacs-config;
      package = final.emacs-unstable;
      defaultInitFile = emacs-config;

      inherit
        extraEmacsPackages
        override;
    };

    wgn-emacs-unstable-nox = final.emacsWithPackagesFromUsePackage {
      config = emacs-config;
      package = final.emacs-unstable-nox;
      defaultInitFile = emacs-config;

      inherit
        extraEmacsPackages
        override;
    };

    wgn-emacs-macport = final.emacsWithPackagesFromUsePackage {
      config = emacs-config;
      package = final.emacs29-macport;
      defaultInitFile = emacs-config;

      inherit
        extraEmacsPackages
        override;
    };

  in rec {
    inherit
      emacs-config
      wgn-emacs-macport
      wgn-emacs-unstable
      wgn-emacs-unstable-nox;

    defaultPackage = wgn-emacs-unstable-nox;

    shell = final.mkShell {
      buildInputs = [
        defaultPackage
        emacs-config
      ];
    };
  };
}
