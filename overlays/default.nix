final: prev: let
  # For building an Emacs configuration for non-Nix systems which
  # are presumed to just have these commands pre-installed.
  build-deps-dynamic = pkgs: {
    asdf = "asdf";
    delta = "delta";
    direnv = "direnv";
    gs = "gs";
    hledger = "hledger";
    multimarkdown = "multimarkdown";
    mupdf = "mupdf";
    parinfer = "(concat parinfer-rust-library-directory parinfer-rust--lib-name)";
    pass = "pass";
    rg = "rg";
  };

  # For building an Emacs configuration for Nix systems.  These
  # commands will be automatically installed alongside the Emacs
  # configuration, and their Nix store paths will be statically
  # linked in the built Emacs configuration.
  build-deps-static = pkgs: {
    asdf = let
      pkg = pkgs.asdf-vm;
    in "${pkg}/bin/asdf";

    delta = let
      pkg = pkgs.delta;
    in "${pkg}/bin/delta";

    direnv = let
      pkg = pkgs.direnv;
    in "${pkg}/bin/direnv";

    gs = let
      pkg = pkgs.ghostscript;
    in "${pkg}/bin";

    hledger = let
      pkg = pkgs.hledger;
    in "${pkg}/bin";

    multimarkdown = let
      pkg = pkgs.multimarkdown;
    in "${pkg}/bin/multimarkdown";

    mupdf = let
      pkg = pkgs.mupdf;
    in "${pkg}/bin";

    parinfer = let
      pkg = pkgs.parinfer-rust-emacs;
    in ''
      "${pkg}/lib/libparinfer_rust.so"
    '';

    pass = let
      pkg = pkgs.pass;
    in "${pkg}/bin/pass";

    rg = let
      pkg = pkgs.ripgrep;
    in "${pkg}/bin/rg";
  };

  build-emacs-config = pkgs: build-deps: let
    deps = build-deps pkgs;
  in
    pkgs.substituteAll {
      name = "default.el";
      src = ../default.el;

      inherit
        (deps)
        asdf
        delta
        direnv
        gs
        hledger
        multimarkdown
        mupdf
        parinfer
        pass
        rg
        ;
    };

  build-emacs = pkgs: build-deps: build-package: let
    config = build-emacs-config pkgs build-deps;
  in
    pkgs.emacsWithPackagesFromUsePackage {
      config = config;
      package = build-package pkgs;
      defaultInitFile = config;

      extraEmacsPackages = ePkgs:
        with ePkgs; [
          treesit-grammars.with-all-grammars
        ];

      override = ePkgs: ePrev: {
        auctex = ePrev.auctex.overrideAttrs (old: {
          outputs =
            (old.outputs or [])
            ++ [
              "out"
              "tex"
            ];

          buildInputs =
            (old.buildInputs or [])
            ++ (with pkgs; [
              ghostscript
              texlive.combined.scheme-full
            ]);

          preConfigure = ''
            ${old.preConfigure or ""}

            mkdir -p "$tex"
          '';

          configureFlags =
            (old.configureFlags or [])
            ++ [
              "--with-lispdir=\${out}/share/emacs/site-lisp"
              "--with-texmf-dir=\${tex}"
            ];
        });

        combobulate = let
          rev = "e3370c97bcd1eb9b5dcba03014b246948c6e7126";
          sha256 = "sha256-4KNlyftCuniP4DDXDBsDQmB1KReYz3xzRzkr/awx9eA=";
        in
          ePkgs.trivialBuild rec {
            pname = "combobulate";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "mickeynp";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        consult-gh = let
          rev = "1fe876d9552b6ec6af257a4299a34eca99b40539";
          sha256 = "sha256-bi+qlNvNMXbS4cXbXt01txwD2NAyAqJGNKeOtdtj7tg=";
        in
          ePkgs.trivialBuild rec {
            pname = "consult-gh";
            version = rev;

            packageRequires = with ePkgs; [
              consult
              embark
            ];

            src = pkgs.fetchFromGitHub {
              owner = "armindarvish";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        elsewhere = let
          rev = "85300095abd8f227413f80b3185c96874fd05002";
          sha256 = "sha256-GdarA9ac2UvJPphkictyM/k1SXlxE0qhCPJzh96nv3A=";
        in
          ePkgs.trivialBuild rec {
            pname = "elsewhere";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "wesnel";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        emacs-copilot = let
          rev = "c629d3951df88b48bdc47620b33d2588508d73be";
          sha256 = "0m6by3m1k1jlbjzgh9fkdsnkcydpxvv6jbg644822dkynvirgihm";
        in
          ePkgs.trivialBuild rec {
            pname = "emacs-copilot";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "jart";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        flymake-hledger = let
          rev = "80cffbc70aa72dc9de311e5cf172664f76b4b7e0";
          sha256 = "sha256-ZcSKhdPe643uzwRA5IjdLcDSU1Gd0hEL8CXSVB5/gys=";
        in
          ePkgs.trivialBuild rec {
            pname = "flymake-hledger";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "DamienCassou";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        go-mode = let
          rev = "166dfb1e090233c4609a50c2ec9f57f113c1da72";
          sha256 = "sha256-SEqDeF5F/DQ5/NOi3n6mYhlkYg+VxY/EPAvxtt5wUG0=";
        in
          ePrev.go-mode.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = old.src.owner;
              repo = old.src.repo;

              inherit
                rev
                sha256
                ;
            };
          });

        ijanet-mode = let
          rev = "2821db192fd0733d402cbd1c58f6060aa02ef100";
          sha256 = "sha256-Ljuj6Oc28xgi5N1XCp4TjHUtR83FFcVBN6dhTi/7qwk=";
        in
          ePkgs.trivialBuild rec {
            pname = "ijanet-mode";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "SerialDev";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        no-littering = let
          rev = "ef02b6fcedd97f3ab039b51411fdaab7336d819b";
          sha256 = "sha256-a3vCZzBUtSJ2EA/wyRfkLpteByZoSUbagiQ8hyJjrsQ=";
        in
          ePrev.no-littering.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = old.src.owner;
              repo = old.src.repo;

              inherit
                rev
                sha256
                ;
            };
          });

        scratch = let
          rev = "afd2590a56a8d33bd07ffbe30feda5ece9c5d5cb";
          sha256 = "1myd3zsxdkflrvpzmnd27n5ga9f4i0v1dq8p365fq3picq6694zw";
        in
          ePrev.scratch.overrideAttrs (old: {
            src = pkgs.fetchzip {
              url = "https://codeberg.org/wgn/scratch/archive/${rev}.tar.gz";

              inherit
                sha256
                ;
            };
          });

        which-key = let
          rev = "6abe544835d67f92a633fbf2622b9a8b6eb10a3c";
          sha256 = "sha256-FzTdq3WXDzXJKXgSG9ctJLSCnsR9nnlV+wY3eNaEWYU=";
        in
          ePrev.which-key.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = "wesnel";
              repo = "emacs-which-key";

              inherit
                rev
                sha256
                ;
            };
          });
      };
    };

  emacs-config = build-emacs-config final build-deps-static;
  emacs-config-dynamic = build-emacs-config final build-deps-dynamic;

  wgn-emacs =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs);

  wgn-emacs-nox =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-nox);

  wgn-emacs-unstable =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-unstable);

  wgn-emacs-unstable-nox =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-unstable-nox);

  wgn-emacs-git =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-git);

  wgn-emacs-git-nox =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-git-nox);

  wgn-emacs-macport =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs29-macport);

  # FIXME: My goal is to build a MacOS executable from Linux.
  #        Unfortunately this currently fails with the error:
  #
  # error: don't yet have a `targetPackages.darwin.LibsystemCross for x86_64-apple-darwin`
  wgn-emacs-macport-cross =
    build-emacs
    final.pkgsCross.x86_64-darwin
    build-deps-static
    (pkgs: pkgs.emacs29-macport);
in {
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
}
