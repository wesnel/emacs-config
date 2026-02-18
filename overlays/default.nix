final: prev: let
  # For building an Emacs configuration for non-Nix systems which
  # are presumed to just have these commands pre-installed.
  build-deps-dynamic = pkgs: {
    copilotcli = "copilot";
    copilotlsp = "copilot-language-server";
    delta = "delta";
    direnv = "direnv";
    git = "git";
    hledger = "hledger";
    mcplsp = "mcp-language-server";
    multimarkdown = "multimarkdown";
    parinfer = "(concat parinfer-rust-library-directory parinfer-rust--lib-name)";
    pass = "pass";
    rg = "rg";
  };

  # For building an Emacs configuration for Nix systems.  These
  # commands will be automatically installed alongside the Emacs
  # configuration, and their Nix store paths will be statically
  # linked in the built Emacs configuration.
  build-deps-static = pkgs: {
    copilotcli = let
      pkg = pkgs.github-copilot-cli;
    in "${pkg}/bin/copilot";

    copilotlsp = let
      pkg = pkgs.copilot-language-server;
    in "${pkg}/bin/copilot-language-server";

    delta = let
      pkg = pkgs.delta;
    in "${pkg}/bin/delta";

    direnv = let
      pkg = pkgs.direnv;
    in "${pkg}/bin/direnv";

    git = let
      pkg = pkgs.git;
    in "${pkg}/bin/git";

    hledger = let
      pkg = pkgs.hledger;
    in "${pkg}/bin";

    mcplsp = let
      pkg = pkgs.mcp-language-server;
    in "${pkg}/bin/mcp-language-server";

    multimarkdown = let
      pkg = pkgs.multimarkdown;
    in "${pkg}/bin/multimarkdown";

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
    # TODO: Split Emacs config across multiple files and combine into
    # one default.el file with nix.  Include `:after` in all
    # use-package forms in order to guarantee that packages are loaded
    # in the correct order regardless of location in final file.
    pkgs.replaceVars ../default.el {
      inherit
        (deps)
        copilotcli
        copilotlsp
        delta
        direnv
        git
        hledger
        mcplsp
        multimarkdown
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
          tree-sitter-langs
          (treesit-grammars.with-grammars
            (grammars:
              with grammars; [
                tree-sitter-bash
                tree-sitter-bibtex
                tree-sitter-c
                tree-sitter-clojure
                tree-sitter-cmake
                tree-sitter-commonlisp
                tree-sitter-cpp
                tree-sitter-c-sharp
                tree-sitter-css
                tree-sitter-dockerfile
                tree-sitter-elisp
                tree-sitter-elixir
                tree-sitter-erlang
                tree-sitter-fish
                tree-sitter-gdscript
                tree-sitter-go
                tree-sitter-go-template
                tree-sitter-godot-resource
                tree-sitter-gomod
                tree-sitter-gowork
                tree-sitter-graphql
                tree-sitter-html
                tree-sitter-java
                tree-sitter-javascript
                tree-sitter-json
                tree-sitter-kotlin
                tree-sitter-latex
                tree-sitter-make
                tree-sitter-markdown
                tree-sitter-nix
                tree-sitter-python
                tree-sitter-regex
                tree-sitter-sql
                tree-sitter-toml
                tree-sitter-tsx
                tree-sitter-typescript
                tree-sitter-yaml
              ]))
        ];

      override = ePkgs: ePrev: {
        agent-shell = let
          rev = "e3fef57d048eb0395ad777350d5f5b6bfc5afb1a";
          sha256 = "sha256-dM3s9r7Y1yVQdPFtbMxGRqXgo9vKv3XBTc+/TU9QDQg=";
        in
          ePrev.agent-shell.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = "xenodium";
              repo = "agent-shell";

              inherit
                rev
                sha256
                ;
            };
          });

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
          rev = "e9c5be84062e8183f556d7133d5a477a57e37e51";
          sha256 = "sha256-r6jObsYx7RRTJUmrCN5h3+0WcHqJA67emhr4/W3rBrM=";
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

        flymake-golangci = let
          rev = "5495d7f5a00b171358cc0f2501ba64b79a240ce5";
          sha256 = "sha256-FZ7IfGsR4qGg2GIc7s7UVlNfHoI9cwE/KjX1UJXpRVk=";
        in
          ePkgs.trivialBuild rec {
            pname = "flymake-golangci";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "storvik";
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

        gptel-quick = let
          rev = "018ff2be8f860a1e8fe3966eec418ad635620c38";
          sha256 = "sha256-7a5+YQifwtVYHP6qQXS1yxA42bVGXmErirra0TrSSQ0=";
        in
          ePkgs.trivialBuild rec {
            pname = "gptel-quick";
            version = rev;

            packageRequires = with ePkgs; [
              gptel
            ];

            src = pkgs.fetchFromGitHub {
              owner = "karthink";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

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

        reader = let
          rev = "a76b1a0e13774be57bed186edf1e5bee8eeb0a56";
          sha256 = "sha256-pC51uw6xSB6ZuGyxAJAYUT3SInC4T3SDcb+haPy/b6o=";

          # HACK: It seems that this must be manually updated to an arbitrary date.
          version = "20251209";

          src = final.fetchFromGitea {
            domain = "codeberg.org";
            owner = "divyaranjan";
            repo = "emacs-reader";
            hash = sha256;

            inherit
              rev
              ;
          };

          render-core = final.stdenv.mkDerivation {
            pname = "render-core";

            strictDeps = true;

            buildFlags = [
              "CC=cc"
              "USE_PKGCONFIG=yes"
            ];

            nativeBuildInputs = with final; [
              pkg-config
            ];

            buildInputs = with final; [
              mupdf-headless
            ];

            installPhase = ''
              runHook preInstall

              install -Dm444 -t $out/lib/ render-core${final.stdenv.targetPlatform.extensions.sharedLibrary}

              runHook postInstall
            '';

            inherit
              src
              version
              ;
          };
        in
          ePkgs.melpaBuild {
            pname = "reader";

            files = ''
              (:defaults "${final.lib.getLib render-core}/lib/render-core.*"))
            '';

            inherit
              src
              version
              ;
          };

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
      };
    };

  emacs-config = build-emacs-config final build-deps-static;
  emacs-config-dynamic = build-emacs-config final build-deps-dynamic;

  wgn-emacs =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs);

  wgn-emacs-pgtk =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs-git-pgtk);

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

  # TODO: Configurable app icon on MacOS.
  wgn-emacs-macport =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs30-macport);

  # FIXME: My goal is to build a MacOS executable from Linux.
  #        Unfortunately this currently fails with the error:
  #
  # error: don't yet have a `targetPackages.darwin.LibsystemCross for x86_64-apple-darwin`
  wgn-emacs-macport-cross =
    build-emacs
    final.pkgsCross.x86_64-darwin
    build-deps-static
    (pkgs: pkgs.emacs30-macport);
in {
  inherit
    emacs-config
    emacs-config-dynamic
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
}
