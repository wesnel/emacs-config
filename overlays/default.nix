final: prev: let
  # The tree-sitter grammars which Emacs should be able to load.  This
  # is shared between the Emacs package set (so that the grammars are
  # installed) and `default.el' (so that it can point
  # `treesit-extra-load-path' at them).  `treesit-grammars' just builds
  # a link farm of prebuilt parsers, so it does not depend on which
  # Emacs is being wrapped.
  treesit-grammars = pkgs:
    pkgs.emacsPackages.treesit-grammars.with-grammars (
      grammars:
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
        ]
    );

  # For building an Emacs configuration for non-Nix systems which
  # are presumed to just have these commands pre-installed.
  build-deps-dynamic = pkgs: {
    parinfer = "(concat parinfer-rust-library-directory parinfer-rust--lib-name)";
    grammars = ''(expand-file-name "tree-sitter" user-emacs-directory)'';
    ensure = "#'use-package-ensure-elpa";
  };

  # For building an Emacs configuration for Nix systems.  These
  # commands will be automatically installed alongside the Emacs
  # configuration, and their Nix store paths will be statically
  # linked in the built Emacs configuration.
  build-deps-static = pkgs: {
    parinfer = let
      pkg = pkgs.parinfer-rust-emacs;
    in ''
      "${pkg}/lib/libparinfer_rust.so"
    '';

    grammars = ''
      "${treesit-grammars pkgs}/lib"
    '';

    ensure = "#'ignore";
  };

  build-emacs-config = pkgs: build-deps: let
    deps = build-deps pkgs;
  in
    # TODO: Split Emacs config across multiple files and combine into
    # one default.el file with nix.  Include `:after` in all
    # use-package forms in order to guarantee that packages are loaded
    # in the correct order regardless of location in final file.
    pkgs.replaceVars ../default.el {
      eca-helper = builtins.readFile ../lisp/wgn-eca.el;
      inherit
        (deps)
        ensure
        grammars
        parinfer
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
        (with ePkgs; [
          # TODO: posframe and nerd-icons are required by knockknock,
          # but for some reason the propagatedBuildInputs in the
          # knockknock derivation doesn't make these dependencies
          # available at runtime on its own.
          nerd-icons
          posframe

          tree-sitter-langs
        ])
        ++ [
          (treesit-grammars pkgs)
        ];

      override = ePkgs: ePrev: {
        # HACK: `emacsWithPackagesFromUsePackage' byte-compiles
        # `default.el' inside a `trivialBuild' derivation which it
        # constructs itself, so overriding `trivialBuild' is the only way
        # to reach it.  That compile needs a writable HOME:
        # `use-package' loads every configured package in order to
        # inspect it, and a few of them (`no-littering',
        # `chatgpt-shell') write under HOME as they load, which fails
        # against the sandbox's read-only /homeless-shelter.
        trivialBuild = args:
          ePrev.trivialBuild (
            args
            // pkgs.lib.optionalAttrs (args.pname or "" == "default") {
              preBuild = ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME"
              '';
            }
          );

        agent-shell-knockknock = let
          rev = "56732434067fe1874dcda62c491f7800bdc0a2f3";
          sha256 = "sha256-R7bvk2v9togbPiGKOXDAKCMStrL1bbRJoTdBtj0PdlU=";
        in
          ePkgs.trivialBuild rec {
            pname = "agent-shell-knockknock";
            version = rev;

            packageRequires = with ePkgs; [
              acp
              agent-shell
              knockknock
            ];

            src = pkgs.fetchFromGitHub {
              owner = "xenodium";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        agent-shell-tramp = let
          rev = "ebdeb204973beb116017a977bee52cdced78e447";
          sha256 = "sha256-G1Q+hvwZ3iBax0f3/7tM1/7geYayEH6QzkQcz32+J0w=";
        in
          ePkgs.trivialBuild rec {
            pname = "agent-shell-tramp";
            version = rev;

            packageRequires = with ePkgs; [
              acp
              agent-shell
            ];

            src = pkgs.fetchFromGitHub {
              owner = "junyi-hou";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        auctex = ePrev.auctex.overrideAttrs (old: {
          outputs =
            (old.outputs or [])
            ++ [
              "out"
              "tex"
            ];

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
          rev = "38773810b5e532f25d11c6d1af02c3a8dffeacd7";
          sha256 = "sha256-gW0j0asfH4My+mBSt8pj3d51PigGWzFUhETydUI9xEg=";
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
          rev = "2cf8f3a55c64b52d6eab4aa13cb95b37442d33d5";
          sha256 = "sha256-gnIDlF706f4FlgeTjIeMRx04eYFwvdgkZHv31qfYJ/Y=";
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

        # x86_64-darwin builds from nixpkgs 26.05, whose ghostel compiles
        # its Zig module without xcbuild, so the vendored ghostty build
        # fails with DarwinSdkNotFound.  preBuild must match the module
        # layout of the 26.05 package.
        ghostel =
          if pkgs.stdenv.hostPlatform.system == "x86_64-darwin"
          then let
            module = ePrev.ghostel.passthru.module.overrideAttrs (old: {
              nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.xcbuild];
            });
          in
            ePrev.ghostel.overrideAttrs (old: {
              preBuild = ''
                install ${module}/lib/libghostel-module.dylib ghostel-module.dylib
              '';

              passthru = old.passthru // {inherit module;};
            })
          else ePrev.ghostel;

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

        knockknock = let
          rev = "7a6ab46503554317b639a7333ec8046d7d181520";
          sha256 = "sha256-hkvEuad3Gh++PMeaMJHd2j//ho+FA59QGxCex3NVi98=";
        in
          ePkgs.trivialBuild rec {
            pname = "knockknock";
            version = rev;

            propagatedBuildInputs = with ePkgs; [
              nerd-icons
              posframe
            ];

            src = pkgs.fetchFromGitHub {
              owner = "konrad1977";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        org-modern-indent = let
          rev = "ebf9a8e571db523dc6e4cd9ed80d0e0626983ae4";
          sha256 = "sha256-+q7KmbU8A+uR61BSa528vYbdFSj2WGsFWYW/5q7J9Kw=";
        in
          ePkgs.trivialBuild rec {
            pname = "org-modern-indent";
            version = rev;

            src = pkgs.fetchFromGitHub {
              owner = "jdtsmith";
              repo = pname;

              inherit
                rev
                sha256
                ;
            };
          };

        reader = let
          rev = "98c5046683e997902a83092b65cdb70ab120e000";
          sha256 = "sha256-Jo8ZecM4Y22T5kc5zJzCvSywkxwcpNEtQ3HHMJNesac=";

          # HACK: It seems that this must be manually updated to an arbitrary date.
          version = "20260318";

          src = final.fetchFromGitea {
            domain = "codeberg.org";
            owner = "MonadicSheep";
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
              emacs
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

  emacs30-macport = prev.emacs30-macport.overrideAttrs (old: {
    postInstall = let
      icons = final.fetchFromGitHub {
        owner = "jimeh";
        repo = "emacs-liquid-glass-icons";
        tag = "v1.1.0";
        hash = "sha256-8U5V1YQymoSLK32NrIGGg6M4l2bIT4L8RvuC+EUO2Bc=";
      };
    in
      (old.postInstall or "")
      + ''
        cp ${icons}/Resources/Assets.car $out/Applications/Emacs.app/Contents/Resources
        cp ${icons}/Resources/EmacsLG1-Default.icns $out/Applications/Emacs.app/Contents/Resources/Emacs.icns

        ICON_NAME="EmacsLG1"
        PLIST="$out/Applications/Emacs.app/Contents/Info.plist"
        /usr/libexec/PlistBuddy -c "Set :CFBundleIconName $ICON_NAME" "$PLIST" 2>/dev/null \
            || /usr/libexec/PlistBuddy -c "Add :CFBundleIconName string $ICON_NAME" "$PLIST"
      '';
  });

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

  wgn-emacs-macport =
    build-emacs
    final
    build-deps-static
    (pkgs: pkgs.emacs30-macport);
in {
  # Pin host and guest ECA servers together. Home Manager selects which
  # sandbox backend to install alongside the Emacs integration.
  eca = final.callPackage ./eca {};
  eca-guest = final.callPackage ./eca {target = "guest";};
  gondolin = final.callPackage ./gondolin {};
  eca-gondolin = final.callPackage ./eca-gondolin {};
  eca-bwrap = final.callPackage ./eca-bwrap {};

  inherit
    emacs30-macport
    emacs-config
    emacs-config-dynamic
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
}
