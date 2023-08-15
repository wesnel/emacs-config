{
  description = "Wesley's Emacs/Nix Configuration";

  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };

  outputs =
    { self
    , emacs-overlay
    , flake-utils
    , nixpkgs
    , ... }:

    flake-utils.lib.simpleFlake {
      inherit
        nixpkgs
        self;

      preOverlays = [
        (import emacs-overlay)

        (final:
          prev:

          {
            parinfer-rust = prev.parinfer-rust.overrideAttrs (old:

              {
                # HACK: on mac, the file has the extension ".dylib",
                #       but it needs to be ".so":
                postInstall = ''
                  ${old.postInstall}

                  if [ -e $out/lib/libparinfer_rust.dylib ]
                    then cp $out/lib/libparinfer_rust.dylib $out/lib/libparinfer_rust.so
                  fi
                '';
              });
          })

        (final:
          prev:

          {
            # HACK: yikes
            emacs-macport = (prev.emacs-macport.override {
              withNativeCompilation = true;
              withTreeSitter = true;
            }).overrideAttrs (old:

              {
                version = "29.1";

                src = final.fetchFromBitbucket {
                  owner = "mituharu";
                  repo = "emacs-mac";
                  rev = "emacs-29.1-mac-10.0";
                  sha256 = "sha256-TE829qJdPjeOQ+kD0SfyO8d5YpJjBge/g+nScwj+XVU=";
                };
              });
          })
      ];

      name = "wgn-emacs";
      overlay = ./overlays;
      systems = flake-utils.lib.defaultSystems;
    };
}
