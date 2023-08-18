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
            emacs-macport = (prev.emacs-macport.override {
              # HACK: if you look inside the derivation for emacs, you
              #       see that these options are set to be true if the
              #       major version of emacs is at least 29. in this
              #       overlay, we override the version attribute of
              #       emacs-macport to have a major version of 29
              #       rather than 28, so you would think that these
              #       options would be consequently set to
              #       true. however, this is not the case. instead,
              #       they are still set to be false due to the major
              #       version being 28 *before* we override the
              #       version attribute. therefore, we need to
              #       manually override these values to be true in
              #       addition to overriding the version attribute.
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
