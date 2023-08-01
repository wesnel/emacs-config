{
  description = "Wesley's Emacs/Nix Configuration";

  inputs = {
    emacs-overlay = {
      url = github:nix-community/emacs-overlay;
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    nixpkgs = {
      url = github:nixos/nixpkgs/nixos-unstable;
    };
  };

  outputs = inputs@
    { self
    , emacs-overlay
    , flake-utils
    , nixpkgs }:
      flake-utils.lib.simpleFlake {
        inherit self nixpkgs;

        preOverlays = [
          (import emacs-overlay)

          (final: prev:

            {
              parinfer-rust = prev.parinfer-rust.overrideAttrs (old:

                {
                  postInstall = ''
                    ${old.postInstall}

                    if [ -e $out/lib/libparinfer_rust.dylib ]
                      then cp $out/lib/libparinfer_rust.dylib $out/lib/libparinfer_rust.so
                    fi
                  '';
                });
            })
        ];

        name = "wgn-emacs";
        overlay = ./overlays;
        systems = flake-utils.lib.defaultSystems;
      };
}
