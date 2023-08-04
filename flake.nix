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

    # TODO: how can i make these configurable by an external user
    #       of this flake?
    mail_address = "wgn@wesnel.dev";
    mail_name = "Wesley Nelson";
    mail_maildir = "$HOME/Maildir";
    mail_keyid = "0x8AB4F50FF6C15D42";
  };

  outputs = inputs@
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

        (final: prev:

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
      ];

      name = "wgn-emacs";
      overlay = import ./overlays {
        inherit (inputs)
          mail_address
          mail_name
          mail_maildir
          mail_keyid;
      };
      systems = flake-utils.lib.defaultSystems;
    };
}
