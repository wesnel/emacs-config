# Wesley's Emacs Configuration

# Quickstart

Technically, there is no need to even clone this repository. All you need is [nix](https://github.com/NixOS/nix) with [flakes](https://xeiaso.net/blog/nix-flakes-1-2022-02-21) enabled.

## No X (Terminal Only)

``` shell
nix build github:wesnel/emacs-config
```

Then,

``` shell
./result/bin/emacs -nw
```

## With X (Cross-Platform GUI)

``` shell
nix build github:wesnel/emacs-config#wgn-emacs-unstable
```

Then,

``` shell
./result/bin/emacs
```

## MacOS Optimized GUI

``` shell
nix build github:wesnel/emacs-config#wgn-emacs-macport
```

Then,

``` shell
open ./result/Applications/Emacs.app
```

## Just the Configuration File

### Targeting Nix Systems

``` shell
nix build github:wesnel/emacs-config#emacs-config
```

Then,

``` shell
cat ./result
```

With this method, all external commands referenced in the Emacs configuration will be installed automatically.  The full path to these commands will be statically linked in the Emacs configuration, so as to not pollute your `$PATH`.

### Targeting non-Nix Systems

``` shell
nix build github:wesnel/emacs-config#emacs-config-dynamic
```

Then,

``` shell
cat ./result
```

With this method, any external commands referenced in the Emacs configuration will fail unless they are manually installed somehow.

# Nix Configuration Module Usage

## Using Nix Flakes

### NixOS

1. Import this flake.
2. Replace the `emacs` package from `nixpkgs` with a modified Emacs package from this flake.
3. Apply the NixOS configuration module from this flake to the system.
4. Apply the Nix [home-manager](https://github.com/nix-community/home-manager) configuration module from this flake to the system.

``` nix
# truncated excerpt from flake.nix

{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-config = {
      url = "git+https://git.sr.ht/~wgn/emacs-config?ref=main"; # (1)
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , emacs-config }:

    {
      nixosConfigurations.${computer} = nixpkgs.lib.nixosSystem {
        modules = [
          (_:

            {
              nixpkgs.overlays = [
                emacs-config.overlays.default
                emacs-config.overlays.emacs # (2)
              ];
            })

          emacs-config.nixosModules.default # (3)

          home-manager.nixosModules.home-manager {
            home-manager = {
              users = {
                "${username}" = _:

                  {
                    imports = [
                      emacs-config.homeManagerModules.default # (4)
                    ];
                  };
              };
            };
          }
        ]
      };
    };
}
```

### Darwin

This flake is also compatible with MacOS systems using [nix-darwin](https://github.com/LnL7/nix-darwin).

1. Import this flake.
2. Replace the `emacs` package from `nixpkgs` with a modified Emacs package from this flake.
3. Apply the NixOS configuration module from this flake to the system.
4. Apply the Nix [home-manager](https://github.com/nix-community/home-manager) configuration module from this flake to the system.

``` nix
# truncated excerpt from flake.nix

{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-config = {
      url = "git+https://git.sr.ht/~wgn/emacs-config?ref=main"; # (1)
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , nix-darwin
    , emacs-config }:

    {
      darwinConfigurations.${computer} = nix-darwin.lib.darwinSystem {
        modules = [
          (_:

            {
              nixpkgs.overlays = [
                emacs-config.overlays.default
                emacs-config.overlays.emacs # (2)
              ];
            })

          emacs-config.nixosModules.default # (3)

          home-manager.darwinModules.home-manager {
            home-manager = {
              users = {
                "${username}" = _:

                  {
                    imports = [
                      emacs-config.homeManagerModules.default # (4)
                    ];
                  };
              };
            };
          }
        ]
      };
    };
}
```
