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

``` shell
nix build github:wesnel/emacs-config#emacs-config
```

Then,

``` shell
cat ./result
```

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
                (final: prev:

                  {
                    emacs = emacs-config.packages.${system}.wgn-emacs-unstable; # (2)
                  })
              ];
            })

          emacs-config.nixosModules.nixos # (3)

          home-manager.nixosModules.home-manager {
            home-manager = {
              users = {
                "${username}" = _:

                  {
                    imports = [
                      emacs-config.nixosModules.home # (4)
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
                (final: prev:

                  {
                    emacs = emacs-config.packages.${system}.wgn-emacs-unstable; # (2)
                  })
              ];
            })

          emacs-config.nixosModules.nixos # (3)

          home-manager.darwinModules.home-manager {
            home-manager = {
              users = {
                "${username}" = _:

                  {
                    imports = [
                      emacs-config.nixosModules.home # (4)
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
