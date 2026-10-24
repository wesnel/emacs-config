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

## Running a command offline

`offline-run` runs one command that must reach nothing, with no
network, the workspace read-only and nothing else of the machine but what it
is handed:

``` sh
offline-run --workspace . --guest-path /src \
  --ro-mount /nix/store/...-go:/opt/go --memory 4G --cpus 4 -- go test ./...
```

On Darwin it boots a Gondolin micro-VM, which has no network device at all.
On Linux it runs under Bubblewrap, which leaves it only loopback and binds no
`/`: the command sees the closure of the tools `offlineTools` lists in
`overlays/offline-run/bwrap.nix`, which are its `PATH`; a stub `/etc` with
`passwd`, `group`, `nsswitch.conf` and `hosts`; and empty `/tmp`, `/run`,
`/var/run` and `$HOME`. Anything else a command needs, such as a toolchain,
has to be handed in with `--ro-mount`.

- `--workspace DIR` (default: the current directory) is mounted read-only at
  `--guest-path PATH` (default: where it lives), and the command starts there.
- `--ro-mount HOST:GUEST` mounts the directory `HOST` read-only at `GUEST`, and
  may be repeated.
- `--memory SIZE` (QEMU syntax, such as `4G`) and `--cpus N` size Gondolin's
  VM. How Bubblewrap applies them is described where it does, in
  `overlays/offline-run/offline-run.sh`.
- `--image` selects a Gondolin image directory or image selector; Bubblewrap
  accepts and ignores it.
- `--offline`, and `--state ''` and `--config ''`, are accepted and change
  nothing; a non-empty `--state` or `--config` is refused.

Without `--image`, Gondolin builds its guest image on first use, which needs
the network and takes several minutes; `offline-run-prepare` builds it ahead of
time. Images are cached under `$XDG_CACHE_HOME/offline-run/images` (by default
`~/.cache/offline-run/images`), keyed on the image configuration in
`overlays/offline-run/build-config.json`, the architecture and the Gondolin
version. Publishing a new image removes the superseded ones.

## Tests

The agent plan tests use ERT and require only Emacs. Run them from an Emacs
server, using the absolute path to your checkout:

``` sh
emacsclient --eval '
(progn
  (load "/absolute/path/to/emacs-config/tests/run-agent-plans.el" nil t)
  (wgn-agent-plans-run-tests))
'
```

The runner returns the test report and signals an error on failure.
GitHub Actions runs the same suite on pushes and pull requests. The test
that syncs plans into an org-roam database runs only where org-roam is on
`load-path`, and is skipped elsewhere, GitHub Actions included.

The guest image provisioning tests and the Gondolin runner tests use
Node's built-in test runner. The runner tests replace the Gondolin SDK
with a stub, so they need no VM:

``` sh
node --test tests/image.test.mjs tests/offline-run.test.mjs
```

On Darwin, the packaged image tools are tested in a Nix derivation with an
empty inherited PATH. This test creates an ext4 filesystem and a compressed
initramfs without network access:

``` sh
nix build .#checks.aarch64-darwin.offline-run-image-tools
```

On Linux, `offline-run-sandbox` runs the Bubblewrap runner against a local
HTTP server and checks what the command can see and reach. It needs a
builder that allows unprivileged user namespaces inside the build sandbox:

``` sh
nix build .#checks.x86_64-linux.offline-run-sandbox
```

GitHub Actions runs these checks alongside the ERT and Node tests.
