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

## ECA and sandboxed local workspaces

The ECA client is configured in `default.el`. Home Manager installs the
pinned ECA server and writes its machine launch settings into
`~/.emacs.d/etc/eca.el`.

``` nix
home.programs.wgn.emacs.eca = {
  enable = true;
  sandbox = {
    enable = true;
    backend = "gondolin";
    args = [
      "--http-map" "ollama:11434=127.0.0.1:11434"
      "--http-map" "docs:6280=127.0.0.1:6280"
      "--env" "OLLAMA_API_URL=http://ollama:11434"
    ];
  };
};
```

Home Manager installs an `eca-sandbox` command that runs the selected
backend with `args`. Gondolin runs a micro-VM and requires hardware
virtualization. On Linux hosts without it, select `bubblewrap`.

Neither backend allows any outbound host by default. Configure service
mappings or `--allow-host` entries as needed. `args` replaces the entire
argument list. `sandbox.enable = false` keeps the pinned server on PATH
and runs it directly.

Local sessions use the configured command. TRAMP sessions run
`eca-sandbox` on the workspace's remote host when it is on the remote
PATH, and otherwise `eca`. The launch context comes from the session's
first workspace, including on restart from a chat buffer. The editor PID
is omitted because it is not meaningful inside a sandbox or on a
different host.

For configurations used without Home Manager, set `eca-custom-command`
in your personal Emacs configuration or `~/.emacs.d/etc/eca.el`. The default
configuration otherwise finds `eca` on PATH.

### Gondolin guest image

Home Manager prepares the guest image during activation when the Gondolin
sandbox is enabled. Nix supplies the builder, `mke2fs` and a build configuration
for the host architecture. The first activation downloads Alpine packages and
guest helpers and builds the image; it requires network access and can take
several minutes.

Images are cached under `$XDG_CACHE_HOME/gondolin/eca-images` (by default
`~/.cache/gondolin/eca-images`). The cache key includes the image configuration,
architecture and Gondolin package. Activations reuse a complete cached image;
changing those inputs or removing the cached image causes another build.
Updating the ECA server alone does not rebuild the image: its Linux executable
is mounted into the guest at startup.

The image includes `gcompat` so ECA's glibc-linked binary runs on Alpine's musl,
and tools such as Git, Python, Node and ripgrep. Builds are verified before a
complete image is made available to sessions. The standalone wrapper also
prepares a missing image when it starts.

To use your own image, add `--image` followed by an image directory or Gondolin
image selector to `sandbox.args`. Home Manager skips managed-image preparation
when an explicit image is selected.
Bubblewrap uses the host filesystem and requires no guest image.

Configure model services, provider settings and agents separately from
Emacs launch settings. The sandbox selects shared skills using
`~/.config/eca/sandbox-skills.json`.

### Network policy and request logging

Both backends default to denying outbound hosts. Add repeatable
`--allow-host` arguments to allow particular hosts. Nothing is implicitly
allowed, including `models.dev`; ECA can start without fetching its model
catalogue.

For a hosted model, `--observe` allows outbound hosts while keeping HTTP/TLS
traffic passing through the inspecting proxy. It overrides the allowlist.
`--deny-host PATTERN` can still exclude hosts: `*.example.com` matches both
that domain and its subdomains, `*` matches everything, and other patterns
match an exact hostname. These two flags are supported by Gondolin only.

``` nix
home.programs.wgn.emacs.eca.sandbox = {
  enable = true;
  backend = "gondolin";
  args = [
    "--observe"
    "--log" "/Users/wgn/.cache/eca-network.jsonl"
    "--deny-host" "*.example.com"
  ];
};
```

Use an absolute host path for `--log` and create its parent directory first.
Both backends append JSONL request/response metadata (time, method, URL and
response status). TLS termination makes HTTPS URLs visible to the proxy;
the log does not include request or response bodies. A refused request is
recorded without a matching response. Logging is best effort: a failed write
does not stop the session.

Without `--log`, Gondolin's `--observe` permits traffic without recording it,
and prints that fact at startup. `--observe` changes the network policy;
filesystem isolation remains in effect. Keep the `--http-map` and `--env`
arguments from the local-model example if those services are also needed,
because `args` replaces the entire list.

### Hosted provider authentication

Gondolin's guest cannot complete `/login`, which opens a browser and waits
on a loopback port. Authenticate using ECA on the host, then add
`--share-login` to the sandbox arguments to copy provider tokens into the
guest's state directory. Combine this with an appropriate allowlist or
`--observe` to reach the provider.

Tokens are stored with chat history in `db.transit.json`. The wrapper copies
this file only when the guest has no copy; it preserves an existing file
because the guest may have refreshed its tokens. To import a fresh host
login, remove the guest's copy from the sandbox state directory first.
`--share-login` is a Gondolin option and also transfers the stored history.

### Reaching local services

Gondolin's `--http-map GUEST_HOST[:PORT]=UPSTREAM_HOST:PORT` reaches a host
service from inside the guest. `--env KEY=VALUE` sets the guest environment
in both backends.
The example above maps Ollama and the docs server and sets `OLLAMA_API_URL`
to the guest-side Ollama address. Use a synthetic guest hostname:
`localhost` resolves inside the VM and cannot identify the host service.

A mapping grants access only to the named upstream port. HTTP mappings pass
through the inspecting proxy, appear in `--log`, and rewrite the `Host`
header for the upstream. That rewrite also allows loopback services which
reject an unexpected host header as DNS rebinding.

`--tcp-map` takes the same mapping syntax but forwards raw TCP below the
proxy. Its traffic is not logged or checked by the HTTP allowlist, and no
headers are rewritten. Use it for services which do not speak HTTP.

The workspace is mounted at the same path inside the guest, so no
`eca-local-to-remote-prefix-map` is needed.

### Remote hosts and backend differences

Emacs running on another machine uses that machine's configured sandbox
for its local workspaces. Opening that machine over TRAMP from local Emacs
runs that machine's `eca-sandbox`, with the arguments configured there;
local sandbox settings are not transferred.

Gondolin prepares an image matching the host architecture and requires
hardware virtualization. On Linux, check for `/dev/kvm`; QEMU software emulation is
far too slow for this use. Cloud guests often do not expose virtualization,
so Bubblewrap is the alternative for those Linux hosts.

Bubblewrap makes `/` read-only and replaces `$HOME` with an empty tmpfs,
into which it binds the workspace and sandbox state writable, the ECA
config read-only, and the `PATH` entries under `$HOME` read-only. The
environment is cleared except for `PATH`, locale and terminal variables.
The sandbox has no network of its own: its only way out is a local
`mitmdump` proxy, which applies the allowlist and records the same JSONL
shape as Gondolin. `--allow-host` adds a domain and its subdomains;
allowing `127.0.0.1` reaches services on the host through the proxy.

The backends' flags are not interchangeable:

| Option | Gondolin | Bubblewrap |
| --- | --- | --- |
| `--allow-host`, `--log`, `--env` | Supported | Supported |
| `--image`, `--guest-path` | Configure the guest | Accepted and ignored |
| `--observe`, `--deny-host` | Supported | Unsupported |
| `--share-login` | Supported | Unsupported |
| `--http-map`, `--tcp-map` | Supported | Unsupported |

An unsupported option causes Bubblewrap to exit at startup. Choose arguments
for the selected backend rather than copying the Gondolin examples wholesale.

## Tests

The ECA host-routing tests use ERT and require only Emacs. Run them from
an Emacs server, using the absolute path to your checkout:

``` sh
emacsclient --eval '
(progn
  (load "/absolute/path/to/emacs-config/tests/run-eca.el" nil t)
  (wgn-eca-run-tests))
'
```

The runner returns the test report and signals an error on failure.
GitHub Actions runs the same suite on pushes and pull requests.

The guest image provisioning tests use Node's built-in test runner:

``` sh
node --test tests/image.test.mjs
```

The packaged image tools are tested in a Nix derivation with an empty inherited
PATH. This test creates an ext4 filesystem and a compressed initramfs without
network access:

``` sh
nix build .#checks.aarch64-darwin.eca-gondolin-image-tools
```

Use `x86_64-linux` or `aarch64-linux` for those hosts. On Linux,
`eca-bwrap-sandbox` runs the Bubblewrap wrapper against a local HTTP server
and checks what the guest can see and reach. It needs a builder that allows
unprivileged user namespaces inside the build sandbox:

``` sh
nix build .#checks.x86_64-linux.eca-bwrap-sandbox
```

GitHub Actions runs the Linux checks alongside the ERT and Node tests.
