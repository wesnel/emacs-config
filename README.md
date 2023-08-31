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