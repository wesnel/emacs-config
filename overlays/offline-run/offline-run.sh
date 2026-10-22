#!/usr/bin/env bash
# Runs one command under bubblewrap with nothing but loopback, the workspace
# read-only and nothing else of this machine but the store paths its tools
# need and what --ro-mount hands it. For code nobody has vetted for this
# machine, such as a pull request's tests.
#
# Usage: offline-run [--workspace DIR] [--guest-path PATH] [--image IMAGE]
#          [--ro-mount HOST:GUEST]... [--memory SIZE] [--cpus N] -- CMD...
#
# The options are the Gondolin runner's, offline-run.mjs, and are validated as
# it validates them, so that one set of arguments is right or wrong for both.

set -euo pipefail

workspace="$PWD"
guest_dir=""
memory=""
cpus=""
ro_mounts=()
command=()

fail() {
    printf 'offline-run: %s\n' "$1" >&2
    exit 2
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --) shift; command=("$@"); break ;;
        # Every run is offline; accepted so that callers may say so.
        --offline) shift ;;
        --workspace|--guest-path|--image|--ro-mount|--memory|--cpus|--state|--config)
            if [[ $# -lt 2 ]]; then
                fail "$1 needs a value"
            fi
            case "$1" in
                --workspace) workspace="$2" ;;
                --guest-path) guest_dir="$2" ;;
                --ro-mount) ro_mounts+=("$2") ;;
                --memory) memory="$2" ;;
                --cpus) cpus="$2" ;;
                # Accepted so that one set of arguments serves both runners;
                # this one has no guest image to select.
                --image) ;;
                # Accepted empty, meaning nothing mounted, which is all a run
                # gets.
                --state|--config)
                    if [[ -n "$2" ]]; then
                        fail "$1 takes only an empty value: nothing of this machine is mounted but --ro-mount"
                    fi
                    ;;
            esac
            shift 2
            ;;
        *) fail "unknown option $1" ;;
    esac
done

if [[ -n "$guest_dir" && ( "$guest_dir" != /* || "$guest_dir" =~ ^/+$ ) ]]; then
    fail "--guest-path expects an absolute path other than /, got $guest_dir"
fi
if [[ ${#command[@]} -eq 0 ]]; then
    fail "runs only a command given after --"
fi

workspace="$(cd "$workspace" && pwd)"
guest_dir="${guest_dir:-$workspace}"

# Split at the last colon: the guest side is an absolute path, and host paths
# are the likelier of the two to contain one.
ro_binds=()
for spec in "${ro_mounts[@]}"; do
    host="${spec%:*}"
    guest="${spec##*:}"
    if [[ "$spec" != *:* || -z "$host" || "$guest" != /* || "$guest" =~ ^/+$ ]]; then
        fail "--ro-mount expects HOST:GUEST with an absolute GUEST, got $spec"
    fi
    if [[ ! -d "$host" ]]; then
        fail "--ro-mount $host is not a directory"
    fi
    ro_binds+=(--ro-bind "$(cd "$host" && pwd)" "$guest")
done

if [[ -n "$memory" && ! "$memory" =~ ^[1-9][0-9]*[KMGTkmgt]?$ ]]; then
    fail "--memory expects a size such as 4G, got $memory"
fi
if [[ -n "$cpus" && ! "$cpus" =~ ^[1-9][0-9]*$ ]]; then
    fail "--cpus expects a positive whole number, got $cpus"
fi

# Bubblewrap sets no limits itself, so a run with --memory or --cpus goes in a
# transient systemd scope of the user manager, which does. systemd reads K, M,
# G and T as powers of 1024, in capitals only. Without a reachable user
# manager the limits cannot hold, and the run is refused. This is the one
# statement of how this runner treats the limits; the README and callers point
# here.
limit=()
if [[ -n "$memory$cpus" ]]; then
    props=()
    if [[ -n "$memory" ]]; then
        props+=(-p "MemoryMax=${memory^^}" -p MemorySwapMax=0)
    fi
    if [[ -n "$cpus" ]]; then
        props+=(-p "CPUQuota=$((cpus * 100))%")
    fi
    limit=(systemd-run --user --scope --quiet "${props[@]}")
    if ! command -v systemd-run >/dev/null || ! "${limit[@]}" true 2>/dev/null; then
        fail "--memory and --cpus need systemd-run and a reachable user manager to be enforced"
    fi
fi

scratch="$(mktemp -d "${TMPDIR:-/tmp}/offline-run.XXXXXX")"
trap 'rm -rf "$scratch"' EXIT INT TERM HUP

# Nothing of /etc but the names a program looks up. Sockets and secrets under
# /run cannot be reached.
etc="$scratch/etc"
mkdir "$etc"
printf '%s:x:%s:%s::%s:@bash@\n' "${USER:-user}" "$(id -u)" "$(id -g)" "$HOME" >"$etc/passwd"
printf '%s:x:%s:\n' "$(id -gn 2>/dev/null || echo users)" "$(id -g)" >"$etc/group"
printf 'passwd: files\ngroup: files\nhosts: files\n' >"$etc/nsswitch.conf"
printf '127.0.0.1 localhost\n::1 localhost\n' >"$etc/hosts"

bwrap_args=()
while read -r store_path; do
    bwrap_args+=(--ro-bind "$store_path" "$store_path")
done <"@offlineStorePaths@"

bwrap_args+=(
    --ro-bind "$etc" /etc
    --dev /dev
    --proc /proc
    --tmpfs /tmp
    --tmpfs /run
    --tmpfs /var/run
    --tmpfs "$HOME"
    --unshare-all
    --new-session
    --die-with-parent
    --clearenv
    --setenv PATH "@offlinePath@"
    --ro-bind "$workspace" "$guest_dir"
    --chdir "$guest_dir"
    --setenv HOME "$HOME"
    --setenv XDG_CONFIG_HOME "$HOME/.config"
    --setenv XDG_CACHE_HOME "$HOME/.cache"
    "${ro_binds[@]}"
)

# Not exec: that would replace this shell and with it the trap that removes
# the scratch directory.
"${limit[@]}" "@bwrap@" "${bwrap_args[@]}" -- "${command[@]}"
