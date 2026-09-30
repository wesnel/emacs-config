#!/usr/bin/env bash
# Runs the ECA server under bubblewrap with a local intercepting proxy, for
# hosts that cannot run the Gondolin backend.
#
# The sandbox has its own network namespace with nothing in it but loopback,
# so the proxy is the only way out: a relay inside listens on the loopback
# port that $HTTPS_PROXY names and forwards to a Unix socket bound in from
# this side, where a second relay reaches the proxy itself. A process that
# ignores the proxy variables gets no network at all rather than the host's.

set -euo pipefail

workspace="${ECA_SANDBOX_WORKSPACE:-$PWD}"
config="${ECA_SANDBOX_CONFIG:-${XDG_CONFIG_HOME:-$HOME/.config}/eca}"
state="${ECA_SANDBOX_STATE:-${XDG_CACHE_HOME:-$HOME/.cache}/eca-bwrap}"
log="${ECA_SANDBOX_LOG:-}"
eca="${ECA_SANDBOX_ECA:-eca}"
# Empty means no host is reachable.
allow="${ECA_SANDBOX_ALLOW_HOSTS:-}"
env_args=()
command=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --) shift; command=("$@"); break ;;
        --workspace|--config|--state|--log|--eca|--allow-host|--env|--image|--guest-path)
            if [[ $# -lt 2 ]]; then
                printf 'eca-bwrap: %s needs a value\n' "$1" >&2
                exit 2
            fi
            case "$1" in
                --workspace) workspace="$2" ;;
                --config) config="$2" ;;
                --state) state="$2" ;;
                --log) log="$2" ;;
                --eca) eca="$2" ;;
                --allow-host) allow="$allow,$2" ;;
                --env)
                    if [[ "$2" != *=* ]]; then
                        printf 'eca-bwrap: --env expects KEY=VALUE, got %s\n' "$2" >&2
                        exit 2
                    fi
                    env_args+=(--setenv "${2%%=*}" "${2#*=}")
                    ;;
                # Accepted so that one .dir-locals.el serves every backend;
                # this one has no guest image to select.
                --image|--guest-path) ;;
            esac
            shift 2
            ;;
        *) printf 'eca-bwrap: unknown option %s\n' "$1" >&2; exit 2 ;;
    esac
done

workspace="$(cd "$workspace" && pwd)"
state="${state%/}"

allow="$(printf '%s\n' "${allow//,/$'\n'}" | awk 'NF && !seen[$0]++' | paste -sd, -)"

# Outside $state, which the guest can write, because this holds the CA's
# private key. It persists so that the CA stays stable between sessions.
confdir="$state-proxy"
proxy_log="$confdir/mitmdump.log"
relay="$(mktemp -d "${TMPDIR:-/tmp}/eca-bwrap.XXXXXX")"

mkdir -p "$state" "$confdir"

cleanup() {
    for pid in "${proxy_pid:-}" "${relay_pid:-}"; do
        if [[ -n "$pid" ]]; then
            kill "$pid" 2>/dev/null || true
        fi
    done
    rm -rf "$relay"
}

trap cleanup EXIT INT TERM HUP

# Port 0 lets the kernel pick, but mitmdump does not report the choice, so a
# fixed-but-unused port is found first.
port="$(
    "@python@" - <<'PY'
import socket
s = socket.socket()
s.bind(("127.0.0.1", 0))
print(s.getsockname()[1])
s.close()
PY
)"

# Lazy, so that a refused host is never dialled on the guest's behalf.
ECA_SANDBOX_ALLOW_HOSTS="$allow" ECA_SANDBOX_LOG="$log" \
    "@mitmdump@" \
    --listen-host 127.0.0.1 \
    --listen-port "$port" \
    --set confdir="$confdir" \
    --set connection_strategy=lazy \
    -s "@allowlist@" \
    >"$proxy_log" 2>&1 &
proxy_pid=$!

"@socat@" "UNIX-LISTEN:$relay/proxy.sock,fork" "TCP:127.0.0.1:$port" \
    2>>"$confdir/relay.log" &
relay_pid=$!

ca="$confdir/mitmproxy-ca-cert.pem"

listening() {
    (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null && [[ -S "$relay/proxy.sock" && -r "$ca" ]]
}

# Wait for the port, not for the CA file: the CA is kept between runs, so it
# can exist before this proxy is listening.
for _ in $(seq 1 100); do
    if listening; then
        break
    fi
    sleep 0.1
done

if ! listening; then
    printf 'eca-bwrap: proxy failed to start; see %s\n' "$proxy_log" >&2
    exit 1
fi

cp "$ca" "$relay/ca.pem"

printf 'eca-bwrap: egress only through the proxy; allowed hosts: %s\n' "${allow:-none}" >&2

# The guest sees the relay directory here, read-only.
inner=/tmp/.eca-proxy
proxy="http://127.0.0.1:3128"

bwrap_args=(
    --ro-bind / /
    --dev /dev
    --proc /proc
    --tmpfs /tmp
    # Hides keys, tokens and agent sockets. What the session needs from
    # $HOME is bound back below.
    --tmpfs "$HOME"
    --unshare-all
    --new-session
    --die-with-parent
    --clearenv
)

# Agent sockets live here as well as under $HOME.
if [[ -d /run/user ]]; then
    bwrap_args+=(--tmpfs /run/user)
fi

for name in PATH USER LOGNAME LANG LC_ALL TERM TZ LOCALE_ARCHIVE; do
    if [[ -n "${!name:-}" ]]; then
        bwrap_args+=(--setenv "$name" "${!name}")
    fi
done

# ~/.nix-profile/bin and the like would otherwise vanish under the tmpfs.
IFS=: read -ra path_entries <<<"${PATH:-}"
for entry in "${path_entries[@]}"; do
    if [[ "$entry" == "$HOME"/* && -d "$entry" ]]; then
        bwrap_args+=(--ro-bind "$entry" "$entry")
    fi
done

bwrap_args+=(
    --bind "$workspace" "$workspace"
    --bind "$state" "$HOME/.cache/eca"
    --ro-bind "$relay" "$inner"
    --chdir "$workspace"
    --setenv HOME "$HOME"
    # ECA resolves its config and cache through these rather than HOME.
    --setenv XDG_CONFIG_HOME "$HOME/.config"
    --setenv XDG_CACHE_HOME "$HOME/.cache"
    --setenv HTTPS_PROXY "$proxy"
    --setenv HTTP_PROXY "$proxy"
    --setenv https_proxy "$proxy"
    --setenv http_proxy "$proxy"
    --setenv no_proxy ""
    --setenv NO_PROXY ""
    # The config-file equivalent does not cover the server's startup traffic;
    # this variable does.
    --setenv SSL_CERT_FILE "$inner/ca.pem"
    --setenv NODE_EXTRA_CA_CERTS "$inner/ca.pem"
)

if [[ -d "$config" ]]; then
    bwrap_args+=(--ro-bind "$config" "$HOME/.config/eca")
fi

bwrap_args+=("${env_args[@]}")

if [[ ${#command[@]} -eq 0 ]]; then
    command=("$eca" server)
fi

# The command must not start before the relay is listening, or its first
# request finds nothing on the proxy port. The relay's stderr is discarded
# because the probe below makes it report a reset connection.
# shellcheck disable=SC2016
shim='
"$0" TCP-LISTEN:3128,bind=127.0.0.1,fork,reuseaddr UNIX-CONNECT:'"$inner"'/proxy.sock 2>/dev/null &
for _ in $(seq 1 100); do
    if (exec 3<>/dev/tcp/127.0.0.1/3128) 2>/dev/null; then
        break
    fi
    sleep 0.05
done
exec "$@"'

# Not exec: that would replace this shell and with it the trap that stops the
# proxy, leaving one behind after every session.
"@bwrap@" "${bwrap_args[@]}" -- "@bash@" -c "$shim" "@socat@" "${command[@]}"
