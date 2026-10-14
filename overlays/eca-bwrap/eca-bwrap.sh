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
pr_loop="${ECA_SANDBOX_PR_LOOP:-}"
plans=""
guard_config=""
env_args=()
protect=()
command=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --) shift; command=("$@"); break ;;
        --workspace|--config|--state|--log|--eca|--allow-host|--env|--image|--guest-path|--pr-loop|--protect|--plans|--guard-config)
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
                --pr-loop) pr_loop="$2" ;;
                --protect) protect+=("${2#/}") ;;
                --plans) plans="$2" ;;
                --guard-config) guard_config="$2" ;;
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
    for pid in "${proxy_pid:-}" "${relay_pid:-}" "${pr_loop_pid:-}"; do
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

# A review-loop session also runs `pr-loop guard' before every tool call. It
# goes into a staged copy, so ordinary sessions and the config ECA writes out
# here never carry it.
if [[ -n "$pr_loop" ]]; then
    staged="$relay/eca-config"
    mkdir -p "$staged"
    if [[ -d "$config" ]]; then
        cp -RL "$config/." "$staged/"
        chmod -R u+w "$staged"
    fi
    if ! "@python@" - "$staged/config.json" <<'PY'
import json, os, sys
path = sys.argv[1]
config = json.load(open(path)) if os.path.exists(path) else {}
config.setdefault("hooks", {})["pr-loop-guard"] = {
    "type": "preToolCall",
    "matcher": ".*",
    "visible": False,
    "description": "Deny writes to GitHub and Linear outside pr-loop",
    "actions": [{"type": "shell", "shell": "pr-loop guard"}],
}
json.dump(config, open(path, "w"), indent=2)
PY
    then
        printf 'eca-bwrap: cannot add the pr-loop guard to %s\n' "$staged/config.json" >&2
        exit 2
    fi
    config="$staged"
fi

if [[ -d "$config" ]]; then
    bwrap_args+=(--ro-bind "$config" "$HOME/.config/eca")
fi

# The file `agent-plan' and its guard read.
if [[ -n "$guard_config" ]]; then
    if [[ -f "$guard_config" ]]; then
        bwrap_args+=(--ro-bind "$guard_config" "$HOME/.config/agent-tools/config.json")
    else
        printf 'eca-bwrap: --guard-config %s does not exist; it is not mounted\n' "$guard_config" >&2
    fi
fi

# Agents write their plans here, at the path the config's `plans.dir' names. A
# review-loop session logs only through the relay, so it gets no mount.
if [[ -n "$plans" && -z "$pr_loop" ]]; then
    if [[ -d "$plans" ]]; then
        plans="$(cd "$plans" && pwd)"
        bwrap_args+=(--bind "$plans" "$plans")
    else
        printf 'eca-bwrap: --plans %s does not exist; plans are not mounted\n' "$plans" >&2
    fi
fi

# The sandbox gets a `pr-loop' client and no GitHub credentials, so every write
# the review loop makes to GitHub passes through `sandbox-relay serve' out
# here, which runs `pr-loop' with the login, and `pr-loop' enforces the loop's
# branch policy. Its socket sits in the relay directory, which the sandbox
# already sees.
if [[ -n "$pr_loop" ]]; then
    sandbox-relay serve --workspace "$workspace" --socket "$relay/sandbox-relay.sock" \
        --program pr-loop --program agent-plan >/dev/null &
    pr_loop_pid=$!

    for _ in $(seq 1 100); do
        if [[ -S "$relay/sandbox-relay.sock" ]]; then
            break
        fi
        sleep 0.05
    done

    if [[ ! -S "$relay/sandbox-relay.sock" ]]; then
        printf 'eca-bwrap: sandbox-relay serve failed to start\n' >&2
        exit 1
    fi

    # $HOME is empty inside, so the identity has to be spelled out. Commits
    # are unsigned: no key reaches the sandbox, and the loop's signing pass
    # re-signs them out here.
    # Git hooks and config, which the next git command run out here would
    # obey, and other agents' project settings, which can name hooks that run
    # outside any sandbox, are read-only inside. A bind that does not exist
    # yet cannot be made, so those directories are left out. Any --protect
    # replaces this list rather than adding to it.
    if [[ ${#protect[@]} -eq 0 ]]; then
        protect=(.git/hooks .git/config .claude .codex)
    fi
    for protected in "${protect[@]}"; do
        if [[ -e "$workspace/$protected" ]]; then
            bwrap_args+=(--ro-bind "$workspace/$protected" "$workspace/$protected")
        fi
    done

    bwrap_args+=(
        --setenv PATH "$pr_loop:$PATH"
        --setenv SANDBOX_RELAY_URL "unix:$inner/sandbox-relay.sock"
        --setenv PR_LOOP_SESSION 1
        --setenv GIT_CONFIG_COUNT 1
        --setenv GIT_CONFIG_KEY_0 commit.gpgsign
        --setenv GIT_CONFIG_VALUE_0 false
    )
    for pair in name:NAME email:EMAIL; do
        if value="$(git -C "$workspace" config "user.${pair%%:*}")"; then
            bwrap_args+=(
                --setenv "GIT_AUTHOR_${pair#*:}" "$value"
                --setenv "GIT_COMMITTER_${pair#*:}" "$value"
            )
        fi
    done
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
