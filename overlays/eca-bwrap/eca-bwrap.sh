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
guest_dir="${ECA_SANDBOX_GUEST_PATH:-}"
plans=""
tools=""
guard_config=""
offline=""
memory=""
cpus=""
ro_mounts=()
env_args=()
protect=()
command=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --) shift; command=("$@"); break ;;
        --offline) offline=1; shift ;;
        --workspace|--config|--state|--log|--eca|--allow-host|--env|--image|--guest-path|--pr-loop|--protect|--plans|--tools|--guard-config|--ro-mount|--memory|--cpus)
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
                --guest-path) guest_dir="$2" ;;
                --protect) protect+=("${2#/}") ;;
                --plans) plans="$2" ;;
                --tools) tools="$2" ;;
                --guard-config) guard_config="$2" ;;
                --ro-mount) ro_mounts+=("$2") ;;
                --memory) memory="$2" ;;
                --cpus) cpus="$2" ;;
                --env)
                    if [[ "$2" != *=* ]]; then
                        printf 'eca-bwrap: --env expects KEY=VALUE, got %s\n' "$2" >&2
                        exit 2
                    fi
                    env_args+=(--setenv "${2%%=*}" "${2#*=}")
                    ;;
                # Accepted so that one .dir-locals.el serves every backend;
                # this one has no guest image to select.
                --image) ;;
            esac
            shift 2
            ;;
        *) printf 'eca-bwrap: unknown option %s\n' "$1" >&2; exit 2 ;;
    esac
done

workspace="$(cd "$workspace" && pwd)"
state="${state%/}"

allow="$(printf '%s\n' "${allow//,/$'\n'}" | awk 'NF && !seen[$0]++' | paste -sd, -)"

# An offline sandbox has no proxy and nothing but loopback, so anything that
# would reach a host is refused rather than left to fail inside.
if [[ -n "$offline" ]]; then
    networked=()
    if [[ -n "$allow" ]]; then networked+=(--allow-host); fi
    if [[ -n "$pr_loop" ]]; then networked+=(--pr-loop); fi
    if [[ -n "$log" ]]; then networked+=(--log); fi
    if [[ ${#networked[@]} -gt 0 ]]; then
        printf -v list '%s, ' "${networked[@]}"
        printf 'eca-bwrap: --offline has no network, so it cannot take %s\n' "${list%, }" >&2
        exit 2
    fi
    # These serve an agent session, and --plans would give unvetted code a
    # writable host directory.
    agentic=()
    if [[ -n "$plans" ]]; then agentic+=(--plans); fi
    if [[ -n "$tools" ]]; then agentic+=(--tools); fi
    if [[ -n "$guard_config" ]]; then agentic+=(--guard-config); fi
    if [[ ${#agentic[@]} -gt 0 ]]; then
        printf -v list '%s, ' "${agentic[@]}"
        printf 'eca-bwrap: --offline runs no agent, so it cannot take %s\n' "${list%, }" >&2
        exit 2
    fi
    if [[ -n "$guest_dir" && ( "$guest_dir" != /* || "$guest_dir" =~ ^/+$ ) ]]; then
        printf 'eca-bwrap: --guest-path expects an absolute path other than /, got %s\n' "$guest_dir" >&2
        exit 2
    fi
    if [[ ${#command[@]} -eq 0 ]]; then
        printf 'eca-bwrap: --offline runs only a command given after --\n' >&2
        exit 2
    fi
fi

# Split at the last colon: the guest side is an absolute path, and host paths
# are the likelier of the two to contain one.
ro_binds=()
for spec in "${ro_mounts[@]}"; do
    host="${spec%:*}"
    guest="${spec##*:}"
    if [[ "$spec" != *:* || -z "$host" || "$guest" != /* || "$guest" =~ ^/+$ ]]; then
        printf 'eca-bwrap: --ro-mount expects HOST:GUEST with an absolute GUEST, got %s\n' "$spec" >&2
        exit 2
    fi
    if [[ ! -d "$host" ]]; then
        printf 'eca-bwrap: --ro-mount %s is not a directory\n' "$host" >&2
        exit 2
    fi
    ro_binds+=(--ro-bind "$(cd "$host" && pwd)" "$guest")
done

# Validated as the Gondolin backend validates them, so that one set of
# arguments is right or wrong for both.
if [[ -n "$memory" && ! "$memory" =~ ^[1-9][0-9]*[KMGTkmgt]?$ ]]; then
    printf 'eca-bwrap: --memory expects a size such as 4G, got %s\n' "$memory" >&2
    exit 2
fi
if [[ -n "$cpus" && ! "$cpus" =~ ^[1-9][0-9]*$ ]]; then
    printf 'eca-bwrap: --cpus expects a positive whole number, got %s\n' "$cpus" >&2
    exit 2
fi

# Bubblewrap sets no limits itself, so an --offline run with --memory or
# --cpus goes in a transient systemd scope of the user manager, which does.
# systemd reads K, M, G and T as powers of 1024, in capitals only. Without a
# reachable user manager the limits cannot hold, and the run is refused. A
# session without --offline runs unlimited, with a notice. This is the one
# statement of how this backend treats the limits; the README and callers
# point here.
limit=()
if [[ -n "$offline" && -n "$memory$cpus" ]]; then
    props=()
    if [[ -n "$memory" ]]; then
        props+=(-p "MemoryMax=${memory^^}" -p MemorySwapMax=0)
    fi
    if [[ -n "$cpus" ]]; then
        props+=(-p "CPUQuota=$((cpus * 100))%")
    fi
    limit=(systemd-run --user --scope --quiet "${props[@]}")
    if ! command -v systemd-run >/dev/null || ! "${limit[@]}" true 2>/dev/null; then
        printf 'eca-bwrap: --memory and --cpus need systemd-run and a reachable user manager to be enforced\n' >&2
        exit 2
    fi
elif [[ -n "$memory$cpus" ]]; then
    printf 'eca-bwrap: bubblewrap sets no resource limits, so --memory and --cpus are not enforced\n' >&2
fi

relay="$(mktemp -d "${TMPDIR:-/tmp}/eca-bwrap.XXXXXX")"

if [[ -n "$state" ]]; then
    mkdir -p "$state"
fi

cleanup() {
    for pid in "${proxy_pid:-}" "${relay_pid:-}" "${pr_loop_pid:-}"; do
        if [[ -n "$pid" ]]; then
            kill "$pid" 2>/dev/null || true
        fi
    done
    rm -rf "$relay"
}

trap cleanup EXIT INT TERM HUP

# The guest sees the relay directory here, read-only.
inner=/tmp/.eca-proxy
proxy="http://127.0.0.1:3128"

if [[ -z "$offline" ]]; then
    # Outside $state, which the guest can write, because this holds the CA's
    # private key. It persists so that the CA stays stable between sessions.
    # An empty --state binds no state, and the CA stays where the default
    # would put it.
    confdir="${state:-${XDG_CACHE_HOME:-$HOME/.cache}/eca-bwrap}-proxy"
    proxy_log="$confdir/mitmdump.log"

    mkdir -p "$confdir"

    # Port 0 lets the kernel pick, but mitmdump does not report the choice,
    # so a fixed-but-unused port is found first.
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

    # Wait for the port, not for the CA file: the CA is kept between runs, so
    # it can exist before this proxy is listening.
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

    guest_path="${PATH:-}"

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
else
    # For code nobody has vetted for this machine: no host file system but
    # the store paths its tools need, and nothing of /etc but the names a
    # program looks up. Sockets and secrets under /run cannot be reached.
    guest_path="@offlinePath@"
    etc="$relay/etc"
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
        --setenv PATH "$guest_path"
    )
fi

# Read-only offline, for the same reason as the rest of that file system, and
# at --guest-path if given. A session keeps the workspace where it lives,
# which the --pr-loop binds and relay below rely on.
if [[ -n "$offline" ]]; then
    guest_dir="${guest_dir:-$workspace}"
    bwrap_args+=(--ro-bind "$workspace" "$guest_dir")
else
    guest_dir="$workspace"
    bwrap_args+=(--bind "$workspace" "$workspace")
fi

bwrap_args+=(
    --chdir "$guest_dir"
    --setenv HOME "$HOME"
    # ECA resolves its config and cache through these rather than HOME.
    --setenv XDG_CONFIG_HOME "$HOME/.config"
    --setenv XDG_CACHE_HOME "$HOME/.cache"
)

if [[ -z "$offline" ]]; then
    bwrap_args+=(
        --ro-bind "$relay" "$inner"
        --setenv HTTPS_PROXY "$proxy"
        --setenv HTTP_PROXY "$proxy"
        --setenv https_proxy "$proxy"
        --setenv http_proxy "$proxy"
        --setenv no_proxy ""
        --setenv NO_PROXY ""
        # The config-file equivalent does not cover the server's startup
        # traffic; this variable does.
        --setenv SSL_CERT_FILE "$inner/ca.pem"
        --setenv NODE_EXTRA_CA_CERTS "$inner/ca.pem"
    )
fi

if [[ -n "$state" ]]; then
    bwrap_args+=(--bind "$state" "$HOME/.cache/eca")
fi

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

# An ordinary session runs `agent-plan' and the plan guard itself, so it gets
# the client directory on PATH, read-only, and no relay. A review-loop session
# has the same directory through --pr-loop.
if [[ -n "$tools" && -z "$pr_loop" ]]; then
    if [[ -d "$tools" ]]; then
        tools="$(cd "$tools" && pwd)"
        bwrap_args+=(--ro-bind "$tools" "$tools" --setenv PATH "$tools:$guest_path")
    else
        printf 'eca-bwrap: --tools %s does not exist; it is not on PATH\n' "$tools" >&2
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

bwrap_args+=("${ro_binds[@]}" "${env_args[@]}")

if [[ ${#command[@]} -eq 0 ]]; then
    command=("$eca" server)
fi

# Not exec: that would replace this shell and with it the trap that removes
# the relay directory.
if [[ -n "$offline" ]]; then
    "${limit[@]}" "@bwrap@" "${bwrap_args[@]}" -- "${command[@]}"
    exit
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
