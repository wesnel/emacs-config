# Run inside eca-bwrap by its Nix test. $1 is a port on the host's loopback
# where an HTTP server answers /hello, and 127.0.0.1 is allowed.
#
# "offline" as $3 marks an --offline session, which must reach nothing: $2 is
# then a --ro-mount guest path holding `file', and $4 a store path outside the
# sandbox's closure. curl and getent are on PATH. "limited" as $5 means the
# session was given --memory 256M, which a command cannot exceed.
set -euo pipefail

port="$1"
mode="${3:-}"

fail() {
    printf 'bwrap-guest: %s\n' "$1" >&2
    exit 1
}

if [[ "$mode" == offline ]]; then
    ro="$2"
    outside="$4"

    if curl -s --noproxy '*' --max-time 5 "http://127.0.0.1:$port/hello" >/dev/null; then
        fail "the host's loopback is reachable"
    fi
    if curl -s --max-time 5 https://example.com >/dev/null; then
        fail "example.com is reachable"
    fi
    if getent hosts example.com >/dev/null; then
        fail "example.com resolves"
    fi
    getent hosts localhost >/dev/null || fail "localhost does not resolve from the /etc stub"
    [[ -z "${HTTPS_PROXY:-}${HTTP_PROXY:-}${https_proxy:-}${http_proxy:-}" ]] || fail "a proxy is set"

    [[ "$(cd /etc && echo *)" == "group hosts nsswitch.conf passwd" ]] || fail "/etc holds $(cd /etc && echo *)"
    [[ -z "$(ls -A /run)" ]] || fail "/run is not empty"
    [[ -z "$(ls -A /var/run)" ]] || fail "/var/run is not empty"
    for path in /run/secrets /etc/shadow /nix/var/nix/daemon-socket /var/run/docker.sock /run/docker.sock \
        "$HOME/.ssh" "$outside"; do
        [[ ! -e "$path" ]] || fail "$path is visible"
    done
    # The workspace lives under $HOME in the test, and nothing else may.
    inside="${PWD#"$HOME"/}"
    [[ "$(ls -A "$HOME")" == "${inside%%/*}" ]] || fail "\$HOME holds $(ls -A "$HOME")"
    [[ -z "${FOO_TOKEN:-}" ]] || fail "the host environment leaked"
    [[ "${TEST_VAR:-}" == ok ]] || fail "--env was not applied"

    [[ "$(cat "$ro/file")" == hello ]] || fail "the read-only mount is not readable"
    if (: >>"$ro/file") 2>/dev/null || touch "$ro/new" 2>/dev/null; then
        fail "the read-only mount is writable"
    fi
    if touch "$PWD/written" 2>/dev/null; then
        fail "the workspace is writable"
    fi
    touch /tmp/written "$HOME/written" || fail "/tmp or \$HOME is not writable"

    if [[ "${5:-}" == limited ]]; then
        # Doubles a string up to 1 GiB, which the memory limit must kill
        # first; without one it ends normally and the test fails.
        if awk 'BEGIN { s = "x"; while (length(s) < 2^30) s = s s }' 2>/dev/null; then
            fail "a command allocated past --memory"
        fi
    fi
    exit 0
fi

[[ ! -e "$HOME/.ssh" ]] || fail "~/.ssh is visible"
[[ -z "${SSH_AUTH_SOCK:-}" ]] || fail "SSH_AUTH_SOCK leaked"
[[ -z "${FOO_TOKEN:-}" ]] || fail "the host environment leaked"
[[ "${TEST_VAR:-}" == ok ]] || fail "--env was not applied"
[[ ! -e "$HOME/.cache/eca-bwrap-proxy" ]] || fail "the proxy's private key is visible"
[[ -r "$SSL_CERT_FILE" ]] || fail "the proxy CA is not readable"

touch "$PWD/written" || fail "the workspace is not writable"
touch "$HOME/.cache/eca/written" || fail "the state directory is not writable"
if touch /etc/eca-bwrap-test 2>/dev/null; then
    fail "/ is writable"
fi

# The host's loopback is not this one, so nothing answers here directly.
if curl -sf --noproxy '*' --max-time 5 "http://127.0.0.1:$port/hello" >/dev/null; then
    fail "the host network is reachable without the proxy"
fi

[[ "$(curl -sf --max-time 10 "http://127.0.0.1:$port/hello")" == hello ]] ||
    fail "an allowed host is unreachable through the proxy"

status="$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 http://blocked.invalid/)"
[[ "$status" == 403 ]] || fail "a refused host answered $status"

# Refused at CONNECT, so curl sees the proxy fail the tunnel.
if curl -s --max-time 10 https://blocked.invalid/ >/dev/null; then
    fail "a refused HTTPS host was reachable"
fi
