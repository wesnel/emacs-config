# Run inside eca-bwrap by its Nix test. $1 is a port on the host's loopback
# where an HTTP server answers /hello, and 127.0.0.1 is allowed.
set -euo pipefail

port="$1"

fail() {
    printf 'bwrap-guest: %s\n' "$1" >&2
    exit 1
}

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
