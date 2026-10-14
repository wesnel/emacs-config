{
  bash,
  bubblewrap,
  coreutils,
  curl,
  eca,
  lib,
  mitmproxy,
  python3,
  runCommand,
  socat,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "eca-bwrap";

  inherit (eca) version;

  dontUnpack = true;
  strictDeps = true;

  # For patchShebangs, which under strictDeps looks only here.
  buildInputs = [bash];

  # The addon is installed into this package rather than referenced where it
  # sits in the flake, so that the closure carries it: a path into the source
  # tree is not a dependency anything would copy to another machine.
  installPhase = ''
    runHook preInstall

    install -Dm644 ${./allowlist.py} $out/libexec/allowlist.py
    install -Dm755 ${./eca-bwrap.sh} $out/bin/eca-bwrap

    substituteInPlace $out/bin/eca-bwrap \
      --replace-fail '@allowlist@' "$out/libexec/allowlist.py" \
      --replace-fail '@bash@' '${lib.getExe bash}' \
      --replace-fail '@bwrap@' '${lib.getExe bubblewrap}' \
      --replace-fail '@mitmdump@' '${mitmproxy}/bin/mitmdump' \
      --replace-fail '@python@' '${lib.getExe python3}' \
      --replace-fail '@socat@' '${lib.getExe socat}'

    runHook postInstall
  '';

  # Needs unprivileged user namespaces inside the build sandbox, so it runs
  # only where the builder allows nesting them.
  passthru.tests.sandbox =
    runCommand "eca-bwrap-sandbox" {
      nativeBuildInputs = [coreutils curl python3];
    } ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME/.ssh" "$HOME/project"
      echo secret > "$HOME/.ssh/id_test"
      export SSH_AUTH_SOCK="$HOME/.ssh/agent.sock" FOO_TOKEN=secret

      mkdir "$TMPDIR/www"
      echo hello > "$TMPDIR/www/hello"
      port="$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')"
      python3 -m http.server --bind 127.0.0.1 --directory "$TMPDIR/www" "$port" >/dev/null 2>&1 &
      for _ in $(seq 1 50); do
        curl -sf "http://127.0.0.1:$port/hello" >/dev/null && break
        sleep 0.1
      done

      mkdir -p "$HOME/notes/agents" "$TMPDIR/bin" "$TMPDIR/client"
      printf '{"plans": {}}' > "$TMPDIR/guard.json"

      # Stands in for `sandbox-relay serve': records its arguments and
      # listens on the socket it is given.
      cat > "$TMPDIR/bin/sandbox-relay" <<EOF
      #!${lib.getExe bash}
      printf '%s\n' "\$@" > "$TMPDIR/relay-args"
      while [ \$# -gt 0 ]; do
        if [ "\$1" = --socket ]; then sock="\$2"; fi
        shift
      done
      exec python3 -c 'import socket, sys, time; s = socket.socket(socket.AF_UNIX); s.bind(sys.argv[1]); s.listen(1); time.sleep(60)' "\$sock"
      EOF
      chmod +x "$TMPDIR/bin/sandbox-relay"

      cd "$HOME/project"

      # A review-loop session logs through the relay and cannot write plans.
      PATH="$TMPDIR/bin:$PATH" ${finalAttrs.finalPackage}/bin/eca-bwrap \
        --plans "$HOME/notes/agents" \
        --guard-config "$TMPDIR/guard.json" \
        --pr-loop "$TMPDIR/client" \
        -- ${lib.getExe bash} ${../../tests/bwrap-guest.sh} "$port" "$HOME/notes/agents" loop
      [ ! -e "$HOME/notes/agents/written" ]
      grep -qx -- '--program' "$TMPDIR/relay-args"
      grep -qx pr-loop "$TMPDIR/relay-args"
      grep -qx agent-plan "$TMPDIR/relay-args"

      ${finalAttrs.finalPackage}/bin/eca-bwrap \
        --allow-host 127.0.0.1 \
        --log "$TMPDIR/log.jsonl" \
        --env TEST_VAR=ok \
        --plans "$HOME/notes/agents" \
        --guard-config "$TMPDIR/guard.json" \
        -- ${lib.getExe bash} ${../../tests/bwrap-guest.sh} "$port" "$HOME/notes/agents"
      [ -f "$HOME/notes/agents/written" ]

      grep -q '"dir": "response", "status": 200, "url": "http://127.0.0.1:'"$port"'/hello"' "$TMPDIR/log.jsonl"
      grep -q '"url": "http://blocked.invalid/"' "$TMPDIR/log.jsonl"
      grep -q '"method": "CONNECT", "url": "blocked.invalid:443"' "$TMPDIR/log.jsonl"
      if grep -q '"status": 403' "$TMPDIR/log.jsonl"; then
        echo "a refusal was logged as a response" >&2
        exit 1
      fi
      [ -f "$HOME/project/written" ]
      touch $out
    '';

  meta = {
    description = "Runs the ECA server under bubblewrap with an intercepting proxy";
    mainProgram = "eca-bwrap";
    platforms = lib.platforms.linux;
  };
})
