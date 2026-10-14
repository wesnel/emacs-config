{
  bash,
  bubblewrap,
  closureInfo,
  coreutils,
  curl,
  diffutils,
  eca,
  findutils,
  gawk,
  getent,
  git,
  gnugrep,
  gnused,
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
      --replace-fail '@offlinePath@' '${lib.makeBinPath finalAttrs.passthru.offlineTools}' \
      --replace-fail '@offlineStorePaths@' '${finalAttrs.passthru.offlineClosure}/store-paths' \
      --replace-fail '@python@' '${lib.getExe python3}' \
      --replace-fail '@socat@' '${lib.getExe socat}'

    runHook postInstall
  '';

  # All an --offline command finds on PATH. The sandbox binds these and their
  # runtime dependencies, and nothing else, from the store; a run that needs
  # more, such as a Go toolchain, is handed it with --ro-mount.
  passthru.offlineTools = [bash coreutils diffutils findutils gawk git gnugrep gnused];
  passthru.offlineClosure = closureInfo {rootPaths = finalAttrs.passthru.offlineTools;};

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

      mkdir -p "$HOME/notes/agents" "$TMPDIR/bin" "$TMPDIR/client" "$TMPDIR/tools"
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

      # An ordinary session gets the tools and starts no relay.
      rm "$TMPDIR/relay-args"
      printf '#!${lib.getExe bash}\necho tool\n' > "$TMPDIR/tools/agent-plan"
      chmod +x "$TMPDIR/tools/agent-plan"

      PATH="$TMPDIR/bin:$PATH" ${finalAttrs.finalPackage}/bin/eca-bwrap \
        --allow-host 127.0.0.1 \
        --log "$TMPDIR/log.jsonl" \
        --env TEST_VAR=ok \
        --plans "$HOME/notes/agents" \
        --guard-config "$TMPDIR/guard.json" \
        --tools "$TMPDIR/tools" \
        -- ${lib.getExe bash} ${../../tests/bwrap-guest.sh} "$port" "$HOME/notes/agents" "" "$TMPDIR/tools"
      [ -f "$HOME/notes/agents/written" ]
      [ ! -e "$TMPDIR/relay-args" ]

      grep -q '"dir": "response", "status": 200, "url": "http://127.0.0.1:'"$port"'/hello"' "$TMPDIR/log.jsonl"
      grep -q '"url": "http://blocked.invalid/"' "$TMPDIR/log.jsonl"
      grep -q '"method": "CONNECT", "url": "blocked.invalid:443"' "$TMPDIR/log.jsonl"
      if grep -q '"status": 403' "$TMPDIR/log.jsonl"; then
        echo "a refusal was logged as a response" >&2
        exit 1
      fi
      [ -f "$HOME/project/written" ]

      # An empty state is not bound, and the proxy's CA stays out of the
      # workspace.
      ${finalAttrs.finalPackage}/bin/eca-bwrap --state "" \
        -- ${lib.getExe bash} -c '[ ! -e "$HOME/.cache/eca" ]'
      [ ! -e "$HOME/project/-proxy" ]

      # An --offline run reaches nothing and sees only its tools, what it is
      # handed read-only, and an empty /run, /var/run, /tmp and $HOME.
      mkdir "$TMPDIR/ro"
      echo hello > "$TMPDIR/ro/file"
      cp ${../../tests/bwrap-guest.sh} "$TMPDIR/ro/guest.sh"

      # The probes are bound in beside the tools.
      probes=()
      while read -r path; do
        probes+=(--ro-mount "$path:$path")
      done < <(comm -13 \
        <(sort ${finalAttrs.passthru.offlineClosure}/store-paths) \
        <(sort ${closureInfo {rootPaths = [curl getent];}}/store-paths))

      ${finalAttrs.finalPackage}/bin/eca-bwrap --offline --state "" --config "" \
        --ro-mount "$TMPDIR/ro:/mnt/ro" "''${probes[@]}" \
        --env TEST_VAR=ok \
        --env PATH=${lib.makeBinPath ([curl getent] ++ finalAttrs.passthru.offlineTools)} \
        -- ${lib.getExe bash} /mnt/ro/guest.sh "$port" /mnt/ro offline ${mitmproxy}
      [ ! -e "$TMPDIR/ro/new" ]

      # --guest-path puts the workspace there, read-only, and starts the
      # command in it; the host path is not bound.
      echo hello > "$HOME/project/guest-file"
      ${finalAttrs.finalPackage}/bin/eca-bwrap --offline --state "" --config "" --guest-path /src \
        -- ${lib.getExe bash} -c '
          [ "$PWD" = /src ] && [ "$(cat guest-file)" = hello ] &&
            [ ! -e "'"$HOME"'/project" ] && ! touch /src/new 2>/dev/null'
      if ${finalAttrs.finalPackage}/bin/eca-bwrap --offline --guest-path src -- true 2>"$TMPDIR/err"; then
        echo "--offline took a relative --guest-path" >&2
        exit 1
      fi
      grep -q 'guest-path expects an absolute path' "$TMPDIR/err"

      for args in "--plans $HOME/notes/agents" "--tools $TMPDIR/tools" "--guard-config $TMPDIR/guard.json"; do
        if ${finalAttrs.finalPackage}/bin/eca-bwrap --offline $args -- true 2>"$TMPDIR/err"; then
          echo "--offline took $args" >&2
          exit 1
        fi
        grep -q 'runs no agent, so it cannot take' "$TMPDIR/err"
      done

      for args in "--allow-host 127.0.0.1" "--pr-loop $TMPDIR/client" "--log $TMPDIR/offline.jsonl"; do
        if ${finalAttrs.finalPackage}/bin/eca-bwrap --offline $args -- true 2>"$TMPDIR/err"; then
          echo "--offline took $args" >&2
          exit 1
        fi
        grep -q 'has no network, so it cannot take' "$TMPDIR/err"
      done
      if ${finalAttrs.finalPackage}/bin/eca-bwrap --offline 2>"$TMPDIR/err"; then
        echo "--offline ran without a command" >&2
        exit 1
      fi
      grep -q 'runs only a command given after --' "$TMPDIR/err"

      # Limits are enforced or the run is refused: without systemd-run on
      # PATH it must not start the command.
      if PATH=${lib.makeBinPath [coreutils gawk]} ${finalAttrs.finalPackage}/bin/eca-bwrap \
        --offline --memory 2G --cpus 2 -- ${lib.getExe bash} -c true 2>"$TMPDIR/err"; then
        echo "--offline ran its limits without systemd-run" >&2
        exit 1
      fi
      grep -q 'need systemd-run and a reachable user manager' "$TMPDIR/err"

      # Where the builder has a user manager, the limit must hold inside.
      if command -v systemd-run >/dev/null && systemd-run --user --scope --quiet true 2>/dev/null; then
        ${finalAttrs.finalPackage}/bin/eca-bwrap --offline --state "" --config "" \
          --memory 256M --cpus 1 \
          --ro-mount "$TMPDIR/ro:/mnt/ro" "''${probes[@]}" \
          --env TEST_VAR=ok \
          --env PATH=${lib.makeBinPath ([curl getent] ++ finalAttrs.passthru.offlineTools)} \
          -- ${lib.getExe bash} /mnt/ro/guest.sh "$port" /mnt/ro offline ${mitmproxy} limited
      fi
      touch $out
    '';

  meta = {
    description = "Runs the ECA server under bubblewrap with an intercepting proxy";
    mainProgram = "eca-bwrap";
    platforms = lib.platforms.linux;
  };
})
