{
  bash,
  bubblewrap,
  closureInfo,
  coreutils,
  curl,
  diffutils,
  findutils,
  gawk,
  getent,
  git,
  gnugrep,
  gnused,
  lib,
  python3,
  runCommand,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  name = "offline-run";

  dontUnpack = true;
  strictDeps = true;

  # For patchShebangs, which under strictDeps looks only here.
  buildInputs = [bash];

  installPhase = ''
    runHook preInstall

    install -Dm755 ${./offline-run.sh} $out/bin/offline-run

    substituteInPlace $out/bin/offline-run \
      --replace-fail '@bash@' '${lib.getExe bash}' \
      --replace-fail '@bwrap@' '${lib.getExe bubblewrap}' \
      --replace-fail '@offlinePath@' '${lib.makeBinPath finalAttrs.passthru.offlineTools}' \
      --replace-fail '@offlineStorePaths@' '${finalAttrs.passthru.offlineClosure}/store-paths'

    runHook postInstall
  '';

  # All a command finds on PATH. The sandbox binds these and their runtime
  # dependencies, and nothing else, from the store; a run that needs more,
  # such as a Go toolchain, is handed it with --ro-mount.
  passthru.offlineTools = [bash coreutils diffutils findutils gawk git gnugrep gnused];
  passthru.offlineClosure = closureInfo {rootPaths = finalAttrs.passthru.offlineTools;};

  # Needs unprivileged user namespaces inside the build sandbox, so it runs
  # only where the builder allows nesting them.
  passthru.tests.sandbox = let
    run = "${finalAttrs.finalPackage}/bin/offline-run";
    # Runs the guest script with curl and getent on PATH as well.
    probe = "${coreutils}/bin/env PATH=${lib.makeBinPath ([curl getent] ++ finalAttrs.passthru.offlineTools)} ${lib.getExe bash}";
  in
    runCommand "offline-run-sandbox" {
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

      cd "$HOME/project"

      # A run reaches nothing and sees only its tools, what it is handed
      # read-only, and an empty /run, /var/run, /tmp and $HOME.
      mkdir "$TMPDIR/ro"
      echo hello > "$TMPDIR/ro/file"
      cp ${../../tests/offline-run-guest.sh} "$TMPDIR/ro/guest.sh"

      # The probes are bound in beside the tools.
      probes=()
      while read -r path; do
        probes+=(--ro-mount "$path:$path")
      done < <(comm -13 \
        <(sort ${finalAttrs.passthru.offlineClosure}/store-paths) \
        <(sort ${closureInfo {rootPaths = [curl getent];}}/store-paths))

      ${run} --offline --state "" --config "" \
        --ro-mount "$TMPDIR/ro:/mnt/ro" "''${probes[@]}" \
        -- ${probe} /mnt/ro/guest.sh "$port" /mnt/ro ${bubblewrap}
      [ ! -e "$TMPDIR/ro/new" ]

      # --guest-path puts the workspace there, read-only, and starts the
      # command in it; the host path is not bound.
      echo hello > "$HOME/project/guest-file"
      ${run} --guest-path /src \
        -- ${lib.getExe bash} -c '
          [ "$PWD" = /src ] && [ "$(cat guest-file)" = hello ] &&
            [ ! -e "'"$HOME"'/project" ] && ! touch /src/new 2>/dev/null'
      if ${run} --guest-path src -- true 2>"$TMPDIR/err"; then
        echo "offline-run took a relative --guest-path" >&2
        exit 1
      fi
      grep -q 'guest-path expects an absolute path' "$TMPDIR/err"

      for args in "--state $TMPDIR" "--config $TMPDIR" "--allow-host 127.0.0.1"; do
        if ${run} $args -- true 2>"$TMPDIR/err"; then
          echo "offline-run took $args" >&2
          exit 1
        fi
      done
      if ${run} 2>"$TMPDIR/err"; then
        echo "offline-run ran without a command" >&2
        exit 1
      fi
      grep -q 'runs only a command given after --' "$TMPDIR/err"

      # Limits are enforced or the run is refused: without systemd-run on
      # PATH it must not start the command.
      if PATH=${lib.makeBinPath [coreutils gawk]} ${run} \
        --memory 2G --cpus 2 -- ${lib.getExe bash} -c true 2>"$TMPDIR/err"; then
        echo "offline-run ran its limits without systemd-run" >&2
        exit 1
      fi
      grep -q 'need systemd-run and a reachable user manager' "$TMPDIR/err"

      # Where the builder has a user manager, the limit must hold inside.
      if command -v systemd-run >/dev/null && systemd-run --user --scope --quiet true 2>/dev/null; then
        ${run} --memory 256M --cpus 1 \
          --ro-mount "$TMPDIR/ro:/mnt/ro" "''${probes[@]}" \
          -- ${probe} /mnt/ro/guest.sh "$port" /mnt/ro ${bubblewrap} limited
      fi
      touch $out
    '';

  meta = {
    description = "Runs a command under bubblewrap with no network";
    mainProgram = "offline-run";
    platforms = lib.platforms.linux;
  };
})
