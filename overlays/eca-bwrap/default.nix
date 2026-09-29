{
  bubblewrap,
  eca,
  lib,
  mitmproxy,
  python3,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation {
  pname = "eca-bwrap";

  inherit (eca) version;

  dontUnpack = true;
  strictDeps = true;

  # The addon is installed into this package rather than referenced where it
  # sits in the flake, so that the closure carries it: a path into the source
  # tree is not a dependency anything would copy to another machine.
  installPhase = ''
    runHook preInstall

    install -Dm644 ${./allowlist.py} $out/libexec/allowlist.py
    install -Dm755 ${./eca-bwrap.sh} $out/bin/eca-bwrap

    substituteInPlace $out/bin/eca-bwrap \
      --replace-fail '@allowlist@' "$out/libexec/allowlist.py" \
      --replace-fail '@bwrap@' '${lib.getExe bubblewrap}' \
      --replace-fail '@mitmdump@' '${mitmproxy}/bin/mitmdump' \
      --replace-fail '@python@' '${lib.getExe python3}'

    # Both backends expose the same command for the Emacs launch settings.
    # Home Manager installs only the selected backend.
    ln -s eca-bwrap $out/bin/eca-sandbox

    runHook postInstall
  '';

  meta = {
    description = "Runs the ECA server under bubblewrap with an intercepting proxy";
    mainProgram = "eca-bwrap";
    platforms = lib.platforms.linux;
  };
}
