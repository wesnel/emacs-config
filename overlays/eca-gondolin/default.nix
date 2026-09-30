{
  bash,
  coreutils,
  cpio,
  eca-guest,
  e2fsprogs,
  findutils,
  gondolin,
  lib,
  lz4,
  makeWrapper,
  nodejs_24,
  qemu,
  runCommand,
  stdenvNoCC,
  which,
  writeText,
}: let
  imageConfig = writeText "eca-gondolin-image.json" (builtins.toJSON (
    (builtins.fromJSON (builtins.readFile ./build-config.json))
    // {arch = stdenvNoCC.hostPlatform.parsed.cpu.name;}
  ));
in
  stdenvNoCC.mkDerivation (finalAttrs: {
    pname = "eca-gondolin";

    inherit (gondolin) version;

    dontUnpack = true;
    strictDeps = true;

    nativeBuildInputs = [makeWrapper];

    # The script resolves the SDK through ECA_GONDOLIN_LIB rather than an import
    # specifier, because Node ignores NODE_PATH for ESM and the package lives in
    # the store rather than a node_modules the script can reach.
    installPhase = ''
      runHook preInstall

      install -Dm755 ${./eca-gondolin.mjs} $out/libexec/eca-gondolin.mjs

      install -Dm644 ${./image.mjs} $out/libexec/image.mjs
      install -Dm644 ${./prepare-image.mjs} $out/libexec/prepare-image.mjs

      # Keep images outside the writable ECA state mounted into the guest.
      for entry in "eca-gondolin:eca-gondolin.mjs" "eca-gondolin-prepare:prepare-image.mjs"; do
        name="''${entry%%:*}"
        script="''${entry#*:}"
        makeWrapper ${nodejs_24}/bin/node $out/bin/$name \
          --add-flags $out/libexec/$script \
          --set ECA_GONDOLIN_LIB ${gondolin}/lib/node_modules/@earendil-works/gondolin/dist/src/index.js \
          --set ECA_GONDOLIN_BUILD_CONFIG ${imageConfig} \
          --set ECA_GONDOLIN_BUILDER_ID ${gondolin} \
          --set-default ECA_SANDBOX_ECA ${eca-guest}/bin/eca \
          --run 'export ECA_GONDOLIN_IMAGE_CACHE="''${ECA_GONDOLIN_IMAGE_CACHE:-''${XDG_CACHE_HOME:-$HOME/.cache}/gondolin/eca-images}"' \
          --prefix PATH : ${lib.makeBinPath [bash coreutils cpio e2fsprogs findutils gondolin lz4 qemu which]}
      done

      runHook postInstall
    '';

    passthru.tests.image-tools = runCommand "eca-gondolin-image-tools" {} ''
      env -i HOME="$TMPDIR" TMPDIR="$TMPDIR" PATH=/nonexistent \
        NODE_OPTIONS="--import ${../../tests/gondolin-image-tools.mjs}" \
        ${finalAttrs.finalPackage}/bin/eca-gondolin-prepare
      touch $out
    '';

    meta = {
      description = "Runs the ECA server inside a Gondolin micro-VM over raw stdio";
      mainProgram = "eca-gondolin";
      inherit (gondolin.meta) platforms;
    };
  })
