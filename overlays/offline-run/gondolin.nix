{
  bash,
  coreutils,
  cpio,
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
  imageConfig = writeText "offline-run-image.json" (builtins.toJSON (
    (builtins.fromJSON (builtins.readFile ./build-config.json))
    // {arch = stdenvNoCC.hostPlatform.parsed.cpu.name;}
  ));
in
  stdenvNoCC.mkDerivation (finalAttrs: {
    pname = "offline-run";

    inherit (gondolin) version;

    dontUnpack = true;
    strictDeps = true;

    nativeBuildInputs = [makeWrapper];

    # The scripts resolve the SDK through OFFLINE_RUN_GONDOLIN_LIB rather than
    # an import specifier, because Node ignores NODE_PATH for ESM and the
    # package lives in the store rather than a node_modules the scripts can
    # reach.
    #
    # The builder ID is the version rather than the store path: the image is
    # Gondolin's guest plus downloaded packages, and a rebuild of the same
    # release against a newer Node or QEMU would produce the same image after
    # minutes of downloading.
    installPhase = ''
      runHook preInstall

      install -Dm644 ${./offline-run.mjs} $out/libexec/offline-run.mjs
      install -Dm644 ${./image.mjs} $out/libexec/image.mjs
      install -Dm644 ${./prepare-image.mjs} $out/libexec/prepare-image.mjs

      for entry in "offline-run:offline-run.mjs" "offline-run-prepare:prepare-image.mjs"; do
        name="''${entry%%:*}"
        script="''${entry#*:}"
        makeWrapper ${nodejs_24}/bin/node $out/bin/$name \
          --add-flags $out/libexec/$script \
          --set OFFLINE_RUN_GONDOLIN_LIB ${gondolin}/lib/node_modules/@earendil-works/gondolin/dist/src/index.js \
          --set OFFLINE_RUN_BUILD_CONFIG ${imageConfig} \
          --set OFFLINE_RUN_BUILDER_ID gondolin-${gondolin.version} \
          --run 'export OFFLINE_RUN_IMAGE_CACHE="''${OFFLINE_RUN_IMAGE_CACHE:-''${XDG_CACHE_HOME:-$HOME/.cache}/offline-run/images}"' \
          --prefix PATH : ${lib.makeBinPath [bash coreutils cpio e2fsprogs findutils gondolin lz4 qemu which]}
      done

      runHook postInstall
    '';

    passthru.tests.image-tools = runCommand "offline-run-image-tools" {} ''
      env -i HOME="$TMPDIR" TMPDIR="$TMPDIR" PATH=/nonexistent \
        NODE_OPTIONS="--import ${../../tests/gondolin-image-tools.mjs}" \
        ${finalAttrs.finalPackage}/bin/offline-run-prepare
      touch $out
    '';

    meta = {
      description = "Runs a command in a Gondolin micro-VM with no network";
      mainProgram = "offline-run";
      inherit (gondolin.meta) platforms;
    };
  })
