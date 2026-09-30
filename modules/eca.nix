{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.home.programs.wgn.emacs.eca;
  gondolin = cfg.sandbox.backend == "gondolin";
  backend =
    if gondolin
    then pkgs.eca-gondolin
    else pkgs.eca-bwrap;

  # Shared by activation and sessions, which must agree on it.
  imageCache = "${config.xdg.cacheHome}/gondolin/eca-images";

  # Carries the configured arguments, so that a TRAMP session from another
  # machine, which finds this on PATH, runs with them as well.
  launcher = pkgs.writeShellScriptBin "eca-sandbox" ''
    ${lib.optionalString gondolin "export ECA_GONDOLIN_IMAGE_CACHE=${lib.escapeShellArg imageCache}"}
    exec ${lib.getExe backend} ${lib.escapeShellArgs cfg.sandbox.args} "$@"
  '';

  managedImage = cfg.sandbox.enable && gondolin && !(builtins.elem "--image" cfg.sandbox.args);
in {
  options.home.programs.wgn.emacs.eca = {
    enable = lib.mkEnableOption "Install the pinned ECA server for Emacs";

    sandbox = {
      enable = lib.mkEnableOption "Run local ECA sessions in a sandbox";

      backend = lib.mkOption {
        type = lib.types.enum ["gondolin" "bubblewrap"];
        default = "gondolin";
        description = ''
          Gondolin uses a micro-VM and requires hardware virtualization.
          Bubblewrap uses Linux namespaces and works without it.
        '';
      };

      args = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = ''
          Arguments for the sandbox, baked into the `eca-sandbox` command
          that local sessions and TRAMP sessions from other machines run.
        '';
      };
    };
  };

  config = lib.mkIf (config.home.programs.wgn.emacs.enable && cfg.enable) {
    home.packages =
      [pkgs.eca]
      ++ lib.optionals cfg.sandbox.enable [backend launcher]
      ++ lib.optionals (cfg.sandbox.enable && gondolin) [pkgs.gondolin];

    # A failure must not abort the switch: the build needs the network, and
    # the wrapper builds a missing image itself on first start.
    home.activation.ecaGondolinImage = lib.mkIf managedImage (
      lib.hm.dag.entryAfter ["writeBoundary"] ''
        run ${pkgs.coreutils}/bin/env \
          ECA_GONDOLIN_IMAGE_CACHE=${lib.escapeShellArg imageCache} \
          ${backend}/bin/eca-gondolin-prepare \
          || warnEcho "The ECA guest image was not prepared; the first sandboxed session will build it."
      ''
    );

    # The profile path rather than the store path, so that a running Emacs
    # picks up a new generation's launcher without reloading this file.
    home.file.".emacs.d/etc/eca.el".text = ''
      ;;; eca.el --- Machine ECA launch settings -*- lexical-binding: t; -*-
      (setq eca-custom-command ${
        if cfg.sandbox.enable
        then "'(${builtins.toJSON "${config.home.profileDirectory}/bin/eca-sandbox"})"
        else "nil"
      })
    '';
  };
}
