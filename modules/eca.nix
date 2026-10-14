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

  # Every session gets these, a review-loop session too: the sandbox itself
  # withholds the plans mount from that one.
  planArgs =
    lib.optionals (cfg.sandbox.plansDir != null) ["--plans" cfg.sandbox.plansDir]
    ++ lib.optionals (cfg.sandbox.guardConfig != null) ["--guard-config" cfg.sandbox.guardConfig];

  # Carries the configured arguments, so that a TRAMP session from another
  # machine, which finds this on PATH, runs with them as well.
  launcher = pkgs.writeShellScriptBin "eca-sandbox" ''
    ${lib.optionalString gondolin "export ECA_GONDOLIN_IMAGE_CACHE=${lib.escapeShellArg imageCache}"}
    ${lib.concatStrings (lib.mapAttrsToList (name: file: ''
        ${name}="$(cat ${lib.escapeShellArg file})"
        export ${name}
      '')
      cfg.sandbox.secretFiles)}
    exec ${lib.getExe backend} ${lib.escapeShellArgs (planArgs ++ cfg.sandbox.args)} "$@"
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

      secretFiles = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {};
        example = {MCP_PROXY_TOKEN = "/home/me/.local/state/mcp-proxy/token";};
        description = ''
          Environment variables the launcher reads from files before starting
          the sandbox, for `--secret NAME=HOST` in {option}`args` to take
          their values from. The files are read on this machine; the guest
          sees only Gondolin's placeholder.
        '';
      };

      plansDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "${config.home.homeDirectory}/git/notes/agents";
        defaultText = lib.literalExpression ''"''${config.home.homeDirectory}/git/notes/agents"'';
        description = ''
          Where agents keep their plans: the `plans.dir` of `agent-plan`'s
          config. Ordinary sessions get it read-write at this path; the bot
          review loop gets no mount and logs through `sandbox-relay`. Set it
          to the same value as the plans directory in the agent-tools
          config, or null to mount nothing. A directory that does not exist
          is skipped with a notice.
        '';
      };

      guardConfig = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "${config.xdg.configHome}/agent-tools/config.json";
        defaultText = lib.literalExpression ''"''${config.xdg.configHome}/agent-tools/config.json"'';
        description = ''
          The agent-tools config, which `agent-plan` and the guard read.
          Every session sees it read-only at `~/.config/agent-tools/config.json`.
          Null mounts nothing.
        '';
      };

      prLoop = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Directory holding a `pr-loop` client for the sandbox. When set,
          `M-x wgn/eca-pr-loop` starts a local session with `--pr-loop` and
          this directory, for the bot review loop; other sessions run without
          it. `sandbox-relay` and `pr-loop` must also be on PATH outside the
          sandbox.
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
      (setq wgn/eca-pr-loop-client ${
        if cfg.sandbox.enable && cfg.sandbox.prLoop != null
        then builtins.toJSON cfg.sandbox.prLoop
        else "nil"
      })
    '';
  };
}
