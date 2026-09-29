{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.home.programs.wgn.emacs.eca;
  sandbox =
    if cfg.sandbox.backend == "gondolin"
    then pkgs.eca-gondolin
    else pkgs.eca-bwrap;
  command = ["${sandbox}/bin/eca-sandbox"] ++ cfg.sandbox.args;
  commandElisp = "(" + lib.concatMapStringsSep " " builtins.toJSON command + ")";
in {
  options.home.programs.wgn.emacs.eca = {
    enable = lib.mkEnableOption "Install the pinned ECA server for Emacs";

    sandbox = {
      enable = lib.mkEnableOption "Run local ECA sessions in a sandbox";

      backend = lib.mkOption {
        type = lib.types.enum ["gondolin" "bubblewrap"];
        default = "gondolin";
        description = ''
          Gondolin uses a micro-VM and enforces egress at the network layer.
          Bubblewrap works without hardware virtualization, but its proxy
          egress restrictions can be bypassed by the guest process.
        '';
      };

      args = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = ["--image" "eca:latest"];
        description = "Arguments for the sandbox used by every local ECA workspace.";
      };
    };
  };

  config = lib.mkIf (config.home.programs.wgn.emacs.enable && cfg.enable) {
    home.packages =
      [pkgs.eca]
      ++ lib.optionals cfg.sandbox.enable [sandbox]
      ++ lib.optionals (cfg.sandbox.enable && cfg.sandbox.backend == "gondolin") [pkgs.gondolin];

    home.file.".emacs.d/etc/eca.el".text = ''
      ;;; eca.el --- Machine ECA launch settings -*- lexical-binding: t; -*-
      (setq eca-custom-command ${
        if cfg.sandbox.enable
        then "'" + commandElisp
        else "nil"
      }
            eca-send-process-id nil)
    '';
  };
}
