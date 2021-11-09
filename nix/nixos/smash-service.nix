{ config, lib, pkgs, ... }:

let
  cfg = config.services.smash;
  inherit (cfg.dbSyncPkgs) cardano-smash-server iohkNix;
  smashConfig = cfg.explorerConfig // {
    inherit (cfg.nodeConfig) ByronGenesisFile ShelleyGenesisFile ByronGenesisHash ShelleyGenesisHash Protocol RequiresNetworkMagic;
  };
  configFile = __toFile "config.json" (__toJSON (smashConfig // cfg.logConfig));
in {

  options = {
    services.smash = {
      enable = lib.mkEnableOption "enable the smash server";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      dbSyncPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. {};
        defaultText = "db-sync pkgs";
        description = ''
          The db-sync packages and library that should be used.
        '';
        internal = true;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cardano-smash-server;
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 3100;
        description = "http serving port";
      };
      explorerConfig = lib.mkOption {
        type = lib.types.attrs;
        default = cfg.environment.explorerConfig;
      };
      nodeConfig = lib.mkOption {
        type = lib.types.attrs;
        default = cfg.environment.nodeConfig;
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = iohkNix.cardanoLib.environments.${cfg.environmentName};
      };
      logConfig = lib.mkOption {
        type = lib.types.attrs;
        default = iohkNix.cardanoLib.defaultExplorerLogConfig;
      };
      environmentName = lib.mkOption {
        type = lib.types.str;
        description = "environment name";
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "smash";
        description = "the user to run as";
      };
      postgres = {
        generatePGPASS = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "generate pgpass";
        };
        pgpass = lib.mkOption {
          type = lib.types.path;
          internal = true;
          default = config.services.cardano-db-sync.pgpass;
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.smash.script = let
    in pkgs.writeShellScript "smash" ''
      set -euo pipefail

      RUNTIME_DIRECTORY=''${RUNTIME_DIRECTORY:-$(pwd)}

      ${lib.optionalString cfg.postgres.generatePGPASS ''
      cp ${cfg.postgres.pgpass} /$RUNTIME_DIRECTORY/pgpass
      chmod 0600 $RUNTIME_DIRECTORY/pgpass
      export SMASHPGPASSFILE=/$RUNTIME_DIRECTORY/pgpass
      ''}

      exec ${cfg.package}/bin/cardano-smash-server \
        --port ${toString cfg.port} \
        --config ${configFile} \
    '';
    environment.systemPackages = [ cfg.package config.services.postgresql.package ];
    systemd.services.smash = {
      path = [ cfg.package pkgs.netcat pkgs.postgresql ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString config.services.cardano-db-sync.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      serviceConfig = {
        ExecStart = config.services.smash.script;
        DynamicUser = true;
        RuntimeDirectory = "smash";
        StateDirectory = "smash";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "cardano-db-sync.service" ];
      requires = [ "postgresql.service" ];
    };
  };
}
