let
  agora = import ./.;
  ports = {
    node = {
      net = 9732;
      rpc = 8732;
    };
  };
  inherit (builtins) toString;
in
  {
    agora = { lib, pkgs, config, ... }:
    {
      # This doesn't seem to be a default top-level import
      imports = [
        <nixpkgs/nixos/modules/virtualisation/docker-containers.nix>
      ];

      # Can probably do some flatten/map magic on the ports attrset instead of this
      networking.firewall.allowedTCPPorts = [
        ports.node.net
        ports.node.rpc
      ];

      virtualisation.docker = {
        enable = true;
        storageDriver = "overlay2";
      };

      services.openssh.enable = true;

      # Create named docker volumes before we try to start the Node container
      systemd.services.docker-volumes = {
        after = [ "docker.service" "docker.socket" ];
        requires = [ "docker.service" "docker.socket" ];
        serviceConfig.Type = "oneshot";

        script = let
          volumes = [ "node_data" "client_data" ];
          mkVolume = volume: ''
            (${pkgs.docker}/bin/docker volume ls -q | grep -q ${volume}) || ${pkgs.docker}/bin/docker volume create ${volume}
          '';
        in lib.concatStrings (map mkVolume volumes);
      };

      systemd.services.docker-node.after = [ "docker-volumes.service" ];
      systemd.services.docker-node.requires = [ "docker-volumes.service" ];

      # The primary Tezos node, taken directly from their generated docker-compose.yml
      docker-containers = {
        node = {
          image = "tezos/tezos:mainnet";
          cmd = [
            "tezos-node" "--net-addr=:${toString ports.node.net}"
          ];

          ports = [
            "${toString ports.node.net}:${toString ports.node.net}"
            "${toString ports.node.rpc}:${toString ports.node.rpc}"
          ];

          volumes = [
            "node_data:/var/run/tezos/node"
            "client_data:/var/run/tezos/client"
          ];
        };
      };
    };
  }
