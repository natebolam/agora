{pkgs, lib, config, ...}:

let
  inherit (builtins) toString typeOf;
  inherit (lib) collect;
  inherit (pkgs) writeText;

  agora = import ./.. { inherit pkgs; };

  ports = {
    node = {
      net = 9732;
      rpc = 8732;
    };
    backend = {
      api = 8190;
    };
    frontend = {
      http = 80;
      https = 443;
    };
  };

  backend-config = writeText "config.yml" ''
    listen_addr: "*:${toString ports.backend.api}"
    node_addr: "node:${toString ports.node.rpc}"
  '';
in
  {
    networking.firewall.allowedTCPPorts = collect (a: (typeOf a) == "int") ports;
    users.users.root.openssh.authorizedKeys.keys = [
      # Yorick
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFQm2OJ8PlnDHfI7FV3hddXP0t2jgKAiCnnuWIc+LK4dnyGmlC/ihIe9KhSENZEnzVAXnYAMOoOvpkVa5p0Itf1n0anCK3k2vDq0Jz9nY3ZXmkSHE09QGCpSG8kU6j+zWJPo2jWYNtxYMRmmHAuzzOdlPY9Q199PEvHVaqzpSVhIdhqhEcmap8oqHW6KbJu+17nLGGQB5XiTB1SlTxbg62copA9KMcvQzNGIooKs5QyrU/B0g05EfbogH7xOLbwYAK676DTUBEcKpEUYFMMv+DBcU4cH2EI6UTLxI5ohrS1pxk20zu5nTRMlQRUETpWN4EbEPfOzF8FW1YOwdttfCas8D6Y6t9gA4o8GpylBG9AElVw7VyOFeBR+AtchormH+wH6nZEvzs6wg2d84I8xo5qYGUJIQS7OYxypjlY01IFCCa/7rjzXGDmdWAP/UEu85ys9FSryn9Ey5DXDQOqhMHguOwQDUyaArWyRCCBzKbx6cPZw2D9bLfDxbnaC2/5dVyxHJXoWmwneX2E/UT5QwtG1nyLShIZhYgO9lfDpO61Mz9Jjap3sj6mJPxHZc5SGye0j47xV6kX4vbSgfoSHHnasaI3fR2ZBS7tnfq1ebxXqFFSPHK6uHjLgDHtkMisvjOVZnybuXB296pjv6K1o2G3qlUnImOqfxZxvuy4Xl/mw=="
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDo1N5E6qkb3McJOvv0PqI7E8iYLAcjil5RWc+zeTtN/"

      # Chris
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOySJ7SAmh+3CBK4fagwkY9PsF6xF+9msMRoQN6JalpauQANALVsVDjC3heFH6Lc/tjLrhQ46oVO3xMFGVKxNe81gaWhvWPxytfH5V8FP52GWEo5HwwMd+VoEyJIYYbj10jwkuzutr9fF0qlp0nhR1IaTKnxJFxV8tUkpiC3a9Qf4yrNy7Ft6DMwyiZSh/mEx+S4LuMqayb93do7+ddlSAyb70NQrLv7H2IRA+qkAzPhZe80o3FqKRvXayH5GSSuYLFfEPFgy0guKAA7P2ICjddLJ+l8BAdTlF8ADY1Z97DvCAgG6CT4cnRzv+cSM+Uvd+ZTxBY6Z+U27kO2LB7UBhVLzrWHSRbv5KWaruFzhOD3E64y3+7XzUg0DpoeS2QVahYc3iF4FvpVfLLPX3F4aev/83Z05G6nEn8lDb1XPAV0KRwo0gB4cCknC6MurnIzxgAeElin9DL5KgVMgVr5jIgBhx01Z9VEVNs5UcMDrA2mXHenY0uAnNk+iWeKZdzxxet50gQuebJ5Q3jHCADS6WZZsBdjxTDiLNvBVo1OiaZ4/tubzVZdrmCkPZDyPUO04Gz7rqXdVFiqzCJgVbcv2gX1qe8UthlRmdblX+l2fY4gvAOGNchVG1cMmvuA5i27td0PqDh6I7kQPvqKQ3QkCI012hwW9ca5S3HGtQDgqSZQ=="

      # gitlab-runner on Jupiter for CD
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILbXt6CSEnOXKq0ivOwCm9XFbbrxSZj55da8v5PZpfB9"
    ];

    virtualisation.docker = {
      enable = true;
      logLevel = "warn";
      storageDriver = "overlay2";

      volumes = [ "client_data" "node_data" ];
      networks.tezos = {};
    };

    # The primary Tezos node, taken directly from their generated docker-compose.yml
    docker-containers = let
      commonOptions = {
        extraDockerOptions = [ "--network=tezos" ];
      };

      in {
      node = {
        image = "tezos/tezos:mainnet";
        cmd = [
          "tezos-node"
          "--net-addr=:${toString ports.node.net}"
          "--history-mode=archive"
        ];

        ports = with ports.node; [
          "${toString net}:${toString net}"
          "${toString rpc}:${toString rpc}"
        ];

        volumes = [
          "node_data:/var/run/tezos/node"
          "client_data:/var/run/tezos/client"
        ];
      } // commonOptions;

      frontend = {
        image = "registry.gitlab.com/tezosagora/agora/frontend";
        ports = with ports.frontend; [
          "${toString http}:${toString http}"
          "${toString https}:${toString https}"
        ];
      } // commonOptions;

      backend = {
        image = "registry.gitlab.com/tezosagora/agora/backend";
        volumes = [ "${backend-config}:/config.yml" ];
        ports = with ports.backend; [
          "${toString api}:${toString api}"
        ];
      } // commonOptions;
    };
  }
