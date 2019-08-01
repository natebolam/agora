{pkgs, lib, config, ...}:

let
  inherit (builtins) toString typeOf toJSON;
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

  backend-config = writeText "nix-config.yaml" (toJSON {
    node_addr = "node:${toString ports.node.rpc}";
    api.listen_addr = "*:${toString ports.backend.api}";
    db.conn_string = "host=postgres dbname=agora user=postgres password=12345";
  });

  pginit = writeText "init.sql" ''
    CREATE DATABASE agora;
  '';
in
  {
    nixpkgs.overlays = import ../nix/overlays.nix {};
    networking.firewall.allowedTCPPorts = collect (a: (typeOf a) == "int") ports;
    users.users.root.openssh.authorizedKeys.keys = [
      # Yorick
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFQm2OJ8PlnDHfI7FV3hddXP0t2jgKAiCnnuWIc+LK4dnyGmlC/ihIe9KhSENZEnzVAXnYAMOoOvpkVa5p0Itf1n0anCK3k2vDq0Jz9nY3ZXmkSHE09QGCpSG8kU6j+zWJPo2jWYNtxYMRmmHAuzzOdlPY9Q199PEvHVaqzpSVhIdhqhEcmap8oqHW6KbJu+17nLGGQB5XiTB1SlTxbg62copA9KMcvQzNGIooKs5QyrU/B0g05EfbogH7xOLbwYAK676DTUBEcKpEUYFMMv+DBcU4cH2EI6UTLxI5ohrS1pxk20zu5nTRMlQRUETpWN4EbEPfOzF8FW1YOwdttfCas8D6Y6t9gA4o8GpylBG9AElVw7VyOFeBR+AtchormH+wH6nZEvzs6wg2d84I8xo5qYGUJIQS7OYxypjlY01IFCCa/7rjzXGDmdWAP/UEu85ys9FSryn9Ey5DXDQOqhMHguOwQDUyaArWyRCCBzKbx6cPZw2D9bLfDxbnaC2/5dVyxHJXoWmwneX2E/UT5QwtG1nyLShIZhYgO9lfDpO61Mz9Jjap3sj6mJPxHZc5SGye0j47xV6kX4vbSgfoSHHnasaI3fR2ZBS7tnfq1ebxXqFFSPHK6uHjLgDHtkMisvjOVZnybuXB296pjv6K1o2G3qlUnImOqfxZxvuy4Xl/mw=="
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDo1N5E6qkb3McJOvv0PqI7E8iYLAcjil5RWc+zeTtN/"

      # Chris
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOySJ7SAmh+3CBK4fagwkY9PsF6xF+9msMRoQN6JalpauQANALVsVDjC3heFH6Lc/tjLrhQ46oVO3xMFGVKxNe81gaWhvWPxytfH5V8FP52GWEo5HwwMd+VoEyJIYYbj10jwkuzutr9fF0qlp0nhR1IaTKnxJFxV8tUkpiC3a9Qf4yrNy7Ft6DMwyiZSh/mEx+S4LuMqayb93do7+ddlSAyb70NQrLv7H2IRA+qkAzPhZe80o3FqKRvXayH5GSSuYLFfEPFgy0guKAA7P2ICjddLJ+l8BAdTlF8ADY1Z97DvCAgG6CT4cnRzv+cSM+Uvd+ZTxBY6Z+U27kO2LB7UBhVLzrWHSRbv5KWaruFzhOD3E64y3+7XzUg0DpoeS2QVahYc3iF4FvpVfLLPX3F4aev/83Z05G6nEn8lDb1XPAV0KRwo0gB4cCknC6MurnIzxgAeElin9DL5KgVMgVr5jIgBhx01Z9VEVNs5UcMDrA2mXHenY0uAnNk+iWeKZdzxxet50gQuebJ5Q3jHCADS6WZZsBdjxTDiLNvBVo1OiaZ4/tubzVZdrmCkPZDyPUO04Gz7rqXdVFiqzCJgVbcv2gX1qe8UthlRmdblX+l2fY4gvAOGNchVG1cMmvuA5i27td0PqDh6I7kQPvqKQ3QkCI012hwW9ca5S3HGtQDgqSZQ=="

      # flyingleafe
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCZjBK3NMsPqtO6oeZU3UoDxhJjS2fqNuWkuD0CdE/wyXIHdeJ3aANgECXvl7y3hWizY48GVR7Ct2gjV9av+U+tHVMFX4ZaGl0MHl796H+4i6tkizpRKw4kuLxJ5URma6M7IBYoV/qHWgMq0FXw0/o57gRs9z4dbK1JitCdEBfLmhZiotQTb6UseQZ7Hbf8clVAtV0wZW0yjBueOtm9JpjolcZ07OoljKMZgEQ6MrHap+lPrI0X0Cxa0V5VLofl5iMKb2wSMCQ0Jd4EDi1vpwPHkHOtcbujtZXyBlUD2YXQRYgq8j2OPxr4RKzjpMxA8CbQhdnjpKWNcWV90eKg4WcxQFAtMQlJmMGmL31iU4fNWYMJXRI0LUok7QXFyPtnfYAXrmpe776Kqpa3STYRuWM3xznQIniEUU8CS+OePix4NxRPkSRoXuBcRA++i0ZV1T8jr8jn/6XzXFzCTN99RTkeXeomJWSx5Q/3EOrwQtOz4Pf0BBmtOqCKGHO/wACxJD+P1e95f7mNlmmaPcF4qUU4frUDDQPrOJiVjvxDzOCCY18Gfwt5Vwcst+0D3PyY1tW7Ai1IFTJ9TZjua40S8thg+fLORB4mpJsCrKP1YfHKRN7C7kKFq0QrJKLBcUxI69LQREFPtH0u0p1u0tNnWMWjQAjGjsuMiYPJyDmomcXjxw=="

      # pva701
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCU3K3xT07W+TvaY/UNJnQhRkFA2r6LIOCx+KYP7fXkIV7IEi2H7wPCZVS09zgj2bQBzViUk4c1xj8MGCoO11duwRPqp5HSf/2dSyEPfcwseU0oau2hXot3i8XjzcAEdOgMjhctMMgtl56f4dt7OPu1qmMGL+KcvZIOZAmjE+Obf5r3Kf6IIV6l1lEZUqjihoSzAGMmC/nTIyO2U1ThqWqCQgZT+hWXenlpLhIZkji5pDRmUwdLszWJ9P+/TatH6xj3KERg3Il1L4Msrc4+Q/BUocJZnN5QyIJUTREdbNJTT/tva8gd6vxbEr0VCWbi+qdyZwRUgWK3/R7xA8pCBvtxIDj4za0iMn+cdMHuF84lsyyfVSudxM9ZRQWA6XYN2VO3t+ngxzC9QActPko+bfcogLoMXFvVs8g3T69VgJdvIT89e9pmlV7M6fee//u1kYKnJ0Z7s8hDm4ebAqmlQNbuF1uQZ8xM3Mui5q6UBGJUoIb7j/fmsqP6KmdFwiOmtvjhkzm98i08QzHe9xKlGG5T48909/oBPV5SGYf9D3SNq1bygnVBN1Nhpf/ZXOq4IqhnNdei0VxONJpaE+RssDSWeyyMN6mqvZKgzU/tPMK6TKzO3vi66OTTDZFxfXp212xnSRGpjKc7kYfeb/TulCY8nv/r11YAW3B+j0QrgFgFjQ=="

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
        extraDockerOptions = [
          # Put all nodes on the same network
          "--network=tezos"

          # Primarily for PostgreSQL during initdb
          "--shm-size=256MB" ];
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
        image = "registry.gitlab.com/tezosagora/agora/frontend:latest";
        imageFile = agora.frontend-image;

        ports = with ports.frontend; [
          "${toString http}:${toString http}"
          "${toString https}:${toString https}"
        ];
      } // commonOptions;

      backend = {
        image = "registry.gitlab.com/tezosagora/agora/backend:latest";
        imageFile = agora.backend-image;
        volumes = [
          "${backend-config}:/nix-config.yaml"
          "/root/secret-backend.yml:/secret-config.yaml"
        ];
        containerDependencies = [ "postgres" ];

        cmd = [
          "-c" "/base-config.yaml"
          "-c" "/nix-config.yaml"
          "-c" "/secret-config.yaml"
        ];

        ports = with ports.backend; [
          "${toString api}:${toString api}"
        ];

        environment = {
          POSTGRES_HOST = "postgres";
        };
      } // commonOptions;

      postgres = let
        datadir = "/var/lib/postgresql/data/pgdata";
      in {
        image = "postgres";
        volumes = [
          "pgdata:${datadir}"
          "${pginit}:/docker-entrypoint-initdb.d/init.sql"
        ];

        environment = {
          PGDATA = datadir;
          POSTGRES_PASSWORD = "12345";
          POSTGRES_USER = "postgres";
        };
      } // commonOptions;
    };
  }
