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
    prometheus = {
      node = 9100;
    };
  };

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

      # Kirill
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJWXeyi74spCJ2sPEOyTdKMstBrjHnW3/ywxUqq9TjNhbce1qYlmTf0IAMNVUvk+j/1fkso/Gw5FRrwZUqNZFxa5/lTZ8N+TNYcbMTZNy9XrF8gRg+wk+LhZbpqtPv6isjA6cTm/Qq+gNsF/1CehLVal1VZsFkfBkPWnqO0i+sFem3OH3+Hd+g5UdfFJJ5bLPxYxvzk9eF0ynYpXbOrMUZej3ftHQOJXaEtEQpxjgGkSLyrXz1mNf9EDWdlKiSIBx5TonhKuIuLD8ve9ik5VdDu5jznmCiMDM4KbG0ZSzYUwgCIm4W1BLB0avNok6NYlpLZlHIZXdNoDBt59SNjP9B"

      # flyingleafe
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCZjBK3NMsPqtO6oeZU3UoDxhJjS2fqNuWkuD0CdE/wyXIHdeJ3aANgECXvl7y3hWizY48GVR7Ct2gjV9av+U+tHVMFX4ZaGl0MHl796H+4i6tkizpRKw4kuLxJ5URma6M7IBYoV/qHWgMq0FXw0/o57gRs9z4dbK1JitCdEBfLmhZiotQTb6UseQZ7Hbf8clVAtV0wZW0yjBueOtm9JpjolcZ07OoljKMZgEQ6MrHap+lPrI0X0Cxa0V5VLofl5iMKb2wSMCQ0Jd4EDi1vpwPHkHOtcbujtZXyBlUD2YXQRYgq8j2OPxr4RKzjpMxA8CbQhdnjpKWNcWV90eKg4WcxQFAtMQlJmMGmL31iU4fNWYMJXRI0LUok7QXFyPtnfYAXrmpe776Kqpa3STYRuWM3xznQIniEUU8CS+OePix4NxRPkSRoXuBcRA++i0ZV1T8jr8jn/6XzXFzCTN99RTkeXeomJWSx5Q/3EOrwQtOz4Pf0BBmtOqCKGHO/wACxJD+P1e95f7mNlmmaPcF4qUU4frUDDQPrOJiVjvxDzOCCY18Gfwt5Vwcst+0D3PyY1tW7Ai1IFTJ9TZjua40S8thg+fLORB4mpJsCrKP1YfHKRN7C7kKFq0QrJKLBcUxI69LQREFPtH0u0p1u0tNnWMWjQAjGjsuMiYPJyDmomcXjxw=="
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsDciZLdWUQ0elPzeJukUZLPAp/uSepUTNPo1vsK5ow4rf3XMLbHwZmIaw0M9PfSHemHHt9Fp1fzch3spm3zltOGO6bDE8Oy1UyOSV+1mGcvj2z4MuJby7tub69k6DwnlSaSDl6LgREXIAtdRbHI3+mqZWXmQp2kCPtq9Pkzv71+jZVQej/eYsO6tJF20jyP/ul9XDGmRSVizI+eaetL8CXg7tlKXNdn9/aONbzMGcmriDKdCKvxmaAYuyGBipURdmuSMwsQobfoNkvAemCpqnIdrVPjQI+s03GRmC/gecCSjWtysKu8BzlxXYJS7yNOboeZBQO2KXfDwtt6OliJuZ"

      # pva701
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCU3K3xT07W+TvaY/UNJnQhRkFA2r6LIOCx+KYP7fXkIV7IEi2H7wPCZVS09zgj2bQBzViUk4c1xj8MGCoO11duwRPqp5HSf/2dSyEPfcwseU0oau2hXot3i8XjzcAEdOgMjhctMMgtl56f4dt7OPu1qmMGL+KcvZIOZAmjE+Obf5r3Kf6IIV6l1lEZUqjihoSzAGMmC/nTIyO2U1ThqWqCQgZT+hWXenlpLhIZkji5pDRmUwdLszWJ9P+/TatH6xj3KERg3Il1L4Msrc4+Q/BUocJZnN5QyIJUTREdbNJTT/tva8gd6vxbEr0VCWbi+qdyZwRUgWK3/R7xA8pCBvtxIDj4za0iMn+cdMHuF84lsyyfVSudxM9ZRQWA6XYN2VO3t+ngxzC9QActPko+bfcogLoMXFvVs8g3T69VgJdvIT89e9pmlV7M6fee//u1kYKnJ0Z7s8hDm4ebAqmlQNbuF1uQZ8xM3Mui5q6UBGJUoIb7j/fmsqP6KmdFwiOmtvjhkzm98i08QzHe9xKlGG5T48909/oBPV5SGYf9D3SNq1bygnVBN1Nhpf/ZXOq4IqhnNdei0VxONJpaE+RssDSWeyyMN6mqvZKgzU/tPMK6TKzO3vi66OTTDZFxfXp212xnSRGpjKc7kYfeb/TulCY8nv/r11YAW3B+j0QrgFgFjQ=="

      # Sandeep.Chandrika
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSTZWP9OtsxQpA8nDr1ubHlsmabN0vYEJo8EM/QK5EBZnqwyJ9ttcXx9TRKoCXGj1h+JdumDo07m6AxpGmQil2mEtY8SkVmxk+EXkIvHwl3y2efY8Punu8qqYQ3QTP3fxAKkR87NfHhW+20wEo0LNSmqtFrLodaPZ65rW7UPaJEHMUK1jYHgOqSZ5T3xdS+dRXHEHO2FzS3q0ehlO3G01eBcB7HmJ1odoo+E97Q1Y0m2AEncJ1eyERaM6nPqRF8c7ZYlvxcea97u5NElcfYYahUAzT6zJNeiBy25LKvIM+YTmtKGap/gbFOgmcFNwazxN79tDvf71cLBTinkvbudNr"

      # gitlab-runner on Jupiter for CD
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILbXt6CSEnOXKq0ivOwCm9XFbbrxSZj55da8v5PZpfB9"
    ];

    services.prometheus.exporters.node = {
      enable = true;
      enabledCollectors = [ "systemd" ];
      disabledCollectors = [ "timex" ];
    };

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

          # Per-env config
          "--env-file=/root/agora.env"

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
        containerDependencies = [ "backend" ];

        volumes = [
          "/root/caddy:/.caddy"
        ];

        ports = with ports.frontend; [
          "${toString http}:${toString http}"
          "${toString https}:${toString https}"
        ];

        environment = {
          API_HOST = "backend";
          API_PORT = toString ports.backend.api;
        };
      } // commonOptions;

      backend = {
        image = "registry.gitlab.com/tezosagora/agora/backend:latest";
        imageFile = agora.backend-image;
        containerDependencies = [ "postgres" "node" ];

        volumes = [
          "/root/secret-backend.yml:/secret-config.yaml"
        ];

        cmd = [
          "-c" "/secret-config.yaml"
        ];

        environment = {
          POSTGRES_HOST = "postgres";
          POSTGRES_DB = "agora";
          POSTGRES_USER = "postgres";
          POSTGRES_PASSWORD = "12345";
          API_PORT = toString ports.backend.api;
          NODE_HOST = "node";
          NODE_PORT = toString ports.node.rpc;
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
