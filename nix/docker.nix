{ pkgs, agora }:

let
  inherit (pkgs) runCommand writeShellScript writeText;
  inherit (pkgs.dockerTools) buildLayeredImage;
in
{
  backend-image =
    let
      # Some Docker-reasonable defaults and two required options with
      # no reasonable defaults: DISCOURSE_API_KEY, CONTRACT_ADDRESS.
      # POSTGRES_PASSWORD is a standard Docker thing and should be the
      # same as in the postgres container.
      agoraConf = writeText "agora.yaml" ''
        node_addr: "tezos-node:8732"
        db:
          conn_string: "host=postgres dbname=agora password=$POSTGRES_PASSWORD"
        discourse:
          host: "http://discourse"
          api_key: $DISCOURSE_API_KEY
        contract:
          address: $CONTRACT_ADDRESS
          contract_block_level: 0
      '';
      entrypoint = writeShellScript "entrypoint.sh" ''
        #!${pkgs.stdenv.shell}

        ${pkgs.gettext}/bin/envsubst \
          '$POSTGRES_PASSWORD $DISCOURSE_API_KEY $CONTRACT_ADDRESS' \
          < ${agoraConf} >| /agora.yaml

        exec ${agora.agora-backend}/bin/agora -c /agora.yaml "$@"
      '';
      in buildLayeredImage {
        name = "registry.gitlab.com/tezosagora/agora/backend";
        tag = "latest";
        config = {
          Entrypoint = ["${entrypoint}"];
          ExposedPorts = { "8190/tcp" = {}; };
        };
        contents = [
          pkgs.cacert  # TLS for Discourse
          pkgs.iana_etc  # /etc/protocols
        ];
      };

  frontend-image =
    let
      root = agora.agora-frontend;
      nginxConf = writeText "nginx.conf" ''
        user nobody nobody;
        daemon off;
        error_log /dev/stdout info;
        pid /dev/null;
        events {}
        http {
          access_log /dev/stdout;
          include ${pkgs.nginx}/conf/mime.types;
          server {
            listen 80;

            location /static/ {
              alias ${root}/;
            }
            location / {
              add_header Cache-Control no-cache;
              root ${root};
              try_files /index.html =404;
            }
          }
        }
      '';
      # Some system files necessary for nginx to function normally
      shadow-files = runCommand "shadow-files" {} ''
        mkdir -p $out/etc
        echo "nobody:x:65534:65534:nobody:/:/sbin/nologin" > $out/etc/passwd
        echo "nobody:!x:::::::" > $out/etc/shadow
        echo "nobody:x:65534:" > $out/etc/group
        echo "nobody:x::" > $out/etc/gshadow
      '';
    in buildLayeredImage {
      name = "registry.gitlab.com/tezosagora/agora/frontend";
      tag = "latest";
      config = {
        Entrypoint = ["${pkgs.nginx}/bin/nginx" "-c" "${nginxConf}"];
        ExposedPorts = { "80/tcp" = {}; };
      };
      contents = [ shadow-files ];
    };
}
