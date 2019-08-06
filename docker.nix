{ pkgs ? import ./nix {} }: with pkgs;
let
  inherit (dockerTools)
    buildImage
    buildLayeredImage
    pullImage
    shadowSetup;

  inherit (builtins) toJSON;

  agora = import ./. { inherit pkgs; };
  backend = agora.agora-backend;
  backend-config = agora.agora-backend-config;
  frontend = agora.agora-frontend;
  nginxPort = 80;

  # Some system files necessary for nginx to function normally
  shadow-files = runCommand "shadow-files" {} ''
    mkdir -p $out/etc
    echo "nobody:x:65534:65534:nobody:/:/sbin/nologin" > $out/etc/passwd
    echo "nobody:!x:::::::" > $out/etc/shadow
    echo "nobody:x:65534:" > $out/etc/group
    echo "nobody:x::" > $out/etc/gshadow
  '';

  backend-config-template = writeText "backend-config-env.yaml.template" (toJSON {
    node_addr = "$NODE_HOST:$NODE_PORT";
    api.listen_addr = "*:$API_PORT";
    db.conn_string = "host=$POSTGRES_HOST dbname=$POSTGRES_DB user=$POSTGRES_USER password=$POSTGRES_PASSWORD";
    discourse.api_token = "$DISCOURSE_API_TOKEN";
  });

  backend-entry-point = writeScriptBin "entrypoint.sh" ''
    #!/bin/bash
    set -euxo pipefail

    /bin/envsubst '$NODE_HOST $NODE_PORT $API_PORT $POSTGRES_HOST $POSTGRES_DB $POSTGRES_USER $POSTGRES_PASSWORD $DISCOURSE_API_TOKEN' \
      < ${backend-config-template} >| /env-config.yaml

    exec /bin/agora -c /base-config.yaml -c /env-config.yaml "$@"
  '';

  nginx-config-template = writeText "nginx.conf.template" ''
    user nobody nobody;
    daemon off;
    error_log /dev/stdout info;
    pid /dev/null;
    events {}
    http {
        include       ${pkgs.nginxStable}/conf/mime.types;
        default_type  application/octet-stream;
        sendfile        on;
        keepalive_timeout  65;
        access_log /dev/stdout;

        server {
            listen ${toString nginxPort};
            root   ${frontend};
            location /api {
                proxy_pass http://''${API_HOST}:''${API_PORT};
                proxy_set_header HOST $host;
                proxy_set_header X-Forwarded-Proto $scheme;
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            }
            location / {
                try_files $uri $uri/ /index.html;
            }
            location ~* \.(?:jpg|jpeg|gif|png|ico|cur|gz|svg|svgz|mp4|ogg|ogv|webm|htc)$ {
                expires 1M;
                access_log off;
                add_header Cache-Control "public";
            }
            location ~* \.(?:css|js)$ {
                try_files $uri =404;
                expires 1y;
                access_log off;
                add_header Cache-Control "public";
            }
        }
    }
  '';

  frontend-entry-point = writeScriptBin "entrypoint.sh" ''
    #!/bin/bash
    set -euxo pipefail
    /bin/envsubst '$API_HOST $API_PORT' < ${nginx-config-template} >| /nginx.conf
    exec /bin/nginx -c /nginx.conf
  '';
in
{
  backend-image = buildLayeredImage {
    name = "registry.gitlab.com/tezosagora/agora/backend";
    tag = "latest";
    maxLayers = 120;

    contents = [
      backend
      backend-config
      backend-entry-point
      bash
      cacert
      gettext
      iana_etc
    ];

    config = {
      Entrypoint = "${backend-entry-point}/bin/entrypoint.sh";
    };
  };

  frontend-image = buildLayeredImage {
    name = "registry.gitlab.com/tezosagora/agora/frontend";
    tag = "latest";
    maxLayers = 120;

    contents = [
      bash
      frontend
      frontend-entry-point
      gettext # envsubst
      nginxStable
      shadow-files
    ];

    config = {
      Entrypoint = "${frontend-entry-point}/bin/entrypoint.sh";
      ExposedPorts = {
        "${toString nginxPort}/tcp" = {};
      };
    };
  };
}
