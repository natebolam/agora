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
  httpPort = 80;
  httpsPort = 443;

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

  caddy-config = writeText "Caddyfile" ''
    {$DNS_DOMAIN} {
      gzip
      root ${frontend}

      proxy /api http://{$API_HOST}:{$API_PORT} {
        transparent
      }

      rewrite /static/ {
        r (.*)
        to /{1}
      }

      rewrite {
        if {path} not_starts_with /api/
        if {path} not_starts_with /static/
        to /
      }
    }
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
      frontend
      caddy
      cacert
    ];

    config = {
      Entrypoint = [ "/bin/caddy" "-conf" caddy-config ];
      ExposedPorts = {
        "${toString httpPort}/tcp" = {};
        "${toString httpsPort}/tcp" = {};
      };
    };
  };
}
