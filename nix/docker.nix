{ pkgs, agora }:

let
  inherit (builtins) toJSON;

  inherit (pkgs) runCommand writeScriptBin writeText;

  inherit (pkgs.dockerTools)
    buildImage
    buildLayeredImage
    pullImage
    shadowSetup;

  backend = agora.agora-backend;
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

  backend-entry-point = writeScriptBin "entrypoint.sh" ''
    #!/bin/bash
    set -euxo pipefail

    exec /bin/agora "$@"
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
      backend-entry-point
      pkgs.bash
      pkgs.cacert
      pkgs.gettext
      pkgs.iana_etc
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
      pkgs.caddy
      pkgs.cacert
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
