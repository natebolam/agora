{ pkgs ? import ./nix {} }: with pkgs;
let
  inherit (dockerTools)
    buildImage
    buildLayeredImage
    pullImage
    shadowSetup;

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

  nginxConfig = writeText "nginx.conf" ''
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
in
{
  backend-image = buildLayeredImage {
    name = "registry.gitlab.com/tezosagora/agora/backend";
    tag = "latest";
    maxLayers = 120;

    contents = [
      backend-config
      backend
    ];

    config = {
      Entrypoint = "/bin/agora";
      Cmd = [ "-c" "/base-config.yaml" ];
    };
  };

  frontend-image = buildLayeredImage {
    name = "registry.gitlab.com/tezosagora/agora/frontend";
    tag = "latest";
    maxLayers = 120;

    contents = [
      shadow-files
      nginxStable
      frontend
    ];

    config = {
      Entrypoint = "nginx";
      Cmd = [ "-c" nginxConfig ];
      ExposedPorts = {
        "${toString nginxPort}/tcp" = {};
      };
    };
  };
}
