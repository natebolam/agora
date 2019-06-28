{ pkgs ? import ./pkgs.nix }: with pkgs;
let
  inherit (dockerTools)
    buildImage
    buildLayeredImage
    pullImage
    shadowSetup;

  agora = import ./. { inherit pkgs; };
  backend = agora.agora-backend;
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
    name = "agora/backend";
    tag = "latest";
    contents = [
      backend
    ];
    config.Cmd = [ "/bin/agora-exe" ];
    maxLayers = 120;
  };

  frontend-image = buildLayeredImage {
    name = "agora/frontend";
    tag = "latest";

    contents = [
      shadow-files
      nginxStable
      frontend
    ];

    config = {
      Entrypoint = [ "nginx" "-c" nginxConfig ];
      ExposedPorts = {
        "${toString nginxPort}/tcp" = {};
      };
    };
  };
}
