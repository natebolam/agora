{
  edition = 201911;

  description = "Explorer for StakerDAO governance";

  outputs =
    deps@{ self, nixpkgs, serokell-nix, haskell-nix, nix-npm-buildpackage }:

      let
        pkgs = nixpkgs {
          overlays = [
            (final: _prev: {
              inherit (final.callPackage nix-npm-buildpackage {}) buildNpmPackage;
            })
          ]
          ++ [serokell-nix.overlay]
          ++ haskell-nix.overlays;
        };

        # Used only for whitespace check and frontend.
        # Backend uses cleanGit from haskell.nix.
        src = serokell-nix.lib.cleanSource "agora" ./.;

        agora-backend = import ./backend { inherit pkgs; };
        agora-frontend = import ./frontend {
          inherit pkgs;
          src = "${src}/frontend";
        };
        docker = import ./nix/docker.nix {
          inherit pkgs;
          agora = self.packages;
        };
      in {

        packages = {
          agora-backend = agora-backend.components.exes.agora;
          agora-backend-config = pkgs.runCommand "agora-backend-config" {} ''
            mkdir -p $out
            cp ${./backend/config.yaml} "$out"/base-config.yaml
          '';
          inherit agora-frontend;
        };

        docker = {
          inherit (docker) backend-image frontend-image;
        };

        checks = {
          whitespace = pkgs.build.checkTrailingWhitespace src;

          backend = {
            build = self.packages.agora-backend;
            haddock = pkgs.build.haskell.haddock "agora" [
              agora-backend.components.library.doc
            ];
            hlint = pkgs.build.haskell.hlint src;
            docker = self.docker.backend-image;
          };

          frontend = {
            build = self.packages.agora-frontend;
            docker = self.docker.frontend-image;
          };
        };

      };
}
