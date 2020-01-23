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
          inherit agora-frontend;
        };

        docker = {
          inherit (docker) backend-image frontend-image;
        };

        nixosModules.agora = import ./nix/modules/agora.nix;

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

          module =
            let
              cfg = (pkgs.lib.evalModules {
                modules = [
                  self.nixosModules.agora
                  { agora = {
                      api.listen_addr = "*:8190";
                      logging.min-severity = "Debug";
                      contract = {
                        address = "KT1someContractAddressForExampleHere";
                        contract_block_level = 12345;
                      };
                      node_addr = "tezos.example.com:8732";
                      discourse = {
                        host = "https://discourse.example.com";
                        api_key = "d06ca53322d1fbaf383a6394d6c229e56871342d2cad953a0fe26c19df7645ba";
                      };
                    };
                  }
                ];
              }).config.agora;
            in {
              config.write = pkgs.writeTextFile {
                name = "agora-config.yaml";
                text = builtins.toJSON cfg;
              };
            };
        };
      };
}
