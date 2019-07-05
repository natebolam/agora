{ pkgs ? import ./pkgs.nix }: with pkgs;
let
  # Helpers to build system derivations
  shim = {
    boot.loader.systemd-boot.enable = true;

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/00000000-0000-0000-0000-000000000000";
      fsType = "btrfs";
    };
  };

  buildSystem = config: (import "${pkgs.path}/nixos" {
    configuration = { imports = [ config shim ]; };
  }).system;

  # Import project derivations
  backend  = import ./backend/release.nix { inherit pkgs; };
  frontend = import ./frontend/release.nix { inherit pkgs; };
  docker   = import ./docker.nix { inherit pkgs; };
in

{
  inherit (backend)
    agora-backend
    agora-backend-trailing-whitespace
    agora-backend-haddock
    agora-backend-hlint;

  inherit (frontend)
    agora-frontend
    agora-frontend-trailing-whitespace;

  inherit (docker)
    backend-image
    frontend-image;

  staging-server = buildSystem ./deployment/configuration.nix;
}
