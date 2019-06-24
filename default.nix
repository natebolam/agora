{ pkgs ? import ./pkgs.nix }: with pkgs;
let
  backend = import ./backend/release.nix { inherit pkgs; };
in

{
  inherit (backend)
    agora-backend
    agora-backend-trailing-whitespace
    agora-backend-haddock
    agora-backend-hlint;
}
