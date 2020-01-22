#
# nix-flakes shim
#

{ exposeFlake ? false }:

let
  sources
    = builtins.removeAttrs (import ./nix/sources.nix) ["__functor"]
    ;

  inputs = builtins.mapAttrs (_: s: import s) sources;
  flake = (import ./flake.nix).outputs (inputs // { self = flake; });
in

if exposeFlake
then flake
else {
  inherit (flake.packages) agora-backend agora-frontend;
  inherit (flake.docker) backend-image frontend-image;
}
