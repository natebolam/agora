let
  pkgs = import ./pkgs.nix;
  src = builtins.toString ./.;
  stdenv = pkgs.stdenvNoCC;

  overlays = stdenv.mkDerivation {
    name = "nixpkgs-overlays";
    buildCommand = "mkdir -p $out && ln -s ${src}/pkgs $_";
  };
in
  with pkgs;

  mkShell {
    buildInputs = [
      nixopsUnstable
    ];

    NIX_PATH = builtins.concatStringsSep ":" [
      "nixpkgs=${toString pkgs.path}"
      "nixpkgs-overlays=${overlays}"
      "local=${src}"
    ];

    NIXOPS_STATE = "${src}/local.nixops";
  }
