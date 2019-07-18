{ pkgs ? import ./../nix {} }: with pkgs;
let
  sources = import ../nix/sources.nix;
  inherit (builtins) toString;
  src = builtins.toString ./.;

  tf = terraform_0_12.withPlugins(p: with p; [
    aws
  ]);
in
  mkShell {
    buildInputs = [
      nixopsUnstable

      tf
      terraform-docs
      awscli

      skopeo
      bash
    ];

    NIX_PATH = builtins.concatStringsSep ":" [
      "nixpkgs=${sources.nixpkgs}"
      "local=${src}"
    ];

    NIXOPS_STATE = "${src}/local.nixops";
  }
