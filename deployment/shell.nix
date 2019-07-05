{ pkgs ? import ./../pkgs.nix }: with pkgs;
let
  src = builtins.toString ./.;
  tf = pkgs.terraform_0_12.withPlugins(p: with p; [
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
      "nixpkgs=${toString pkgs.path}"
      "local=${src}"
    ];

    NIXOPS_STATE = "${src}/local.nixops";
  }
