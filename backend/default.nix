{ pkgs ? import ./../pkgs.nix, shell ? false }: with pkgs;

stackToNix {
  root = constGitIgnore "agora-backend-src" ./. [
    "*.nix"
    "/README.md"
    "/docs"
  ];
  inherit shell;
}
