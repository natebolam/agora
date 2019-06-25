{ pkgs ? import ./../pkgs.nix }: with pkgs;

mkShell {
  buildInputs = with nodePackages; [
    nodejs
  ];
}
