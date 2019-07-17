{ pkgs ? import ./../nix {} }: with pkgs;

mkShell {
  buildInputs = with nodePackages; [
    nodejs
  ];
}
