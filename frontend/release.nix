{ pkgs ? import ./../nix {} }: with pkgs;

let
  # Keep in mind that parent or global .gitignore are not respected
  source = gitignoreSource ./.;

  project = import ./. { inherit pkgs; };
in
{
  agora-frontend = project;
  agora-frontend-trailing-whitespace = checkTrailingWhitespace source;
}
