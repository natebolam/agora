{ pkgs ? import ./../pkgs.nix }: with pkgs;

let
  # Keep in mind that parent or global .gitignore are not respected
  source = constGitIgnore "agora-frontend" ./. [
    "/.cache"
    "/dist"
    "node_modules"
    "*.png"
  ];

  project = import ./. { inherit pkgs; };
in
{
  agora-frontend = project;
  agora-frontend-trailing-whitespace = checkTrailingWhitespace source;
}
