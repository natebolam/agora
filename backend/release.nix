{ pkgs ? import ./../nix {} }: with pkgs;

let
  ignoreFilter = path: type:
    let
      inherit (lib) removePrefix hasPrefix hasSuffix;
      relPath = removePrefix (toString ./. + "/") (toString path);
      baseName = baseNameOf relPath;
    in
      !(
        baseName == ".gitignore" ||
        hasPrefix "resources" relPath ||
        hasSuffix ".md" baseName ||
        hasSuffix ".nix" baseName
      );

  # Set a constant name for the src closure
  source = let
    root = ./.;
  in builtins.path {
    name = "agora-backend-src";
    path = root;
    filter = name: type:
      (ignoreFilter name type && gitignoreFilter root name type && lib.cleanSourceFilter name type);
  };

  project = import ./. { inherit pkgs; };

  packages = with project; [
    agora
  ];
in
{
  agora-backend = symlinkJoin {
    name = "agora-backend";
    paths = map haskell.lib.justStaticExecutables packages;
  };

  agora-backend-config = runCommand "agora-backend-config" {} ''
    mkdir -p $out
    cp ${source}/config.yaml $out/base-config.yaml
  '';

  agora-backend-hlint = runCommand "hlint.html" {} ''
    ${hlint}/bin/hlint ${source} --no-exit-code --report=$out -j
  '';

  agora-backend-haddock = with lib;
    let
      docs = remove isNull (map (drv: drv.doc or null) (attrValues project));
      globs = map (doc: "${doc}/share/doc/*") docs;
    in
    runCommand "agora-backend-haddock.tar.gz" {} ''
      for drv in ${concatStringsSep " " globs}; do
        ln -s $drv/html $(basename $drv)
      done

      tar czfh $out *
    '';

  agora-backend-trailing-whitespace = checkTrailingWhitespace source;
}
