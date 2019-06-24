{ pkgs ? import ./../pkgs.nix }: with pkgs;

let
  # Keep in mind that parent or global .gitignore are not respected
  source = constGitIgnore "agora-backend-release-src" ./. [
    ".stack-work"
  ];
  project = import ./. { inherit pkgs; };

  runCheck = source: runCommand "check" {} ''
    ${source}
    touch $out
  '';

  packages = with project; [
    agora
  ];
in
{
  agora-backend = symlinkJoin {
    name = "agora-backend";
    paths = map haskell.lib.justStaticExecutables packages;
  };

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

  agora-backend-trailing-whitespace = runCheck ''
    files=$(grep -rl '[[:blank:]]$' ${source} || true)
    if [[ ! -z $files ]];then
      echo '  Files with trailing whitespace found:'
      for f in ''${files[*]}; do
        echo "  * $f" | sed -re 's|/nix/store/[a-z0-9]+-agora-backend-release-src/||'
      done
      exit 1
    fi
  '';
}
