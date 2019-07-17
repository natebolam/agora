{ sources ? import ./sources.nix }:
let
  all-cabal-hashes-component = name: version: type:
    builtins.fetchurl "https://raw.githubusercontent.com/commercialhaskell/all-cabal-hashes/hackage/${name}/${version}/${name}.${type}";
in
[(self: super: {
  stackToNix = self.callPackage sources.stack-to-nix {};
  inherit (self.callPackage sources.nix-npm-buildpackage {}) buildNpmPackage;
  inherit (self.callPackage sources.gitignore {}) gitignoreSource;

   haskellPackages = super.haskellPackages.override { overrides = self: super: {
    hackage2nix = name: version: self.haskellSrc2nix {
      name   = "${name}-${version}";
      sha256 = ''$(sed -e 's/.*"SHA256":"//' -e 's/".*$//' "${all-cabal-hashes-component name version "json"}")'';
      src    = all-cabal-hashes-component name version "cabal";
    };
   };};

  /*
  * Run a series of commands only for their exit status, producing an empty
  * closure.
  */
  runCheck = script: src:  self.runCommand "check" {} ''
    src="${src}"
    ${script}
    touch $out
  '';

  /*
  * Check the given target path for files with trailing whitespace, fail if any
  * are found
  */
  checkTrailingWhitespace = self.runCheck ''
    files=$(grep --recursive --files-with-matches --binary-files=without-match '[[:blank:]]$' "$src" || true)
    if [[ ! -z $files ]];then
      echo '  Files with trailing whitespace found:'
      for f in "''${files[@]}"; do
        echo "  * $f" | sed -re "s|$src/||"
      done
      exit 1
    fi
  '';
})]
