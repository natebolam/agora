let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {
    inherit (import sources."haskell.nix") config overlays;
  };
in

pkgs.haskell.lib.buildStackProject {
  name = "agora";
  ghc = pkgs.haskell-nix.compiler.ghc865;
  buildInputs = with pkgs; [ lzma postgresql zlib ];
}
