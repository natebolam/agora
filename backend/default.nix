{ _expose ? false }:

let
  pkgs = import ../nix { };
  inherit (pkgs) haskell;

  project = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ../.;
      subDir = "backend";
    };
    modules = [
      ({ pkgs, ... }: {
        packages = {
          loot-log.components.library.doHaddock = false;
          agora.components.all.dontCheck = true;
        };
      })
    ];
  };
in project.agora
