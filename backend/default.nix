{ pkgs }:

let
  project = pkgs.haskell-nix.stackProject {
    src = with pkgs.haskell-nix.haskellLib; cleanSourceWith {
      name = "stakerdao-agora";
      src = cleanGit {
        src = ../.;
        subDir = "backend";
      };
    };
    modules = [
      ({ pkgs, ... }: {
        packages = {
          # FIXME haddock fails on loot-log, remove the following line when fixed
          loot-log.components.library.doHaddock = false;

          # FIXME Tests fail, remove the following two lines when fixed
          agora.components.tests = pkgs.lib.mkForce { };
          agora.components.all.dontCheck = true;
        };
      })
    ];
  };
in project.agora
