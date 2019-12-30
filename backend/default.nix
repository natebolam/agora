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
          agora = {
            preCheck = ''
              export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
            '';
            # we need the temporary directory from pg_tmp
            # so extract it out of $TEST_PG_CONN_STRING
            postCheck = ''
              pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
            '';
          };
          components.tests.agora-test = {
            depends = with pkgs; [ ephemeralpg getopt postgresql ];
          };
        };
      })
    ];
  };
in project.agora
