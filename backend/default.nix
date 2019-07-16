{ pkgs ? import ./../pkgs.nix, shell ? false }: with pkgs;
let
  withPostgreSQL = drv: pkgs.haskell.lib.overrideCabal drv (a: {
    testToolDepends = (a.testToolDepends or []) ++
      [ pkgs.ephemeralpg pkgs.postgresql pkgs.getopt ];
    preCheck = (a.preCheck or "") + ''
      export TEST_PG_CONN_STRING=$(pg_tmp -w 600)
    '';
    # we need the temporary directory from pg_tmp
    # so extract it out of $TEST_PG_CONN_STRING
    postCheck = (a.postCheck or "") + ''
      pg_tmp stop -d $(echo ''${TEST_PG_CONN_STRING#*=} | sed 's:%2F:/:g') || :
    '';
  });
in
stackToNix {
  root = constGitIgnore "agora-backend-src" ./. [
    "*.nix"
    "/README.md"
    "/docs"
  ];
  inherit shell;
  overrides = self: previous: {
    agora = (withPostgreSQL previous.agora);
  };
}
