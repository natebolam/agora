{ pkgs ? import ./../nix {}, shell ? false }: with pkgs;
let
  withPostgreSQL = drv: haskell.lib.overrideCabal drv (a: {
    testToolDepends = (a.testToolDepends or []) ++
      [ ephemeralpg postgresql getopt ];
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
  root = gitignoreSource ./.;
  inherit shell;
  overrides = _: super: {
    agora = (withPostgreSQL super.agora);
  };
}
