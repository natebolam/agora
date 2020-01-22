let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) lib;
  inherit (lib) collect concatStringsSep mapAttrsRecursiveCond;

  checks = (import ./default.nix { exposeFlake = true; }).checks;
  checkNamesTree =
    mapAttrsRecursiveCond
      (x: !(lib.isDerivation x))
      (path: _: concatStringsSep "." path)
      checks;
  checkNames = collect lib.isString checkNamesTree;

  generatedSteps =
    map
      (name: {
        label = name;
        command = "nix-build --no-out-link --arg exposeFlake true -A checks.${name}";
        agents = [ "private=true" ];
      })
      checkNames;

  deploy = branch: target: {
      label = "Deploy ${branch}";
      agents = [ "private=true" ];
      branches = [ branch ];
      command = ''
        ssh buildkite@${target} "nix-shell -p git gnutar --run 'sudo nix-channel --update; sudo nixos-rebuild switch'"
      '';
  };
  extraSteps = [
    "wait"
    {
      label = "Build and copy to wasabi";
      agents = [ "private=true" ];
      command = ''
        nix-build
        echo "+++ Uploading to wasabi"
        nix copy --to "s3://serokell-public-cache?endpoint=s3.eu-central-1.wasabisys.com"
      '';
    }
    "wait"
    (deploy "staging" "stakerdao.tezos.serokell.team")
    (deploy "staging2" "stakerdao2.tezos.serokell.team")
  ];
  steps = generatedSteps ++ extraSteps;
in builtins.toFile "pipeline.yml" (builtins.toJSON { inherit steps; })
