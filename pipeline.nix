let
  release = (import ./default.nix { });
  generatedSteps = builtins.attrValues (builtins.mapAttrs (name: _: {
    label = name;
    command = "nix-build --no-out-link -A ${name}";
    agents = [ "private=true" ];
  }) release);
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
