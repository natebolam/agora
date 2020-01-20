let
  release = (import ./default.nix { });
  generatedSteps = builtins.attrValues (builtins.mapAttrs (name: _: {
    label = name;
    command = "nix-build --no-out-link -A ${name}";
    agents = [ "private=true" ];
  }) release);
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
    {
      label = "Deploy staging";
      agents = [ "private=true" ];
      branches = [ "staging" ];
      command = ''
        ssh buildkite@stakerdao.tezos.serokell.team "nix-shell -p git gnutar --run 'sudo nix-channel --update; sudo nixos-rebuild switch --upgrade'"
      '';
    }
  ];
  steps = generatedSteps ++ extraSteps;
in builtins.toFile "pipeline.yml" (builtins.toJSON { inherit steps; })
