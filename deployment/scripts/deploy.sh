#!/usr/bin/env bash
# shellcheck shell=bash
set -euo pipefail
# set -x

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )/.."
NIXOS_REBUILD="$ROOT/scripts/nixos-rebuild"
export NIXOS_CONFIG="$ROOT/aws.nix"


if [[ "${1:-switch}" == "build" ]]; then
    "$NIXOS_REBUILD" build

else
    HOST="root@staging.agora.tezos.serokell.org"

    if [[ -n $CI ]]; then
        # $STAGING_SSH_KEY is a path pointing at a file with the private SSH key
        # Variable defined in CI settings -> Variables
        # https://gitlab.com/tezosagora/agora/-/settings/ci_cd

        # ssh does not like the default 644 permissions
        chmod 600 "$STAGING_SSH_KEY"
        export NIX_SSHOPTS="-i $STAGING_SSH_KEY"
    fi

    "$NIXOS_REBUILD" --target-host "$HOST" --build-host localhost switch

    ## Restart docker units so they re-pull from registry
    for unit in node; do
        # shellcheck disable=SC2029 disable=SC2086
        ssh ${NIX_SSHOPTS:-} "$HOST" systemctl restart "docker-${unit}.service"
    done
fi
