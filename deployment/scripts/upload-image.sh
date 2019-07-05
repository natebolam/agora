#!/usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash
set -euo pipefail
set -x

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

REGISTRY_TARGET="docker://$CI_REGISTRY_IMAGE/$1:latest"
IMAGE_PATH="$(nix-build $DIR/../../docker.nix -A $1-image --no-out-link)"

skopeo copy --dest-creds "$CI_REGISTRY_USER:$CI_REGISTRY_PASSWORD" "tarball://$IMAGE_PATH" "$REGISTRY_TARGET"
