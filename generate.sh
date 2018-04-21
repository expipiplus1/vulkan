#!/usr/bin/env bash

set -e

generate=$(nix-build --no-out-link generate)
generate_bin="$generate"/bin/generate

cmd="$generate_bin $@"

nix-shell -p asciidoctor python3 --run "$cmd"
