#!/bin/sh
# Compile this example's shaders to committed SPIR-V (input to reflection + runtime).
set -eu
cd "$(dirname "$0")"
for src in *.vert *.frag; do
  [ -e "$src" ] || continue
  glslangValidator -V "$src" -o "$src.spv"
  echo "wrote $src.spv"
done
