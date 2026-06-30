#!/bin/sh
# Compile the test fixture shaders to SPIR-V with a local glslang. The committed
# .spv files are the stable input to reflection in the test suite (no glslang
# dependency at build time).
set -eu
cd "$(dirname "$0")"
for src in *.comp *.vert *.frag; do
  [ -e "$src" ] || continue
  # buffer_reference (BDA) needs SPIR-V 1.3+, i.e. a Vulkan 1.2 target; the
  # Int64 capability (uint64_t/int64_t) needs a Vulkan 1.1 target.
  if grep -q buffer_reference "$src"; then
    glslangValidator -V --target-env vulkan1.2 "$src" -o "$src.spv"
  elif grep -q int64 "$src"; then
    glslangValidator -V --target-env vulkan1.1 "$src" -o "$src.spv"
  else
    glslangValidator -V "$src" -o "$src.spv"
  fi
  echo "wrote $src.spv"
done
