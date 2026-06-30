#!/bin/sh
# Compile the GLSL shader to SPIR-V with a local glslang. The resulting .spv is
# committed and consumed both at runtime (Data.FileEmbed.embedFile) and at
# compile time (Vulkan.Utils.SpirV reflection).
set -eu
cd "$(dirname "$0")"
# buffer_reference (BDA) needs SPIR-V 1.3+, i.e. a Vulkan 1.2 target.
glslangValidator -V --target-env vulkan1.2 shader.comp -o shader.comp.spv
echo "wrote shader.comp.spv"
