#!/bin/sh
# Compile the GLSL shaders to SPIR-V with a local glslang. The resulting .spv is
# committed and consumed both at runtime (Data.FileEmbed.embedFile) and at
# compile time (Vulkan.Utils.SpirV reflection).
set -eu
cd "$(dirname "$0")"
glslangValidator -V shader.comp -o shader.comp.spv
echo "wrote shader.comp.spv"
