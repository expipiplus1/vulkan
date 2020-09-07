#!/usr/bin/env bash

set -x
set -e

################################################################
# Vulkan
################################################################

echo "Cleaning Vulkan-Docs"
git -C generate-new/Vulkan-Docs clean -dxf

echo "Generating Vulkan-Docs documentation"
(cd generate-new/Vulkan-Docs &&
  nix-shell -p \
    python3 asciidoctor gnumake nodejs nodePackages.he nodePackages.escape-string-regexp \
    --run "./makeAllExts refpages generated")

echo "Building generator"
generate=$(IN_NIX_SHELL='' nix-build -A generate-new)

echo "Cleaning src"
git rm --quiet -r src/Vulkan src/Vulkan.hs
mkdir -p src
test -f generate-new/out || test -L generate-new/out || ln -s ../src generate-new/out
echo "Generating vulkan"
nix-shell -p asciidoctor --run "sh -c 'cd generate-new && \"$generate/bin/vk\"'"
git add src
hpack

################################################################
# VMA
################################################################

echo "Cleaning VulkanMemoryAllocator source"
git -C VulkanMemoryAllocator/VulkanMemoryAllocator clean -dxf

echo "Generating VMA documentation"
(cd VulkanMemoryAllocator/VulkanMemoryAllocator/src &&
  sed -i -e 's|^GENERATE_DOCBOOK.*|GENERATE_DOCBOOK=YES|' -e 's|^BRIEF_MEMBER_DESC.*|BRIEF_MEMBER_DESC=NO|' Doxyfile &&
  nix-shell -p doxygen --run "doxygen Doxyfile")

echo "Generating VulkanMemoryAllocator"
nix-shell -p asciidoctor --run "sh -c 'cd generate-new && \"$generate/bin/vma\"'"
hpack VulkanMemoryAllocator

echo "Cleaning VMA documentation"
git -C VulkanMemoryAllocator/VulkanMemoryAllocator clean -dxf
git -C VulkanMemoryAllocator/VulkanMemoryAllocator checkout .
