#!/usr/bin/env bash

set -x
set -e

# like 1.4.145
version=$1

echo "Updating and cleaning Vulkan-Docs"
git -C generate-new/Vulkan-Docs fetch
git -C generate-new/Vulkan-Docs checkout v$version
git -C generate-new/Vulkan-Docs clean -dxf

git add generate-new/Vulkan-Docs

echo "Generating Vulkan-Docs documentation"
(cd generate-new/Vulkan-Docs &&
  nix-shell -p \
    python3 asciidoctor gnumake nodejs nodePackages.he nodePackages.escape-string-regexp \
    --run "./makeAllExts refpages generated")

echo "Building generator"
nix-shell --run "cabal build generate-new"

echo "Cleaning src"
rm -r src/Vulkan src/Vulkan.hs
echo "Generating vulkan"
nix-shell --run "sh -c 'cd generate-new && cabal run vk'"
hpack

git add src
git add vulkan.cabal

echo "Adding version bump to changelog"
sed -i.bak 's/^## WIP$/\0\n  - Bump API version to 1.2.145/' changelog.md
git add changelog.md

git commit -m "Bump vulkan version to v$version"

# VMA stuff

# echo "Updating and cleaning VulkanMemoryAllocator"
# git -C VulkanMemoryAllocator/VulkanMemoryAllocator pull
# git -C VulkanMemoryAllocator/VulkanMemoryAllocator checkout .
# git -C VulkanMemoryAllocator/VulkanMemoryAllocator clean -dxf

# echo "Generating VMA documentation"
# (cd VulkanMemoryAllocator/VulkanMemoryAllocator/src &&
#   sed -i -e 's|^GENERATE_DOCBOOK.*|GENERATE_DOCBOOK=YES|' -e 's|^BRIEF_MEMBER_DESC.*|BRIEF_MEMBER_DESC=NO|' Doxyfile &&
#   nix-shell -p doxygen --run "doxygen Doxyfile")
# echo "cleaning VulkanMemoryAllocator src"
# rm -r VulkanMemoryAllocator/src/VulkanMemoryAllocator.hs
# echo "Generating VulkanMemoryAllocator"
# (cd generate-new && cabal run vma)
# hpack VulkanMemoryAllocator
