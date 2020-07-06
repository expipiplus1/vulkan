#!/usr/bin/env bash

set -x
set -e

# like 1.4.145
version=$1

echo "Updating Vulkan-Docs"
git -C generate-new/Vulkan-Docs fetch
git -C generate-new/Vulkan-Docs checkout "v$version"

git add generate-new/Vulkan-Docs

if ! ./regenerate.sh; then
  echo "Failed to regenerate vulkan source"
  git restore --staged src/Vulkan src/Vulkan.hs
  git checkout src/Vulkan src/Vulkan.hs
  exit 1
fi

git add src
git add vulkan.cabal

echo "Adding version bump to changelog"
sed -i.bak "s/^## WIP$/\0\n  - Bump API version to $version/" changelog.md
git add changelog.md

git commit -m "Bump vulkan version to v$version"
