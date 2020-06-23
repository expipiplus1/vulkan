#!/usr/bin/env bash

set -x
set -e

# like 1.4.145
version=$1

echo "Updating Vulkan-Docs"
git -C generate-new/Vulkan-Docs fetch
git -C generate-new/Vulkan-Docs checkout v$version

git add generate-new/Vulkan-Docs

./regenerate.sh "$version"

git add src
git add vulkan.cabal

echo "Adding version bump to changelog"
sed -i.bak "s/^## WIP$/\0\n  - Bump API version to $version/" changelog.md
git add changelog.md

git commit -m "Bump vulkan version to v$version"
