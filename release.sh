#!/usr/bin/env bash

set -e

help=
regenerate=
vulkan_version=
vma_version=
haddocks=

# from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -h|--help) help=1 ;;
        -r|--regenerate) regenerate=1 ;;
        --vulkan) vulkan_version="$2"; shift ;;
        --vma) vma_version="$2"; shift ;;
        --standalone-haddocks) haddocks="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

if [ $help ]; then
  cat <<EOF
    Options:
      --regenerate: regenerate source
      --vulkan 3.4.5: new vulkan version
      --vma 3.4.1: new VulkanMemoryAllocator version
      --standalone-haddocks path/to/standalone-haddocks
EOF
  exit 0
fi

if [ "$vulkan_version" ]; then
  tag=v$vulkan_version
elif [ "$vma_version" ]; then
  tag=vma-v$vma_version
else
  echo "running without new vma or vulkan version"
  exit 1
fi

if [ $regenerate ]; then
  echo "Regenerating source"
  ./regenerate.sh
fi

if [[ -n $(git status --short --untracked-files=no) ]]; then
  echo "There are untracked changes in the working tree, please resolve these before making a release"
  exit 1
fi

if [ "$vulkan_version" ]; then
  echo "Bumping vulkan version and generating tarballs"

  sed -i.bak "s/^version: .*/version: $vulkan_version/g" package.yaml
  sed -i.bak "s/^## WIP$/\0\n\n## [$vulkan_version] - $(date --iso-8601)/" changelog.md
  hpack
  git add package.yaml vulkan.cabal changelog.md
fi

if [ "$vma_version" ]; then
  echo "Bumping VulkanMemoryAllocator version and generating tarballs"

  vulkan_breaking=$(yq <package.yaml .version |
    sed -E 's/([0-9]+\.[0-9]+).*/\1/')

  sed -i.bak "s/^version: .*/version: $vma_version/g" VulkanMemoryAllocator/package.yaml
  sed -i.bak "s/- vulkan [0-9 .<>=*]*/- vulkan == $vulkan_breaking.*/" VulkanMemoryAllocator/package.yaml
  sed -i.bak "s/^## WIP$/\0\n\n## [$vma_version] - $(date --iso-8601)/" VulkanMemoryAllocator/changelog.md
  hpack VulkanMemoryAllocator
  git add VulkanMemoryAllocator/package.yaml VulkanMemoryAllocator/VulkanMemoryAllocator.cabal VulkanMemoryAllocator/changelog.md
fi

branch="release-$tag"
git checkout -b "$branch"
git commit -m "$tag"

if [ "$vulkan_version" ]; then
  git tag "$tag"
fi

if [ "$vulkan_version" ]; then
  echo "Generating vulkan tarballs"
  cabal haddock  --haddock-for-hackage --haddock-option="--hyperlinked-source"
  cabal sdist
fi

if [ "$vma_version" ]; then
  echo "generating VulkanMemoryAllocator tarballs"
  cabal haddock --haddock-option="--hyperlinked-source"
  cabal haddock --haddock-for-hackage --haddock-option="--hyperlinked-source" VulkanMemoryAllocator
  cabal sdist VulkanMemoryAllocator
fi

if [ "$haddocks" ]; then
  git -C "$haddocks" rm --quiet -r -- .
  nix-shell --pure -p stack nix --run "NIX_PATH=$NIX_PATH ./gen-standalone-haddocks.sh \"$haddocks\""
  git -C "$haddocks" add .
  git -C "$haddocks" commit -m "v$vulkan_version"
fi


cat <<EOF
  --------------------------------
  Commands to upload these changes
  --------------------------------

  # Open a PR for this release
  git push
  git pull-request
  # Wait for CI to complete
  git push --tags
  git checkout master
  git merge "$branch"
  git push

EOF

if [ "$vulkan_version" ]; then
  cat <<EOF
  # Upload vulkan-$vulkan_version
  cabal upload dist-newstyle/sdist/vulkan-$vulkan_version.tar.gz
  cabal upload --doc dist-newstyle/vulkan-$vulkan_version-docs.tar.gz

EOF
fi

if [ "$vma_version" ]; then
  cat <<EOF
  # Upload VulkanMemoryAllocator-$vulkan_version
  cabal upload dist-newstyle/sdist/VulkanMemoryAllocator-$vma_version.tar.gz
  cabal upload --doc dist-newstyle/VulkanMemoryAllocator-$vma_version-docs.tar.gz

EOF
fi

if [ "$haddocks" ]; then
  # Upload standalone haddocks
  echo "git -C \"$haddocks\" push"
fi
