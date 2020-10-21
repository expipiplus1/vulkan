#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

help=
regenerate=
vulkan_version=
vma_version=
utils_version=
haddocks=

# from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -h|--help) help=1 ;;
        -r|--regenerate) regenerate=1 ;;
        --vulkan) vulkan_version="$2"; shift ;;
        --vma) vma_version="$2"; shift ;;
        --utils) utils_version="$2"; shift ;;
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
      --utils 0.1.3: new vulkan-utils version
      --standalone-haddocks path/to/standalone-haddocks
EOF
  exit 0
fi

ghc --version

if [ "$vulkan_version" ]; then
  tag=v$vulkan_version
elif [ "$vma_version" ]; then
  tag=vma-v$vma_version
elif [ "$utils_version" ]; then
  tag=vulkan-utils-v$utils_version
else
  echo "running without new version of any package"
  exit 1
fi

if [ $regenerate ]; then
  echo "Regenerating source"
  "$DIR/regenerate.sh"
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

  vulkan_breaking=$(yq <package.yaml .version --raw-output |
    sed -E 's/([0-9]+\.[0-9]+).*/\1/')

  sed -i.bak "s/^version: .*/version: $vma_version/g" VulkanMemoryAllocator/package.yaml
  sed -i.bak "s/- vulkan [0-9 .<>=*]*/- vulkan == $vulkan_breaking.*/" VulkanMemoryAllocator/package.yaml
  sed -i.bak "s/^## WIP$/\0\n\n## [$vma_version] - $(date --iso-8601)/" VulkanMemoryAllocator/changelog.md
  hpack VulkanMemoryAllocator
  git add VulkanMemoryAllocator/package.yaml VulkanMemoryAllocator/VulkanMemoryAllocator.cabal VulkanMemoryAllocator/changelog.md
fi

if [ "$utils_version" ]; then
  echo "Bumping vulkan-utils version and generating tarballs"

  vulkan_breaking=$(yq <package.yaml .version --raw-output |
    sed -E 's/([0-9]+\.[0-9]+).*/\1/')

  sed -i.bak "s/^version: .*/version: $utils_version/g" utils/package.yaml
  sed -i.bak "s/- vulkan [0-9 .<>=*]*/- vulkan == $vulkan_breaking.*/" utils/package.yaml
  sed -i.bak "s/^## WIP$/\0\n\n## [$utils_version] - $(date --iso-8601)/" utils/changelog.md
  hpack utils
  git add utils/package.yaml utils/vulkan-utils.cabal utils/changelog.md
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

if [ "$utils_version" ]; then
  echo "generating vulkan-utils tarballs"
  cabal haddock --haddock-option="--hyperlinked-source"
  cabal haddock --haddock-for-hackage --haddock-option="--hyperlinked-source" vulkan-utils
  cabal sdist vulkan-utils
fi

if [ "$haddocks" ]; then
  git -C "$haddocks" rm --quiet -r -- .
  nix-shell --pure -p stack nix fd --run "NIX_PATH=$NIX_PATH \"$DIR/gen-standalone-haddocks.sh\" \"$haddocks\""
  git -C "$haddocks" add .
  git -C "$haddocks" commit -m "v$vulkan_version"
fi


cat <<EOF
  --------------------------------
  Commands to upload these changes
  --------------------------------

  # Open a PR for this release
  git push --set-upstream origin "$branch"
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

  # After checking everything's OK
  cabal upload --publish dist-newstyle/sdist/vulkan-$vulkan_version.tar.gz
  cabal upload --publish --doc dist-newstyle/vulkan-$vulkan_version-docs.tar.gz

EOF
fi

if [ "$vma_version" ]; then
  cat <<EOF
  # Upload VulkanMemoryAllocator-$vma_version
  cabal upload dist-newstyle/sdist/VulkanMemoryAllocator-$vma_version.tar.gz
  cabal upload --doc dist-newstyle/VulkanMemoryAllocator-$vma_version-docs.tar.gz

  # After checking everything's OK
  cabal upload --publish dist-newstyle/sdist/VulkanMemoryAllocator-$vma_version.tar.gz
  cabal upload --publish --doc dist-newstyle/VulkanMemoryAllocator-$vma_version-docs.tar.gz
EOF
fi

if [ "$utils_version" ]; then
  cat <<EOF
  # Upload vulkan-utils-$utils_version
  cabal upload dist-newstyle/sdist/vulkan-utils-$utils_version.tar.gz
  cabal upload --doc dist-newstyle/vulkan-utils-$utils_version-docs.tar.gz

  # After checking everything's OK
  cabal upload --publish dist-newstyle/sdist/vulkan-utils-$utils_version.tar.gz
  cabal upload --publish --doc dist-newstyle/vulkan-utils-$utils_version-docs.tar.gz
EOF
fi

if [ "$haddocks" ]; then
  cat <<EOF
  # Upload standalone haddocks
  git -C $haddocks push
EOF
fi
