#!/usr/bin/env bash

set -e

help=
requireFirst=
releaseNote=release-note.md
assets=assets
ignoreDirty=

# from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
while [[ "$#" -gt 0 ]]; do
  case $1 in
  -h | --help) help=1 ;;
  --require-first)
    requireFirst="$2"
    shift
    ;;
  --release-note)
    releaseNote="$2"
    shift
    ;;
  --assets)
    assets="$2"
    shift
    ;;
  --ignore-dirty) ignoreDirty=1 ;;
  *)
    echo "Unknown parameter passed: $1" >&2
    exit 1
    ;;
  esac
  shift
done

if [ $help ]; then
  me=$(basename "$0")
  cat <<EOF
$me: Create releases for the tags on HEAD. Fails when there are no relevant
     tags.

Options:
  --require-first: Exit without doing anything if this doesn't match the
      first package in the release. This is useful for CI scripts which are
      spawned multiple times on the same commmit with several matching tags,
      passing the spawning tag here will ensure that only the "highest
      priority" job actually does anything because it will be the only job for
      which, once the tags are sorted by priority, the highest priority tag
      matches this argument.
  --release-note: The file into which to write a note for the release in
      markdown. Default "release-note.md".
  --assets: A directory into which release assets will be placed, will be
      created if it is absent. Default "assets".
  --ignore-dirty: Proceed even with a dirty git tree.
  --help: show this message
EOF
  exit
fi

################################################################
# Releases every package which has a release tagged on this commit
################################################################

tagNames=(^v[0-9.]+$ ^vma-v[0-9.]+$ ^utils-v[0-9.]+$)
funNames=(releaseVulkan releaseVMA releaseUtils releaseOpenXR)

# Get the tags on HEAD
mapfile -t tags < <(git tag --points-at HEAD | sort)

# Order them according to the arrays above
declare -a tagMap
for i in "${tags[@]}"; do
  for r in "${!tagNames[@]}"; do
    if [[ "$i" =~ ${tagNames[$r]} ]]; then
      tagMap[$r]=$i
    fi
  done
done

if [ ${#tagMap[@]} -eq 0 ]; then
  echo >&2 "No relevant tags to release"
  exit
fi

if ! [ $ignoreDirty ] && [[ -n $(git status --short --untracked-files=no) ]]; then
  echo "There are untracked changes in the working tree, please resolve these before making a release or pass '--ignore-dirty'"
  exit 1
fi

# If we're not the job running on the most important release, exit
for first in "${tagMap[@]}"; do break; done
if [ "$requireFirst" ] && [ "$requireFirst" != "$first" ]; then
  echo >&2 "Skipping because --require-first didn't match"
  exit
fi

releaseTitle="Release $(printf "%s" "${tagMap[*]}" | sed 's/ /, /g')"
echo "$releaseTitle" >&2

printf "%s\n\n" "$releaseTitle" >"$releaseNote"

releaseVulkan() {
  mkdir -p "$assets"
  ln -s "$(nix-build nix/release.nix -A vulkan --no-out-link)"/*.tar.gz "$assets/"
  ln -s "$(nix-build nix/release.nix -A docs.vulkan --no-out-link)"/*.tar.gz "$assets/"
  awk '/## WIP/{flag=0;next};/##/{flag=flag+1};flag==1' <changelog.md |
    sed "s/##/## Vulkan/" >>"$releaseNote"
}

releaseVMA() {
  mkdir -p "$assets"
  ln -s "$(nix-build nix/release.nix -A VulkanMemoryAllocator --no-out-link)"/*.tar.gz "$assets/"
  ln -s "$(nix-build nix/release.nix -A docs.VulkanMemoryAllocator --no-out-link)"/*.tar.gz "$assets/"
  awk '/## WIP/{flag=0;next};/##/{flag=flag+1};flag==1' <VulkanMemoryAllocator/changelog.md |
    sed "s/##/## VulkanMemoryAllocator/" >>"$releaseNote"
}

releaseUtils() {
  mkdir -p "$assets"
  ln -s "$(nix-build nix/release.nix -A vulkan-utils --no-out-link)"/*.tar.gz "$assets/"
  ln -s "$(nix-build nix/release.nix -A docs.vulkan-utils --no-out-link)"/*.tar.gz "$assets/"
  awk '/## WIP/{flag=0;next};/##/{flag=flag+1};flag==1' <utils/changelog.md |
    sed "s/##/## vulkan-utils/" >>"$releaseNote"
}

releaseOpenXR() {
  mkdir -p "$assets"
  ln -s "$(nix-build nix/release.nix -A openxr --no-out-link)"/*.tar.gz "$assets/"
  ln -s "$(nix-build nix/release.nix -A docs.openxr --no-out-link)"/*.tar.gz "$assets/"
  awk '/## WIP/{flag=0;next};/##/{flag=flag+1};flag==1' <openxr/changelog.md |
    sed "s/##/## OpenXR/" >>"$releaseNote"
}

for i in "${!tagMap[@]}"; do
  ${funNames[$i]}
  echo >>"$releaseNote"
done
