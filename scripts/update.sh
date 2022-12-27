#!/usr/bin/env bash

set -x
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

no_doc_diff=
requested_version=

while [[ "$#" -gt 0 ]]; do
  case $1 in
  -h | --help) help=1 ;;
  --no-doc-diff)
    no_doc_diff="$2"
    shift
    ;;
  --version)
    requested_version="$2"
    shift
    ;;
  --force) force=1 ;;
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
$me: Update Vulkan-Docs and regenerate source

Options:
  --version v1.2.145 : Update to this version of Vulkan-Docs rather than the
                       latest
  --no-doc-diff file.patch : Generate a diff between the existing and new
                             version without documentaiton comments
  --force : regenerate and commit even with no version update
EOF
  exit
fi

# like v1.4.145
latest_version=$(curl -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/KhronosGroup/Vulkan-Docs/tags |
  jq --raw-output 'map(.name) | .[]' |
  sort --version-sort | tail -n1)
version=${requested_version:-$latest_version}

# For GitHub actions
echo "::set-output name=vulkan_version::$version"

echo "Fetching Vulkan-Docs"
git -C generate-new/Vulkan-Docs fetch
git -C generate-new/Vulkan-Docs fetch --tags

if git -C generate-new/Vulkan-Docs describe --tags | grep "$version"; then
  echo "Vulkan-Docs is already at $version"
  if ! [ $force ]; then
    exit 0
  fi
fi

before=
if [ "$no_doc_diff" ]; then
  before=$(nix-build --no-out-link nix/generate.nix --arg withDocs false)
fi

echo "Updating Vulkan-Docs"
git -C generate-new/Vulkan-Docs checkout "tags/$version"

after=
if [ "$no_doc_diff" ]; then
  after=$(nix-build --no-out-link nix/generate.nix --arg withDocs false)
  git diff --no-ext-diff --no-index "$before"/ "$after"/ > "$no_doc_diff" || true
fi


git add generate-new/Vulkan-Docs

if ! "$DIR/regenerate.sh"; then
  echo "Failed to regenerate vulkan source"
  git restore --staged src/Vulkan src/Vulkan.hs
  git checkout src/Vulkan src/Vulkan.hs
  exit 1
fi

git add src
git add vulkan.cabal

if git diff --cached --no-ext-diff --quiet --exit-code; then
  echo "no changes"
else
  echo "Adding version bump to changelog"
  sed -i.bak "s/^## WIP$/\0\n- Bump API version to $version/" changelog.md
  git add changelog.md

  git commit -m "Bump vulkan version to $version"
fi
