#!/usr/bin/env bash

set -e

help=
startRev=HEAD
tagPrefix=v
packageYaml=package.yaml

# from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash
while [[ "$#" -gt 0 ]]; do
  case $1 in
  -h | --help) help=1 ;;
  --start-rev)
    startRev="$2"
    shift
    ;;
  --tag-prefix)
    tagPrefix="$2"
    shift
    ;;
  --package)
    packageYaml="$2"
    shift
    ;;
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
$me: Find the commit where the current version number in package.yaml was
set, and add an appropriate tag to that version.

Options:
  --tag-prefix: Prefix for git tag, (default "v")
  --start-rev: Revision at which to start searching for chages to version, (default "HEAD")
  --package: Path to package.yaml, (default "package.yaml")
  --help: duh
EOF
  exit 0
fi

################################################################
# Assertions
################################################################

if ! [ -f "$packageYaml" ]; then
  echo "$packageYaml doesn't seem to be a file" >&2
  exit 1
fi

if ! git rev-parse -q >/dev/null; then
  echo "This doesn't seem to be a git repository" >&2
  exit 1
fi

if ! git rev-parse -q "$startRev" 2>/dev/null >/dev/null; then
  echo "Revision $startRev doesn't exist" >&2
  exit 1
fi

if ! git rev-parse -q "$startRev"~ 2>/dev/null >/dev/null; then
  echo "This seems to be a shallow repository ($startRev~ doesn't exist)" >&2
  exit 1
fi

################################################################
# The program
################################################################

getVersion() {
  rev=$1
  yaml=$2

  if ! ver=$(git show "$rev:$yaml" | yq --exit-status --raw-output .version); then
    echo "Unable to get version from $yaml (at $rev)" >&2
    exit 1
  fi
  if ! [[ "$ver" =~ ^[0-9.]+$ ]]; then
    echo "version from $yaml (at $rev) isn't a valid version: $ver" >&2
    exit 1
  fi
  printf "%s" "$ver"
}

# This is the version we want to find the first appearance of on the main
# branch (the one followed by --first-parent)
currentVersion=$(getVersion "$startRev" "$packageYaml")

tag=$tagPrefix$currentVersion

# Exit if this version already has a tag
if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  echo "Tag $tag already exists" >&2
  exit
fi

# The current candidate for the package.yaml changing commit
prev=$(git rev-parse --short "$startRev")

# - Get all the comments on the main branch which touch this file, most recent
#   ones first, starting with the immediate parent
# - If the version has changed between this candidate and the parent, return
#   the candidate
# - Otherwise continue into history, using the parent as the new candidate
while read -r hash; do
  oldVersion=$(getVersion "$hash" "$packageYaml")
  if [ "$oldVersion" != "$currentVersion" ]; then
    break
  fi
  prev=$hash
done < <(git log --first-parent --pretty=tformat:"%h" "$startRev"~ -- "$packageYaml")

# The first commit changing the version
firstChange=$prev

# Set the tag
echo "Tagging $firstChange as $tag" >&2
git tag "$tag" "$firstChange"
