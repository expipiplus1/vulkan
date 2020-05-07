#!/usr/bin/env bash

set -e

################################################################
# This script will:
#
# - Build documentation for all packages with stack
# - Copy local packages to the output directory
# - Fixup links so that any non-local-package links point to
#   Hackage
#
# You may want to change 'stackCommand' or 'remote'
################################################################

if [ "$#" -ne 1 ]; then
    echo "Need output directory"
fi

out=$1

remote=https://hackage.haskell.org/package/
stackCommand="stack --skip-ghc-check --system-ghc --nix haddock --haddock-hyperlink-source"

printf "Generating docs with stack\n"
indexPath=$($stackCommand 2>&1 |
  tee >(cat 1>&2) |
  grep '/doc/index.html$' |
  grep '/.stack-work/' |
  head -n1)

docPath=$(dirname "$indexPath")
printf "Docs generated in $docPath\n"

mkdir -p "$out"

go(){
  printf "Copying docs to destination\n"
  cp -r "$docPath"/{vulkan*,Vulkan*,doc-index*,index.html,*.css,*.js,*.png} .

  printf "Replacing all relative links to point to Hackage\n"
  fd 'html$' --type f --exec \
    sed -i 's|<a href="\.\./\([^/]\+\)|<a href="'"$remote"'\1/docs|g'

  printf "Making absolute local links relative\n"
  fd 'html$' --type f --exec \
    sed -i 's|<a href="'"$docPath"'/|<a href="./|g'

  printf "Making any links to local packages on Hackage relative\n"
  fd . --type d --maxdepth 1 |
    while read d; do
      fd '.html$' --type f --exec sed -i 's|<a href="'"$remote$d"'/docs|<a href="../'"$d"'|g'
    done
}

(cd "$out" && go)
