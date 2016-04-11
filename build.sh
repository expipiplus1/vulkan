#!/usr/bin/env bash
set -e
pushd generate
stack build $*
rm -rf out
cat ./Vulkan-Docs/src/spec/vk.xml | stack exec generate
popd
rm -rf src
mv generate/out src
stack build $*
