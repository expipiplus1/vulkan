#!/usr/bin/env bash

set -x
set -e

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
NIX_DOC_DEPS="python3 python3Packages.pyparsing asciidoctor gnumake nodejs nodePackages.he nodePackages.escape-string-regexp"
GENERATE=""

usage() {
  cat <<'USAGE'
Usage: ./scripts/regenerate.sh [target] [stage]

Targets:
  (none) / all     Run full regeneration for vk, vma, and xr
  build            Build the generator binary only
  vk               Regenerate Vulkan bindings
  vma              Regenerate VulkanMemoryAllocator bindings
  xr               Regenerate OpenXR bindings
  help             Show this help message

Stages (per target):
  (none)           Run all stages for the target
  docs             Generate documentation inputs (slow)
  reset            Remove old generated src, set up symlinks
  build            Build generator binary (shared across targets)
  generate         Run the code generator
  save             Persist results (git add, hpack)

Examples:
  ./scripts/regenerate.sh                    # full build (backward compatible)
  ./scripts/regenerate.sh vk                 # all stages for vk
  ./scripts/regenerate.sh vk docs            # single stage
  ./scripts/regenerate.sh vk generate        # run generator only (after docs+reset)
  ./scripts/regenerate.sh vma                # regenerate VMA only
  ./scripts/regenerate.sh build              # just build the generator
USAGE
}

ensure_generator() {
  if [ -z "$GENERATE" ]; then
    echo "Building generator"
    GENERATE=$(IN_NIX_SHELL='' nix-build -A generate-new)
  fi
}

################################################################
# Vulkan
################################################################

vk_docs() {
  echo "Cleaning Vulkan-Docs"
  git -C generate-new/Vulkan-Docs clean -dxf

  echo "Generating Vulkan-Docs documentation"
  (cd "$REPO_ROOT/generate-new/Vulkan-Docs" &&
    nix-shell -p $NIX_DOC_DEPS \
      --run "./makeAllExts refpages generated")
}

vk_reset() {
  echo "Cleaning src"
  git rm --quiet -r src/Vulkan src/Vulkan.hs
  mkdir -p src
  test -f generate-new/out || test -L generate-new/out || ln -s ../src generate-new/out
}

vk_generate() {
  ensure_generator
  echo "Generating vulkan"
  nix-shell -p asciidoctor --run "sh -c 'cd generate-new && \"$GENERATE/bin/vk\"'"
}

vk_save() {
  git add src
  nix-shell -p haskellPackages.hpack --run hpack
}

vk_all() {
  vk_docs
  vk_reset
  vk_generate
  vk_save
}

################################################################
# VMA
################################################################

vma_docs() {
  echo "Cleaning VulkanMemoryAllocator source"
  git -C VulkanMemoryAllocator/VulkanMemoryAllocator clean -dxf

  echo "Generating VMA documentation"
  (cd "$REPO_ROOT/VulkanMemoryAllocator/VulkanMemoryAllocator" &&
    sed -i -e 's|^GENERATE_DOCBOOK.*|GENERATE_DOCBOOK=YES|' \
           -e 's|^BRIEF_MEMBER_DESC.*|BRIEF_MEMBER_DESC=NO|' \
           -e 's|^PREDEFINED *=|PREDEFINED = VMA_STATS_STRING_ENABLED=1 |' \
           -e 's|^PREDEFINED *=|PREDEFINED = VMA_EXTENDS_VK_STRUCT(s)=s |' \
           -e 's|@CMAKE_SOURCE_DIR@/||' \
           Doxyfile &&
    nix-shell -p doxygen --run 'doxygen Doxyfile')

  echo "Generating Vulkan-Docs headers"
  (cd "$REPO_ROOT/generate-new/Vulkan-Docs/xml" &&
    nix-shell -p $NIX_DOC_DEPS \
      --run "make clean install codec_headers")
}

vma_reset() {
  echo "VMA has no reset step (overwrites in place)"
}

vma_generate() {
  ensure_generator
  echo "Generating VulkanMemoryAllocator"
  nix-shell -p vulkan-headers --run "sh -c 'cd generate-new && \"$GENERATE/bin/vma\"'"
}

vma_save() {
  nix-shell -p haskellPackages.hpack --run 'hpack VulkanMemoryAllocator'

  echo "Cleaning VMA documentation"
  git -C VulkanMemoryAllocator/VulkanMemoryAllocator clean -dxf
  git -C VulkanMemoryAllocator/VulkanMemoryAllocator checkout .
}

vma_all() {
  vma_docs
  vma_generate
  vma_save
}

################################################################
# OpenXR
################################################################

xr_docs() {
  echo "Cleaning OpenXR Docs"
  git -C generate-new/OpenXR-Docs clean -dxf

  echo "Generating OpenXR-Docs documentation"
  (cd "$REPO_ROOT/generate-new/OpenXR-Docs/specification" &&
    nix-shell -p $NIX_DOC_DEPS \
      --run "./makeAllExts man/apispec.txt generated")
}

xr_reset() {
  echo "Cleaning openxr src"
  git rm --quiet -r openxr/src/OpenXR openxr/src/OpenXR.hs
  mkdir -p openxr/src
  test -f generate-new/out-xr || test -L generate-new/out-xr || ln -s ../openxr/src generate-new/out-xr
}

xr_generate() {
  ensure_generator
  echo "Generating openxr"
  nix-shell -p asciidoctor --run "sh -c 'cd generate-new && \"$GENERATE/bin/xr\"'"
}

xr_save() {
  git add openxr/src
  nix-shell -p haskellPackages.hpack --run 'hpack openxr'
}

xr_all() {
  xr_docs
  xr_reset
  xr_generate
  xr_save
}

################################################################
# Main dispatch
################################################################

main() {
  local target="${1:-all}"
  local stage="${2:-}"

  case "$target" in
    help|-h|--help)
      usage
      ;;
    build)
      ensure_generator
      echo "Generator built at: $GENERATE"
      ;;
    all)
      vk_all
      vma_all
      xr_all
      ;;
    vk|vma|xr)
      if [ -z "$stage" ]; then
        "${target}_all"
      else
        case "$stage" in
          docs|reset|generate|save)
            "${target}_${stage}"
            ;;
          build)
            ensure_generator
            echo "Generator built at: $GENERATE"
            ;;
          *)
            echo "Unknown stage: $stage"
            echo "Valid stages: docs, reset, build, generate, save"
            exit 1
            ;;
        esac
      fi
      ;;
    *)
      echo "Unknown target: $target"
      usage
      exit 1
      ;;
  esac
}

main "$@"
