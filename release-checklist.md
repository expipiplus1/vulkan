## Generating sources

- Regenerate source for `vulkan` and `VulkanMemoryAllocator`
  - Clean submodules and regenerate documentation first to ensure it's up to
    date.
  - `./scripts/regenerate.sh` will do this for you
- If there are any changes commit those before proceeding

## Versions

- Bump versions
  - Bump version on `vulkan` in `package.yaml`
  - Bump version on `VulkanMemoryAllocator`
  - Bump version on `utils`
  - Bump version on `openxr`
- Bump dependency versions
  - Bump version bounds of `vulkan` dependency in `VulkanMemoryAllocator`
  - Bump version bounds of `vulkan` dependency in `utils`
  - Bump version bounds of `vulkan` dependency in `openxr`
- Run hpack in
  - `.`
  - `./VulkanMemoryAllocator`
  - `./utils`
  - `./openxr`

- Make sure changelogs are up to date
  - `./changelog.md`
  - `./VulkanMemoryAllocator/changelog.md`
  - `./utils/changelog.md`
  - `./openxr/changelog.md`

## vulkan

- Build documentation (So doc building for VMA works correctly)
  - `cabal haddock --haddock-option="--hyperlinked-source"`
- Build documentation for Hackage
  - `cabal haddock  --haddock-for-hackage --haddock-option="--hyperlinked-source"`
- sdist
- unpack sdist elsewhere
- build sdl-triangle
  - Observe it running

## VulkanMemoryAllocator

- Build documentation
  - `cabal haddock  --haddock-for-hackage --haddock-option="--hyperlinked-source" VulkanMemoryAllocator`
- sdist
- unpack sdist elsewhere
- build

## Tag git revision

This is done automatically on merge to the main branch

## gh-pages documentation

(on my machine)

```bash
rm -rf ../vulkan-docs/*
cp -r $(nix-build --no-link nix/release.nix -A docs-combined)/* ../vulkan-docs
cd ../vulkan-docs
git add .
git commit -m 'vXXX'
git push
```

## Upload to Hackage

This will upload the release artifacts:

```bash
tag=v3.8
hub release download "refs/tags/$tag" |
cut -d' ' -f2 |
sort -r |
while read f; do
  if [[ "$f" =~ "-docs.tar.gz" ]]; then
    cabal upload --publish --doc "$f"
  else
    cabal upload --publish "$f"
  fi
  rm "$f"
done
```
