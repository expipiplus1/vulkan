## Generating sources

- Regenerate source for `vulkan` and `VulkanMemoryAllocator`
- If there are any changes commit those before proceeding

## Versions

- Bump version on `vulkan` in `package.yaml`
- Bump version on `VulkanMemoryAllocator`
- Bump version of `vulkan` dependency in `VulkanMemoryAllocator`
- Run hpack in
  - `.`
  - `./VulkanMemoryAllocator`

- Make sure changelogs are up to date
  - `./changelog.md`
  - `./VulkanMemoryAllocator/changelog.md`

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

## gh-pages documentation

- Run `./gen-standalone-haddocks.sh` script pointing to a worktree on the gh-pages branch
  - Make sure to clean that directory first
