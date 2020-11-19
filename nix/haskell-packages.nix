# Haskell package overrides

{ pkgs, hoogle }:

with pkgs.haskell.lib;

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure ../.gitignore;
  aggressiveFilter = builtins.filterSource (path: _type:
    (pkgs.lib.any (x: baseNameOf path == x) [
      "package.yaml"
      "changelog.md"
      "readme.md"
    ]) || pkgs.lib.hasInfix "/src" path || pkgs.lib.hasInfix "/vk" path
    || pkgs.lib.hasInfix "/vma" path);

  mod = drv: doHaddock (disableLibraryProfiling drv);

in self: super:
{
  #
  # Our packages
  #
  vulkan = self.developPackage {
    name = "vulkan";
    root = aggressiveFilter ../.;
    modifier = drv: (mod drv).override { vulkan = pkgs.vulkan-loader; };
    returnShellEnv = false;
  };
  vulkan-utils = self.developPackage {
    name = "vulkan-utils";
    root = gitignore ../utils;
    modifier = drv: addExtraLibrary (mod drv) pkgs.vulkan-headers;
    returnShellEnv = false;
  };
  VulkanMemoryAllocator = self.developPackage {
    name = "VukanMemoryAllocator";
    root = gitignore ../VulkanMemoryAllocator;
    modifier = drv: addExtraLibrary (mod drv) pkgs.vulkan-headers;
    returnShellEnv = false;
  };
  vulkan-examples = self.developPackage {
    name = "vulkan-examples";
    root = gitignore ../examples;
    modifier = drv:
      addExtraLibrary (addBuildTool (mod drv) pkgs.glslang) pkgs.renderdoc;
    returnShellEnv = false;
    cabal2nixOptions = "--flag=renderdoc";
  };
  generate-new = self.developPackage {
    name = "generate-new";
    root = aggressiveFilter ../generate-new;
    modifier = drv: dontHaddock (mod drv);
    returnShellEnv = false;
  };

  #
  # Overrides for examples
  #
  pretty-simple = self.pretty-simple_4_0_0_0;
  prettyprinter = self.prettyprinter_1_7_0;

  #
  # Overrides for generate
  #
  pandoc = appendPatch super.pandoc
    ../generate-new/patches/pandoc-haddock-tables.patch;
  language-c = self.language-c_0_9;
} // pkgs.lib.optionalAttrs hoogle {
  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
  ghcWithPackages = self.ghc.withPackages;
}
