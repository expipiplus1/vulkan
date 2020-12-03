# Haskell package overrides

{ pkgs, hoogle, buildProfiling ? false, buildInstrumented ? false }:

with pkgs.haskell.lib;

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure ../.gitignore;
  aggressiveFilter = builtins.filterSource (path: _type:
    (pkgs.lib.any (x: baseNameOf path == x) [
      "package.yaml"
      "changelog.md"
      "readme.md"
    ]) || pkgs.lib.hasInfix "/src" path || pkgs.lib.hasInfix "/src-manual" path
    || pkgs.lib.hasInfix "/vk" path || pkgs.lib.hasInfix "/vma" path);

  mod = if buildProfiling then
    drv: doHaddock (enableLibraryProfiling drv)
  else
    drv: doHaddock (disableLibraryProfiling drv);

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
    cabal2nixOptions = if buildInstrumented then "--flag=trace-calls" else "";
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
      addExtraLibrary (addBuildTools (mod drv) [ pkgs.glslang pkgs.shaderc ])
      pkgs.renderdoc;
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
  derive-storable = self.callHackageDirect {
    pkg = "derive-storable";
    ver = "0.3.0.0";
    sha256 = "0h9v6k1651acsqs64mkidqrflld7ghhbiir7z9f0wm8vrqwc6wyp";
  } { };
  derive-storable-plugin = appendPatch (doJailbreak (self.callHackageDirect {
    pkg = "derive-storable-plugin";
    ver = "0.2.3.1";
    sha256 = "0iibzdjlx2v5caib41a3i5l67dplwwhp8sys3hfc6m3lyhghzg16";
  } { })) (pkgs.fetchpatch {
    url = "https://github.com/mkloczko/derive-storable-plugin/pull/4.patch";
    name = "pure.patch";
    sha256 = "11fm91062slgh25na3pmjpf2sn9z1gg9lg5jr4nv2q8a2bzg32zs";
  });

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
