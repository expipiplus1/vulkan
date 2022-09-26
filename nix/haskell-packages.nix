# Haskell package overrides
{ pkgs ? import ./nixpkgs.nix, compiler ? null, hoogle ? false
, safeVulkanFFI ? false, safeOpenXrFFI ? false, buildProfiling ? false
, buildInstrumented ? false, openxrNoVulkan ? false }:

with pkgs.haskell.lib.compose;

let
  gitignore = pkgs.nix-gitignore.gitignoreSourcePure ../.gitignore;
  aggressiveFilter = builtins.filterSource (path: _type:
    (pkgs.lib.any (x: baseNameOf path == x) [
      "package.yaml"
      "changelog.md"
      "readme.md"
    ]) || pkgs.lib.any (i: pkgs.lib.hasInfix i path) [
      "/src"
      "/src-manual"
      "/test"
      "/vk"
      "/xr"
      "/khronos-spec"
      "/vma"
    ]);
  mod = if buildProfiling then
    drv: doHaddock (enableLibraryProfiling drv)
  else
    drv: doHaddock (disableLibraryProfiling drv);

  baseHaskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  overrides = self: super:
    {
      #
      # Our packages
      #
      vulkan = self.developPackage {
        name = "vulkan";
        root = aggressiveFilter ../.;
        overrides = _: _: { vulkan = pkgs.vulkan-loader; };
        modifier = mod;
        returnShellEnv = false;
        cabal2nixOptions = with pkgs.lib;
          concatStringsSep " "
          (optional safeVulkanFFI "--flag=safe-foreign-calls"
            ++ optional buildInstrumented "--flag=trace-calls");
      };
      vulkan-utils = self.developPackage {
        name = "vulkan-utils";
        root = gitignore ../utils;
        modifier = drv: addExtraLibrary pkgs.vulkan-headers (mod drv);
        returnShellEnv = false;
      };
      VulkanMemoryAllocator = self.developPackage {
        name = "VukanMemoryAllocator";
        root = gitignore ../VulkanMemoryAllocator;
        modifier = drv: addExtraLibrary pkgs.vulkan-headers (mod drv);
        returnShellEnv = false;
      };
      vulkan-examples = self.developPackage {
        name = "vulkan-examples";
        root = gitignore ../examples;
        modifier = drv:
          addExtraLibrary pkgs.renderdoc
          (addBuildTools [ pkgs.glslang pkgs.shaderc ] (mod drv));
        returnShellEnv = false;
        cabal2nixOptions = "--flag=renderdoc";
      };
      generate-new = self.developPackage {
        name = "generate-new";
        root = aggressiveFilter ../generate-new;
        modifier = drv: dontHaddock (mod drv);
        returnShellEnv = false;
      };
      openxr = self.developPackage {
        name = "openxr";
        root = aggressiveFilter ../openxr;
        overrides = _: _: { openxr_loader = pkgs.openxr-loader; };
        modifier = mod;
        returnShellEnv = false;
        cabal2nixOptions = with pkgs.lib;
          concatStringsSep " "
          (optional openxrNoVulkan "--flag=-use-vulkan-types"
            ++ optional safeOpenXrFFI "--flag=safe-foreign-calls"
            ++ optional buildInstrumented "--flag=trace-calls");
      };

      #
      # Overrides for examples
      #
      # profiling
      eventlog2html = markUnbroken (overrideSrc {
        src = pkgs.fetchFromGitHub {
          owner = "expipiplus1";
          repo = "eventlog2html";
          rev = "3612e7000cfbb1498349c331b5adaa2d17f02206"; # ellie-size
          sha256 = "0s2wxqwmaldqyz9yz52wxy0dla9pahqlpq6cx4pm4c744ggmpswd";
        };
      } super.eventlog2html);
      hs-speedscope = markUnbroken (addBuildDepend self.machines (overrideSrc {
        src = pkgs.fetchFromGitHub {
          owner = "mpickering";
          repo = "hs-speedscope";
          rev = "5a77aeb163e07dc63769d4e0a659e5821cdee527";
          sha256 = "0q9kdlxhm37f260v4ydmznwmmsaa4w9mq3fh2iivj792y6ybmp5j";
        };
      } super.hs-speedscope));

      autoapply = doJailbreak super.autoapply;
      sdl2 = doJailbreak super.sdl2;

      #
      # Overrides for generate
      #
      pandoc = appendPatch ../generate-new/patches/pandoc-haddock-tables.patch
        super.pandoc;
    } // pkgs.lib.optionalAttrs hoogle {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = super.ghcWithPackages.override { withHoogle = true; };
    };

in baseHaskellPackages.override { inherit overrides; }
