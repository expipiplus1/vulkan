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
      vulkan-utils-spirv = self.developPackage {
        name = "vulkan-utils-spirv";
        root = gitignore ../utils-spirv;
        # tests compile GLSL fixtures with the glslang quasiquoters
        modifier = drv: addBuildTools [ pkgs.glslang ] (mod drv);
        returnShellEnv = false;
      };
      vulkan-utils-framegraph = self.developPackage {
        name = "vulkan-utils-framegraph";
        root = gitignore ../utils-framegraph;
        modifier = mod;
        returnShellEnv = false;
      };
      vulkan-init-sdl2 = self.developPackage {
        name = "vulkan-init-sdl2";
        root = gitignore ../utils-init/vulkan-init-sdl2;
        modifier = mod;
        returnShellEnv = false;
      };
      vulkan-init-glfw = self.developPackage {
        name = "vulkan-init-glfw";
        root = gitignore ../utils-init/vulkan-init-glfw;
        modifier = mod;
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
          dontCheck
          (addExtraLibrary pkgs.renderdoc
            (addBuildTools [ pkgs.glslang pkgs.shaderc ] (mod drv)));
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
      hs-speedscope = markUnbroken (addBuildDepend self.machines (overrideSrc {
        src = pkgs.fetchFromGitHub {
          owner = "mpickering";
          repo = "hs-speedscope";
          rev = "5a77aeb163e07dc63769d4e0a659e5821cdee527";
          sha256 = "0q9kdlxhm37f260v4ydmznwmmsaa4w9mq3fh2iivj792y6ybmp5j";
        };
      } super.hs-speedscope));
      opentelemetry-extra = markUnbroken (dontCheck super.opentelemetry-extra);

      binary-orphans = addBuildDepend self.OneTuple super.binary-orphans;

      #
      # Dependencies for vulkan-utils-spirv, absent or too old in nixpkgs
      #
      # dontCheck: hedgehog property gives up on discards
      geomancy = dontCheck (self.callHackageDirect {
        pkg = "geomancy";
        ver = "0.3.1.0";
        sha256 = "0l9qkh958xwpv31wzddz4a25i57vbcr1q1b0jgrbwz72gmhqqlyp";
      } { });
      fragr = self.callHackageDirect {
        pkg = "fragr";
        ver = "0.1.0.0";
        sha256 = "18fpxbd4ghcnillzdn3xfj6xl3dmvgvww1bzkhaj6wifg6nlzfvs";
      } { };
      gl-block = self.callHackageDirect {
        pkg = "gl-block";
        ver = "1.1";
        sha256 = "0d6gi2hi96r7yzwg7vmrz936d2wmn88qs09pqhzk1ph7qwjbaqx0";
      } { };
      spirv-enum = self.callHackageDirect {
        pkg = "spirv-enum";
        ver = "1.104.350.0";
        sha256 = "0pmj7a0sqvx2nlk7fl1yqlljxdrf91l0rzaa42fwjb39qg9hxzcf";
      } { };
      spirv-reflect-ffi = self.callHackageDirect {
        pkg = "spirv-reflect-ffi";
        ver = "0.4.1";
        sha256 = "0f5j6ddrx6z2cq74sq8y0yh743qjy4nip0sm0gy7jm9cd433ik2k";
      } { };
      spirv-reflect-types = self.callHackageDirect {
        pkg = "spirv-reflect-types";
        ver = "0.4";
        sha256 = "1v5bip44jmzmcnlhjf8ckx8zz05f7fsx4d2idlkqs6q09zd7cb6n";
      } { };
      # dontCheck: test suite wants base >= 4.18
      webcolor-labels = dontCheck (self.callHackageDirect {
        pkg = "webcolor-labels";
        ver = "0.1.0.0";
        sha256 = "1cwjz31xiya9wmy0h0srn4f6brd4xh4hi0rh5ry80vkwqz7f3fhm";
      } { });

      #
      # Dependencies for the glitch-post example, absent or too old in nixpkgs
      #
      pl-synth = self.callHackageDirect {
        pkg = "pl-synth";
        ver = "0.1.0.0";
        sha256 = "0aqcwyxksc6wkinz3c7q5jgfv70hl12q6980sw8h9kxq66yf4xdq";
      } { };
      openal-ffi = self.callHackageDirect {
        pkg = "openal-ffi";
        ver = "0.0.2";
        sha256 = "0la6c1hdxljk4rcj87n1dx7lp4s47va3gx9wjgbn42jl6iwqsr17";
      } { };

      #
      # Overrides for generate
      #
      pandoc = appendPatch ../generate-new/patches/pandoc-haddock-tables.patch
        super.pandoc;
      fourmolu = doJailbreak super.fourmolu;
    } // pkgs.lib.optionalAttrs hoogle {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = super.ghcWithPackages.override { withHoogle = true; };
    };

in baseHaskellPackages.override { inherit overrides; }
