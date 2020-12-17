{ pkgs ? import ../nix/nixpkgs.nix, compiler ? null
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell, withSwiftshader ? false
, buildProfiling ? false, buildInstrumented ? false, safeVulkanFFI ? false
, withOpenXR ? true }:

let
  haskellPackages = import ../nix/haskell-packages.nix {
    inherit pkgs compiler hoogle buildProfiling buildInstrumented safeVulkanFFI;
  };
in if forShell then
  haskellPackages.shellFor ({
    packages = p: [ p.vulkan-examples ];
    buildInputs = with pkgs;
      [ vulkan-tools-lunarg vulkan-validation-layers shaderc ]
      ++ pkgs.lib.optional withOpenXR monado
      ++ pkgs.lib.optional withSwiftshader vulkan-extension-layer
      ++ pkgs.lib.optional buildProfiling [
        haskellPackages.eventlog2html
        haskellPackages.hs-speedscope
        haskellPackages.opentelemetry-extra
        pkgs.tracy
        pkgs.gdb
        pkgs.linuxPackages.perf
      ];
    withHoogle = hoogle;
  } // pkgs.lib.optionalAttrs withSwiftshader {
    VK_ICD_FILENAMES =
      "${pkgs.swiftshader}/share/vulkan/icd.d/vk_swiftshader_icd.json";
    VK_INSTANCE_LAYERS = "VK_LAYER_KHRONOS_timeline_semaphore";
  })
else
  haskellPackages.vulkan-examples
