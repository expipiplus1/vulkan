{ pkgs ? import ../nix/nixpkgs.nix, compiler ? null
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell, withSwiftshader ? false
, buildProfiling ? false, buildInstrumented ? false, safeVulkanFFI ? false
, withOpenXR ? true }:

let
  haskellPackages = import ../nix/haskell-packages.nix {
    inherit pkgs compiler hoogle buildProfiling buildInstrumented safeVulkanFFI;
  };
  # Eww, some packages put things in etc and others in share. This ruins the
  # order, but ehh
  makeVkLayerPath = ps:
    pkgs.lib.concatStringsSep ":" [
      (pkgs.lib.makeSearchPathOutput "lib" "share/vulkan/explicit_layer.d" ps)
      (pkgs.lib.makeSearchPathOutput "lib" "etc/vulkan/explicit_layer.d" ps)
    ];
in if forShell then
  haskellPackages.shellFor ({
    packages = p: [ p.vulkan-examples ];
    buildInputs = with pkgs;
      [ shaderc ] ++ pkgs.lib.optional withOpenXR libglvnd
      ++ pkgs.lib.optional buildProfiling [
        haskellPackages.eventlog2html
        haskellPackages.hs-speedscope
        haskellPackages.opentelemetry-extra
        pkgs.tracy
        pkgs.gdb
        pkgs.linuxPackages.perf
      ];
    withHoogle = hoogle;
    VK_LAYER_PATH = with pkgs;
      makeVkLayerPath ([ vulkan-validation-layers vulkan-tools-lunarg ]
        ++ lib.optional withSwiftshader vulkan-extension-layer);
  } // pkgs.lib.optionalAttrs withSwiftshader {
    VK_ICD_FILENAMES =
      "${pkgs.swiftshader}/share/vulkan/icd.d/vk_swiftshader_icd.json";
    VK_INSTANCE_LAYERS = "VK_LAYER_KHRONOS_timeline_semaphore";
  })
else
  haskellPackages.vulkan-examples
