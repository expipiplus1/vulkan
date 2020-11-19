{ pkgs ? import ../nix/nixpkgs.nix, compiler ? null
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell, withSwiftshader ? false }:

let
  haskellPackages = let
    hp = if compiler == null then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};
  in hp.override {
    overrides = import ../nix/haskell-packages.nix { inherit pkgs hoogle; };
  };

in if forShell then
  haskellPackages.shellFor ({
    packages = p: [ p.vulkan-examples ];
    buildInputs = with pkgs; [ vulkan-validation-layers ];
    withHoogle = hoogle;
  } // pkgs.lib.optionalAttrs withSwiftshader {
    VK_ICD_FILENAMES =
      "${pkgs.swiftshader}/share/vulkan/icd.d/vk_swiftshader_icd.json";
  })
else
  haskellPackages.vulkan-examples
