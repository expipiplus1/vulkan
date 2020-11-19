{ pkgs ? import ../nix/nixpkgs.nix, compiler ? null
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

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
  haskellPackages.shellFor {
    packages = p: [ p.vulkan-examples ];
    buildInputs = with pkgs; [ vulkan-validation-layers ];
    withHoogle = hoogle;
  }
else
  haskellPackages.vulkan-examples
