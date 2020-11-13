{ pkgs ? import ./nix/nixpkgs.nix, compiler ? "ghc884"
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

let
  haskellPackages = let
    hp = if compiler == null then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};
  in hp.override {
    overrides = import ./nix/haskell-packages.nix { inherit pkgs hoogle; };
  };

  tools = with pkgs; [
    pkgconfig
    asciidoctor
    python3
    doxygen
    vulkan-validation-layers
  ];

  packages = p:
    with p;
    [ vulkan vulkan-utils VulkanMemoryAllocator vulkan-examples ]
    ++ pkgs.lib.optional (p.ghc.version == "8.8.4") generate-new;

in if forShell then
  haskellPackages.shellFor {
    inherit packages;
    buildInputs = tools;
    withHoogle = hoogle;
  }
else
  pkgs.lib.listToAttrs (builtins.map (value: {
    inherit value;
    name = value.pname;
  }) (packages haskellPackages))
