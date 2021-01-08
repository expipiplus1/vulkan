{ pkgs ? import ./nix/nixpkgs.nix, compiler ? null
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

let
  haskellPackages =
    import ./nix/haskell-packages.nix { inherit pkgs compiler hoogle; };

  tools = with pkgs; [
    pkgconfig
    asciidoctor
    python3
    doxygen
    vulkan-validation-layers
  ];

  packages = p:
    with p;
    [ vulkan vulkan-utils VulkanMemoryAllocator vulkan-examples openxr ]
    ++ pkgs.lib.optional (p.ghc.version == "8.10.3") generate-new;

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
