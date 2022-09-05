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

  generator-ghc-version = "8.10.7";

  packages = p:
    with p;
    if compiler == "ghcHEAD" then [
      (pkgs.haskell.lib.dontCheck vulkan)
      (pkgs.haskell.lib.dontCheck VulkanMemoryAllocator)
    ] else
      [ vulkan vulkan-utils VulkanMemoryAllocator vulkan-examples openxr ]
      ++ pkgs.lib.optional (p.ghc.version == generator-ghc-version) generate-new;

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
