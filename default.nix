{ pkgs ? import ./nix/nixpkgs.nix, compiler ? "ghc884"
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

let

  compiler' = if compiler != null then
    compiler
  else
    "ghc" + pkgs.lib.concatStrings
    (pkgs.lib.splitVersion pkgs.haskellPackages.ghc.version);

  targets = let
    srcFilter = path: _type:
      (pkgs.lib.any (x: baseNameOf path == x) [
        "package.yaml"
        "changelog.md"
        "readme.md"
      ]) || pkgs.lib.hasInfix "/src" path || pkgs.lib.hasInfix "/vk" path
      || pkgs.lib.hasInfix "/vma" path;
    filter = builtins.filterSource srcFilter;
  in {
    vulkan = filter ./.;
    vulkan-utils = ./utils;
    vulkan-examples = ./examples;
    VulkanMemoryAllocator = ./VulkanMemoryAllocator;
  } // pkgs.lib.optionalAttrs (compiler == "ghc884") {
    generate-new = filter ./generate-new;
  };

  # Any overrides we require to the specified haskell package set
  haskellPackages = with pkgs.haskell.lib;
    pkgs.haskell.packages.${compiler'}.override {
      overrides = self: super:
        (pkgs.lib.mapAttrs makeDrv targets)
        // import ./nix/haskell-packages.nix { inherit pkgs hoogle; } self
        super;
    };

  buildSet = pkgs.lib.foldl (ps: p: ps // { ${p.pname} = p; }) { } packages;
  packages = map (t: haskellPackages.${t}) (builtins.attrNames targets);
  tools = with pkgs; [
    pkgconfig
    asciidoctor
    python3
    doxygen
    vulkan-validation-layers
  ];

  # Generate a haskell derivation using the cabal2nix tool on `package.yaml`
  makeDrv = name: src:
    with pkgs.haskell.lib;
    let
      drv = haskellPackages.callCabal2nix "" src ({ }
        // pkgs.lib.optionalAttrs (name == "vulkan") {
          vulkan = pkgs.vulkan-loader;
        } // pkgs.lib.optionalAttrs ((name == "vulkan-examples" || name
          == "vulkan-utils" || name == "VulkanMemoryAllocator") && forShell) {
            # For the shell we don't want to have the compile the local dependency
            # for VMA
            vulkan = null;
          } // pkgs.lib.optionalAttrs (name == "vulkan-examples" && forShell) {
            # For the shell we don't want to have the compile the local dependency
            # for VMA
            vulkan-utils = null;
            VulkanMemoryAllocator = null;
          });
      drv' = if name == "vulkan-examples" then
        addBuildTool drv pkgs.glslang
      else if name == "VulkanMemoryAllocator" then
        addExtraLibrary drv pkgs.vulkan-headers
      else if name == "vulkan-utils" then
        addExtraLibrary drv pkgs.vulkan-headers
      else if name == "generate-new" then
        dontHaddock drv
      else
        drv;
    in disableLibraryProfiling drv';

in if forShell then
  haskellPackages.shellFor {
    packages = _: packages;
    buildInputs = tools;
    withHoogle = hoogle;
  }
else
  buildSet
