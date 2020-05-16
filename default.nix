{ nixpkgsSrc ? builtins.fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/33dd04ea4afa2e7c1a84bdc48e8cf0bd9923e882.tar.gz"
, pkgs ? import nixpkgsSrc { }, compiler ? "ghc882"
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

let

  compiler' = if compiler != null then
    compiler
  else
    "ghc" + pkgs.lib.concatStrings
    (pkgs.lib.splitVersion pkgs.haskellPackages.ghc.version);

  targets = let
    srcFilter = path: type:
      (baseNameOf path == "package.yaml") || pkgs.lib.hasInfix "/src" path;
    filter = builtins.filterSource srcFilter;
  in {
    vulkan = filter ./.;
    vulkan-utils = ./utils;
    vulkan-examples = ./examples;
    VulkanMemoryAllocator = ./VulkanMemoryAllocator;
  } // pkgs.lib.optionalAttrs (compiler == "ghc882") {
    generate-new = ./generate-new;
  };

  # Any overrides we require to the specified haskell package set
  haskellPackages = with pkgs.haskell.lib;
    pkgs.haskell.packages.${compiler'}.override {
      overrides = self: super:
        (pkgs.lib.mapAttrs (n: v: makeDrv n v) targets) // {
          #
          # Examples
          #
          sdl2 = overrideSrc super.sdl2 {
            src = pkgs.fetchFromGitHub {
              owner = "haskell-game";
              repo = "sdl2";
              rev = "d10b2ae86ce3db58c5c011cbec6eccf69c2fd2f1";
              sha256 = "1qfjfrzc9yjg8ibgr0a7fly6fnd1f2yv731n7h1wjgz9vaa3q6wg";
            };
          };
          th-desugar = self.callCabal2nix "" (pkgs.fetchFromGitHub {
            owner = "goldfirere";
            repo = "th-desugar";
            rev = "f075206882ce4e554c37537e624b4be7409d74a3";
            sha256 = "0747xggx2q8yphag2wv06dj0pgi9zvadi069c2d6lckg26chhnlk";
          }) { };
          autoapply = doHaddock (self.callCabal2nix "" (pkgs.fetchFromGitHub {
            owner = "expipiplus1";
            repo = "autoapply";
            rev = "4ff481b28c9f2b081496593bba491633873ca155";
            sha256 = "0hfcx9mnan0f5h5x8qvpzybbvn6brmia7s2wfgk8j61arghfwg8k";
          }) { });

          #
          # Generate
          #
          algebraic-graphs = dontCheck super.algebraic-graphs;
          first-class-families = doJailbreak super.first-class-families;
          dependent-sum = self.callCabal2nix "" ((pkgs.fetchFromGitHub {
            owner = "obsidiansystems";
            repo = "dependent-sum";
            rev = "73ab6cb23331f463c384290b4a1be542e68b323d";
            sha256 = "18h0k0n05spsk5jvgmphv3sj4wdwb3qdxklfkn5wq8kni0bqabzk";
          }) + "/dependent-sum") { };
          dependent-map = self.callCabal2nix "" (pkgs.fetchFromGitHub {
            owner = "obsidiansystems";
            repo = "dependent-map";
            rev = "26677886eced970d661a5a7356ba4fe221c0324c";
            sha256 = "1865yqnxzlrkmbag4xn47csgagmk968z4n633sk2c75d48icyzf9";
          }) { };
          polysemy-plugin = self.callCabal2nix "" ((pkgs.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "72dc96fbd13dba6d8e9767253b7298e00a781bee";
            sha256 = "09b1n71gjmhf4ggx2wlywxm11jl3qbmhnlmmchj8pyy3hczl6hb5";
          } + "/polysemy-plugin")) { };
          polysemy = self.callCabal2nix "" ((pkgs.fetchFromGitHub {
            owner = "polysemy-research";
            repo = "polysemy";
            rev = "72dc96fbd13dba6d8e9767253b7298e00a781bee";
            sha256 = "09b1n71gjmhf4ggx2wlywxm11jl3qbmhnlmmchj8pyy3hczl6hb5";
          })) { };
          polysemy-zoo = dontCheck (self.callCabal2nix ""
            ((pkgs.fetchFromGitHub {
              owner = "polysemy-research";
              repo = "polysemy-zoo";
              rev = "57c6012e196db7fe1ce7551f1f762cbddc71f095";
              sha256 = "18smd2c66gdn9585sdkn60ykvdvkbvkxrnnl9zix687dca6h9jw0";
            })) { });
          compact = doJailbreak super.compact;
          pandoc = appendPatch super.pandoc
            ./generate-new/patches/pandoc-haddock-tables.patch;
          language-c = appendPatches super.language-c [
            ./generate-new/patches/language-c-custom-state.patch
            ./generate-new/patches/language-c-align.patch
            ./generate-new/patches/language-c-show-type.patch
          ];
        } // pkgs.lib.optionalAttrs hoogle {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = p:
            self.ghc.withPackages
            (f: p f ++ (if forShell then [ f.process ] else [ ]));
        };
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
      drv =
        haskellPackages.callCabal2nixWithOptions "" src "--flag=build-examples"
        ({ } // pkgs.lib.optionalAttrs (name == "vulkan") {
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
      else
        drv;
    in disableLibraryProfiling drv';

  addHoogleDatabase = drv:
    if hoogle then
      drv.overrideAttrs (attrs: {
        shellHook = attrs.shellHook + ''
          export HIE_HOOGLE_DATABASE="$(cat $(${pkgs.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
        '';
      })
    else
      drv;

in if forShell then
  addHoogleDatabase (haskellPackages.shellFor {
    packages = _: packages;
    buildInputs = tools;
    withHoogle = hoogle;
  })
else
  buildSet
