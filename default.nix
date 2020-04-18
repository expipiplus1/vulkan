{ pkgs ? import <nixpkgs> { }, compiler ? "ghc882"
, forShell ? pkgs.lib.inNixShell, hoogle ? forShell }:

let

  compiler' = if compiler != null then
    compiler
  else
    "ghc" + pkgs.lib.concatStrings
    (pkgs.lib.splitVersion pkgs.haskellPackages.ghc.version);

  targets = {
    vulkan = ./.;
    VulkanMemoryAllocator = ./VulkanMemoryAllocator;
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
          bytes = self.bytes_0_17;

          #
          # Generate
          #
          algebraic-graphs = dontCheck super.algebraic-graphs;
          first-class-families = doJailbreak super.first-class-families;
          inline-c = self.inline-c_0_9_0_0;
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
  tools = with pkgs; [ pkgconfig asciidoctor python3 doxygen glslang vulkan-validation-layers ];

  # Generate a haskell derivation using the cabal2nix tool on `package.yaml`
  makeDrv = name: src:
    let
      drv =
        haskellPackages.callCabal2nixWithOptions "" src "--flag=build-examples"
        ({ } // pkgs.lib.optionalAttrs (name == "vulkan") {
          vulkan = pkgs.vulkan-loader;
        } // pkgs.lib.optionalAttrs
          (name == "VulkanMemoryAllocator" && forShell) {
            # For the shell we don't want to have the compile the local dependency
            # for VMA
            vulkan = null;
          });
    in if name == "vulkan" then
      pkgs.haskell.lib.addExtraLibrary drv pkgs.vulkan-headers
    else
      drv;

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
