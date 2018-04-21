{ pkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

# Strip out the irrelevant parts of the source
let src = with pkgs.lib;
          let p = n: (toString ./generate) == n || (toString ./dist) == n;
          in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};

    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        vector-sized = self.vector-sized_1_0_0_0;

        vector-sized_1_0_0_0 = super.vector-sized_1_0_0_0.override {
          indexed-list-literals = self.indexed-list-literals_0_2_0_0;
        };
      };
    };

    extraEnvPackages = [
      haskellPackages.cabal-install
      haskellPackages.hscolour
    ];

    drv =
      haskellPackages.callCabal2nix
        "vulkan"
        src
        { vulkan = pkgs.vulkan-loader; };

    envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
      buildInputs = attrs.buildInputs ++ extraEnvPackages;
    });

in
  drv // { env = envWithExtras; }
