{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

# Strip out the irrelevant parts of the source
let src = with pkgs.lib;
          let p = n: (toString ./generate) == n || (toString ./dist) == n;
          in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};

    extraEnvPackages = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.hscolour
    ];

    drv =
      pkgs.haskellPackages.callCabal2nix
        "vulkan"
        src
        { vulkan = pkgs.vulkan-loader; };

    envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
      buildInputs = attrs.buildInputs ++ extraEnvPackages;
    });

in
  drv // { env = envWithExtras; }
