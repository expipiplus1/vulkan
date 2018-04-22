{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let
  # Strip out the irrelevant parts of the source
  src = with pkgs.lib;
        let p = n: toString ./dist == n;
        in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};

  extraEnvPackages = with pkgs; [
    asciidoctor
    python3
  ];

  haskellPackages = pkgs.haskell.packages.${compiler}.override{
    overrides = self: super: {
      pandoc = pkgs.haskell.lib.appendPatches
                 super.pandoc
                 [ ./patches/pandoc-math.patch
                   ./patches/pandoc-haddock-math.patch
                   ./patches/pandoc-haddock-table.patch
                 ];
      async-pool = pkgs.haskell.lib.appendPatches
                 super.async-pool
                 [ ./patches/async-pool-bounds.patch
                 ];
    };
  };

  drv = haskellPackages.callCabal2nix "generate" src {};

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
