{ pkgs ? import <nixpkgs> {} }:

let
  # Strip out the irrelevant parts of the source
  src = with pkgs.lib;
        let p = n: toString ./dist == n;
        in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};

  extraEnvPackages = with pkgs; [
    asciidoctor
    python3
  ];

  haskellPackages = pkgs.haskellPackages.override{
    overrides = self: super: {
      pandoc = pkgs.haskell.lib.appendPatches
                 super.pandoc_2_1_2
                 [ ./pandoc-patches/pandoc-math.patch
                   ./pandoc-patches/pandoc-haddock-math.patch
                   ./pandoc-patches/pandoc-haddock-table.patch
                 ];
    };
  };

  drv = haskellPackages.callCabal2nix "generate" src {};

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
