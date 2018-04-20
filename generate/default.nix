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
      pandoc = pkgs.haskell.lib.appendPatch
                 super.pandoc
                 ./pandoc-math.patch;
    };
  };

  drv = haskellPackages.callCabal2nix "generate" src {};

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
