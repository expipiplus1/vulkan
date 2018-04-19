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

  drv = pkgs.haskellPackages.callCabal2nix "generate" src {};

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
