{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc822.override{
    overrides =
      let overrideAttrs = package: newAttrs: package.override (args: args // {
              mkDerivation = expr: args.mkDerivation (expr // newAttrs);
            });
      in self: super: {
        };
      };

  haskellPackageGen = { doFilter ? true }: src:
    let filteredSrc = builtins.filterSource (n: t: t != "unknown") src;
        package = pkgs.runCommand "default.nix" {} ''
          ${pkgs.haskellPackages.cabal2nix}/bin/cabal2nix \
            ${if doFilter then filteredSrc else src} \
            > "$out"
        '';
    in import package;

  drv = haskellPackages.callPackage (haskellPackageGen {} ./.) {};

  extraEnvPackages = with haskellPackages; [];

  envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
    buildInputs = attrs.buildInputs ++ extraEnvPackages;
  });

in drv // { env = envWithExtras; }
