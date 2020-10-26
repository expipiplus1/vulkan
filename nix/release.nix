{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc884" }:

let
  vulkanPkgs = import ../default.nix {
    inherit pkgs compiler;
    hoogle = false;
    forShell = false;
  };

  docDrv = _name: drv:
    pkgs.haskell.lib.overrideCabal drv (drv: {
      doHaddock = true;
      haddockFlags = [ "--for-hackage" ];
      postHaddock = ''
        mkdir -p "$doc"
        tar czf "$doc/${drv.pname}-${drv.version}-docs.tar.gz" -C dist/doc/html "${drv.pname}-${drv.version}-docs"
      '';
    });

  tarballDrv = _name: pkgs.haskell.lib.sdistTarball;

in with pkgs.lib;
mapAttrs tarballDrv vulkanPkgs // {
  doc = mapAttrs docDrv vulkanPkgs;
}
