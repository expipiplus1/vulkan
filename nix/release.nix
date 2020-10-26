{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc884" }:

let
  vulkanPkgs = import ../default.nix {
    inherit pkgs compiler;
    hoogle = false;
    forShell = false;
  };

  docDrv = _name: drv:
    (pkgs.haskell.lib.overrideCabal drv (drv: {
      doHaddock = true;
      haddockFlags = [ "--for-hackage" ];
      postHaddock = ''
        mkdir -p "$doc"
        tar czf "$doc/${drv.pname}-${drv.version}-docs.tar.gz" -C dist/doc/html "${drv.pname}-${drv.version}-docs"
      '';
    })).doc;

  tarballDrv = _name: pkgs.haskell.lib.sdistTarball;

  sdistTestDrv = _name: pkgs.haskell.lib.buildFromSdist;

in with pkgs.lib;
mapAttrs tarballDrv vulkanPkgs // {
  docs = mapAttrs docDrv
    (filterAttrs (n: _: n != "generate-new" && n != "vulkan-examples")
      vulkanPkgs);
  sdistTest = mapAttrs sdistTestDrv vulkanPkgs;
}
