{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc884" }:
# Use old GHC until
# https://github.com/haskell-infra/hackage-trustees/issues/276 is fixed

with pkgs.haskell.lib;
with pkgs.lib;

let
  #
  # Utils
  #

  haskellPackages = let
    hp = if compiler == null then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};
  in hp.override {
    overrides = import ./haskell-packages.nix {
      inherit pkgs;
      hoogle = false;
    };
  };

  haddockInterface = pkg:
    overrideDerivation pkg (drv: {
      name = "${drv.name}-haddock-interface";
      # Disable the "doc" output here.
      outputs = [ "out" ];
      buildPhase = ''
        ./Setup haddock --haddock-options="--dump-interface=haddock-interface"
      '';
      haddockPhase = ":";
      checkPhase = ":";
      installPhase = ''
        mkdir -p "$out"
        mv haddock-interface "$out/"
      '';
    });

  #
  # Generate a
  #
  combineDocs = ps:
    pkgs.runCommandNoCC "docs-combined" {
      nativeBuildInputs = with pkgs; [ fd haskellPackages.ghc ];
    } ''
      for p in ${
        concatStringsSep " "
        (map (p: "${documentationTarball p}/${p.name}-docs.tar.gz") ps)
      }; do
        tar xzv < "$p"
      done;

      remote=https://hackage.haskell.org/package/

      printf "Making any links to local packages relative\n"
      fd 'html$' --type f --exec \
        sed -E -i 's|<a href="/package/(${
          concatStringsSep "\\|" (map (p: "${p.pname}-${p.version}") ps)
        })/docs|<a href="../\1-docs|g'

      printf "Replacing all absolute links to point to Hackage\n"
      fd 'html$' --type f --exec \
        sed -i 's|<a href="/package/|<a href="'$remote'|g'

      haddock \
        --quickjump \
        --gen-contents \
        --gen-index \
        ${
          concatMapStringsSep " " (s:
            "--read-interface=${s.name}-docs,${
              haddockInterface s
            }/haddock-interface") ps
        }

      mkdir -p "$out"
      mv * "$out/"
      rm "$out/env-vars"
    '';

  #
  # Interesting stuff
  #

  vulkanPkgs = import ../default.nix {
    inherit pkgs compiler;
    hoogle = false;
    forShell = false;
  };

  docDrv = _name: documentationTarball;

  tarballDrv = _name: sdistTarball;

  sdistTestDrv = _name: buildFromSdist;

  docPackages =
    filterAttrs (n: _: n != "generate-new" && n != "vulkan-examples")
    vulkanPkgs;

in mapAttrs tarballDrv vulkanPkgs // {
  sdistTest = mapAttrs sdistTestDrv vulkanPkgs;
  docs = mapAttrs docDrv docPackages;
  docs-combined = combineDocs (attrValues docPackages);
}
