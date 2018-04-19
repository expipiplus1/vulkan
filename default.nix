{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

# Strip out the irrelevant parts of the source
let src = with pkgs.lib;
          let p = n: (toString ./generate) == n || (toString ./dist) == n;
          in cleanSourceWith {filter = (n: t: !p n); src = cleanSource ./.;};
in

pkgs.haskell.packages.${compiler}.callCabal2nix
  "vulkan"
  src
  { vulkan = pkgs.vulkan-loader; }
