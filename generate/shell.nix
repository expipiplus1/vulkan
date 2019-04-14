{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc864"
, hoogle ? true
}:

(import ./default.nix { inherit pkgs compiler hoogle; }).env
