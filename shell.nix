{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

(import ./default.nix { inherit pkgs compiler; }).env
