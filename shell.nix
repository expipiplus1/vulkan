{ pkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:

(import ./default.nix { inherit pkgs compiler; }).env
