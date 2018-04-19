{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

(import ./default.nix { inherit pkgs compiler; }).env
