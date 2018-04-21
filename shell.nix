{ pkgs ? import <nixpkgs> {}, compiler ? "ghc841" }:

(import ./default.nix { inherit pkgs compiler; }).env
