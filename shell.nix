{ pkgs ? import (import ./pkgs.nix) {}
, compiler ? "ghc864"
, hoogle ? true
}:

(import ./default.nix { inherit pkgs compiler hoogle; }).env
