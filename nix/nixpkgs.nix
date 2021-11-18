let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      # "https://github.com/nixos/nixpkgs/archive/17e2591d9b216634ab501293efa2ba4c55ff6bc9.tar.gz"; # refs/heads/haskell-updates
      "https://github.com/expipiplus1/nixpkgs/archive/79cb423531c178af07353e2285a17abe3073e306.tar.gz"; # refs/heads/ellie-ghcHEAD
    sha256 = "0h7qi4ypl368y6i1458khb2q6db2m579cm5hv5c7d7l7rwvh7n3x";
  };

in import nixpkgsSrc { }
