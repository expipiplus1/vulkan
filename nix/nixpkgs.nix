let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/ae1dc133ea5f1538d035af41e5ddbc2ebcb67b90.tar.gz"; # refs/heads/nixos-unstable
      # "https://github.com/expipiplus1/nixpkgs/archive/79cb423531c178af07353e2285a17abe3073e306.tar.gz"; # refs/heads/ellie-ghcHEAD
    sha256 = "0dq22dagzk76x2ws4dz88w018i6byamd6rnzqizx68bzimg6g7xn";
  };

in import nixpkgsSrc { }
