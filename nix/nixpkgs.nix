let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/04a2b269d8921505a2969fc9ec25c1f517f2b307.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "15hgx2i71pqgvzv56jwzfs8rkhjbm35wk1i6mxrqbq6wd0y10isv";
  };

in import nixpkgsSrc { }
