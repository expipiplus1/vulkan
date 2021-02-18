let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/64b4617883844efe0cc20163e007ee636462eb18.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "1vqqllxzdvvarwydv6yx0qwwl9shqla08ijabvmydi1kwc6388ww";
  };

in import nixpkgsSrc { }
