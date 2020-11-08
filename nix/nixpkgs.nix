let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/34ad166a830d3ac1541dcce571c52231f2f0865a.tar.gz"; # nixos-unstable
    sha256 = "1jvi1562x3kq65w642vfimpszv65zbc7c2nv8gakhzcx4n3f47xq";
  };

in import nixpkgsSrc { }

