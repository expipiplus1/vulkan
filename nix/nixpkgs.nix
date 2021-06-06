let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/1c2986bbb806c57f9470bf3231d8da7250ab9091.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "0y1275nzlmsys5rk7ivzbdc8cpjs9cbk0wz6yh3i2c57b8nbd3ym";
  };

in import nixpkgsSrc { }
