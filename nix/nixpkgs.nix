let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/fbfb79400a08bf754e32b4d4fc3f7d8f8055cf94.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "0pgyx1l1gj33g5i9kwjar7dc3sal2g14mhfljcajj8bqzzrbc3za";
  };

in import nixpkgsSrc { }
