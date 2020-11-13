let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/d3edc3a53ee646bb225b74d5d99bd7a2df33903d.tar.gz"; # haskell-updates
    sha256 = "188y95a66g0i53ls74fqfwpcfvvkvqz7b7bwfisr5fw3y6s1jjd5";
  };

in import nixpkgsSrc { }

