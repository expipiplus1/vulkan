let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/nixpkgs/archive/8f23b9ae7adc6f2a834589dfb38213e7efc4e654.tar.gz"; # joe-develop-opts
    sha256 = "188y95a66g0i53ls74fqfwpcfvvkvqz7b7bwfisr5fw3y6s1jjd5";
  };

in import nixpkgsSrc { }

