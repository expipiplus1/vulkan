let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/3478ac5c5ba30b841cb8096d314c01fa553b19a3.tar.gz"; # refs/heads/haskell-updates
    sha256 = "0n19dfpwsidyk2vpyi0b42rmqrphsdhvyklr732s3zgxfhxj4plm";
  };

in import nixpkgsSrc { }
