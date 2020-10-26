let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/expipiplus1/nixpkgs/archive/c338c92af7808d2910e71b56f43c2e1cdb916d1f.tar.gz"; # pin
    sha256 = "1ysh8fr3y7yc5didx24qxsm6n6xw9r0w3kz2vkg1jh0iaw6lfizc";
  };

in import nixpkgsSrc { }

