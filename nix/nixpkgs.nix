let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/17e2591d9b216634ab501293efa2ba4c55ff6bc9.tar.gz"; # refs/heads/haskell-updates
    sha256 = "151fkb184mng86dgpyr327by2fr8cr46cqr3wnghmhsgmc5yfz3b";
  };

in import nixpkgsSrc { }
