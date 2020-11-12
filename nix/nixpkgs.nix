let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/14eeda702b7a5d746db8a7689dd8a22b6453ff21.tar.gz"; # haskell-updates
    sha256 = "01h8xqrsdx0kmfnyjwj80g4cilajlbx7q2wrz59xqa4ghrm53nwz";
  };

in import nixpkgsSrc { }

