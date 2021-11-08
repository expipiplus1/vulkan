let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/2413ab9e72fe4933c832cc216d6fbc23beef2f10.tar.gz"; # refs/heads/haskell-updates
    sha256 = "196sl2p3cn9bc3bl12bb4i0a7crx8yqqynhsm7bf87mi7wabv3b6";
  };

in import nixpkgsSrc { }
