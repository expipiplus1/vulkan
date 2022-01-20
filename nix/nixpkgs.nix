let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/934d058a48e26b9b42f610024ba7139293e4164e.tar.gz"; # refs/heads/haskell-updates
      # "https://github.com/expipiplus1/nixpkgs/archive/79cb423531c178af07353e2285a17abe3073e306.tar.gz"; # refs/heads/ellie-ghcHEAD
    sha256 = "0ivdy18av2xjk2w43x2iqdcdk0rfpfbijgap1h47k4ql3ygrn250";
  };

in import nixpkgsSrc { }
