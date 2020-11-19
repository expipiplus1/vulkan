let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/f133f71eaa99554865131511bba2d4c650b9e3ed.tar.gz"; # haskell-updates
    sha256 = "1ipfs9f98hcba1jpaz89xvjn8k07ka84z5jji7s7by03q8m2r2yz";
  };

in import nixpkgsSrc { }
