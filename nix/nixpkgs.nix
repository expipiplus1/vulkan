let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/fad51abd42ca17a60fc1d4cb9382e2d79ae31836.tar.gz"; # refs/heads/nixos-unstable
      # "https://github.com/expipiplus1/nixpkgs/archive/79cb423531c178af07353e2285a17abe3073e306.tar.gz"; # refs/heads/ellie-ghcHEAD
    sha256 = "0jzy84zgjgblp2ph3kb2kj5z2k05vagn6ms5m80pjr2g27m6hr37";
  };

in import nixpkgsSrc { }
