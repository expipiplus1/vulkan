let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/e019872af81e4013fd518fcacfba74b1de21a50e.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "1sshfjblicvg4p5qz84j5w47vvb1hfzsn4bnr9xcpd4dik17gv6a";
  };

in import nixpkgsSrc { }
