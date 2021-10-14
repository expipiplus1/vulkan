let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/9bf75dd50b7b6d3ce6aaf6563db95f41438b9bdb.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "0ii3z5v9p21la8gc8l136s5rax932awz7mk757jciai766lp2fhz";
  };

in import nixpkgsSrc { }
