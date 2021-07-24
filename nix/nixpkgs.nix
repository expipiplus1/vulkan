let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/16105403bdd843540cbef9c63fc0f16c1c6eaa70.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "0sl6hsxlh14kcs38jcra908nvi5hd8p8hlim3lbra55lz0kd9rcl";
  };

in import nixpkgsSrc { }
