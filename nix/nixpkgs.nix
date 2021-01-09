let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/f211631c1cb3e94828c7650b5d12c1e5a89e0e16.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "0r085j42991qcbzx4l0hnwlsxw016y4b7r821s4qxvqnvwr9lxar";
  };

in import nixpkgsSrc { }
