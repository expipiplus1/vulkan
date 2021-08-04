let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/aaf9676fbb7fb4570216ca1e189a3dc769d62c45.tar.gz"; # refs/heads/nixos-unstable
    sha256 = "1kkngmj6kafq6gp922cjn4a2ijkk4hyxmjcn64v64vbkanq0qp81";
  };

in import nixpkgsSrc { }
