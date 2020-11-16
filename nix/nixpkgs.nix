let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/bff9a1a12437140ac900e5fc419b6c8185782abf.tar.gz"; # refs/heads/master
    sha256 = "1z0crmx86l88mi3zwwlc5drc24d8nv7lqiri87jxpnm50y07kca5";
  };

in import nixpkgsSrc { }
