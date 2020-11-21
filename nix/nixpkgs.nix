let
  nixpkgsSrc = builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/65c4e2500f25076a2db330b8953410faab817420.tar.gz"; # refs/heads/master
    sha256 = "1cxsyhjawwgr231ry66rc631k72n276c9zzqswr5jnl2cb05qwhl";
  };

in import nixpkgsSrc { }
