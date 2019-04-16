let pkgsRev = "acbdaa569f4ee387386ebe1b9e60b9f95b4ab21b";
    pkgsSHA256 = "0xzyghyxk3hwhicgdbi8yv8b8ijy1rgdsj5wb26y5j322v96zlpz";
in builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${pkgsRev}.tar.gz";
  sha256 = pkgsSHA256;
}
