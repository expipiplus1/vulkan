# Haskell package overrides

{ pkgs, hoogle }:

with pkgs.haskell.lib;

self: super:
{
  #
  # Examples
  #
  th-desugar = self.th-desugar_1_11;
  pretty-simple = self.pretty-simple_4_0_0_0;
  prettyprinter = self.prettyprinter_1_7_0;
  autoapply = markUnbroken (dontCheck super.autoapply);

  #
  # Generate
  #
  dependent-sum = self.callCabal2nix "" ((pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum";
    rev = "73ab6cb23331f463c384290b4a1be542e68b323d";
    sha256 = "18h0k0n05spsk5jvgmphv3sj4wdwb3qdxklfkn5wq8kni0bqabzk";
  }) + "/dependent-sum") { };
  dependent-map = self.callCabal2nix "" (pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-map";
    rev = "26677886eced970d661a5a7356ba4fe221c0324c";
    sha256 = "1865yqnxzlrkmbag4xn47csgagmk968z4n633sk2c75d48icyzf9";
  }) { };
  polysemy = doJailbreak (self.callCabal2nix "" ((pkgs.fetchFromGitHub {
    owner = "polysemy-research";
    repo = "polysemy";
    rev = "72dc96fbd13dba6d8e9767253b7298e00a781bee";
    sha256 = "09b1n71gjmhf4ggx2wlywxm11jl3qbmhnlmmchj8pyy3hczl6hb5";
  })) { });
  polysemy-zoo = dontCheck (self.callCabal2nix "" ((pkgs.fetchFromGitHub {
    owner = "polysemy-research";
    repo = "polysemy-zoo";
    rev = "57c6012e196db7fe1ce7551f1f762cbddc71f095";
    sha256 = "18smd2c66gdn9585sdkn60ykvdvkbvkxrnnl9zix687dca6h9jw0";
  })) { });
  compact = doJailbreak super.compact;
  pandoc =
    appendPatch super.pandoc ../generate-new/patches/pandoc-haddock-tables.patch;
  language-c = self.callHackageDirect {
    pkg = "language-c";
    ver = "0.9";
    sha256 = "1dxi56aawabq2ds6crvhhr9dwmbyanjkn9l0yhw7wcqrwx71kliq";
  } { };
} // pkgs.lib.optionalAttrs hoogle {
  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
  ghcWithPackages = p:
    self.ghc.withPackages (f: p f ++ (if forShell then [ f.process ] else [ ]));
}
