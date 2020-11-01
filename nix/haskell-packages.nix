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
  pandoc =
    appendPatch super.pandoc ../generate-new/patches/pandoc-haddock-tables.patch;
  language-c = self.language-c_0_9;
} // pkgs.lib.optionalAttrs hoogle {
  ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
  ghcWithPackages = p:
    self.ghc.withPackages (f: p f ++ (if forShell then [ f.process ] else [ ]));
}
