{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Enum
  ( writeEnum
  ) where

import           Data.Maybe                    (fromMaybe)
import           Prelude                       hiding (Enum)
import           Spec.Enum
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.Utils
import           Write.WriteMonad

writeEnum :: Enum -> Write Doc
writeEnum e = do
  tellRequiredName (ExternalName (ModuleName "Data.Int") "Int32")
  tellRequiredName (ExternalName (ModuleName "Foreign.Storable") "Storable(..)")
  tellRequiredNames
    (ExternalName (ModuleName "Text.Read") <$> [ "Read(..)"
                                               , "parens"
                                               ])
  tellRequiredName (ExternalName (ModuleName "Text.Read.Lex") "Lexeme(Ident)")
  tellRequiredNames
    (ExternalName (ModuleName "Text.ParserCombinators.ReadPrec") <$>
      [ "(+++)"
      , "prec"
      , "step"
      ])
  tellRequiredNames
    (ExternalName (ModuleName "GHC.Read") <$> [ "expectP"
                                              , "choose"
                                              ])
  tellExtension "GeneralizedNewtypeDeriving"
  tellExtension "PatternSynonyms"
  pure [qc|-- ** {eName e}
{predocComment $ fromMaybe "" (eComment e)}
newtype {eName e} = {eName e} Int32
  deriving (Eq, Ord, Storable)

instance Show {eName e} where
  {indent 0 $ vcat (writeElementShowsPrec <$> eElements e)}
  showsPrec p ({eName e} x) = showParen (p >= 11) (showString "{eName e} " . showsPrec 11 x)

instance Read {eName e} where
  readPrec = parens ( choose [ {indent (-2) . vcat $ intercalatePrepend "," (writeReadTuple <$> eElements e)}
                             ] +++
                      prec 10 (do
                        expectP (Ident "{eName e}")
                        v <- step readPrec
                        pure ({eName e} v)
                        )
                    )

{vcat $ writeElement e <$> eElements e}
|]

writeElement :: Enum -> EnumElement -> Doc
writeElement e el =
  [qc|{maybe "" predocComment (eeComment el)}
pattern {eeName el} = {eName e} {showsPrec 10 (eeValue el) ""}|]

writeElementShowsPrec :: EnumElement -> Doc
writeElementShowsPrec el =
  [qc|showsPrec _ {eeName el} = showString "{eeName el}"|]

writeReadTuple :: EnumElement -> Doc
writeReadTuple el = [qc|("{eeName el}", pure {eeName el})|]
