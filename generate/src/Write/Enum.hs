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
  pure [qc|-- ** {eHsName e}
{predocComment $ fromMaybe "" (eComment e)}
newtype {eHsName e} = {eHsName e} Int32
  deriving (Eq, Storable)

instance Show {eHsName e} where
  {indent 0 $ vcat (writeElementShowsPrec <$> eElements e)}
  showsPrec p ({eHsName e} x) = showParen (p >= 11) (showString "{eHsName e} " . showsPrec 11 x)

instance Read {eHsName e} where
  readPrec = parens ( choose [ {indent (-2) . vcat $ intercalatePrepend "," (writeReadTuple <$> eElements e)}
                             ] +++
                      prec 10 (do
                        expectP (Ident "{eHsName e}")
                        v <- step readPrec
                        pure ({eHsName e} v)
                        )
                    )

{vcat $ writeElement e <$> eElements e}
|]

writeElement :: Enum -> EnumElement -> Doc
writeElement e el =
  [qc|{maybe "" predocComment (eeComment el)}
pattern {eeHsName el} = {eHsName e} {showsPrec 10 (eeValue el) ""}|]

writeElementShowsPrec :: EnumElement -> Doc
writeElementShowsPrec el =
  [qc|showsPrec _ {eeHsName el} = showString "{eeHsName el}"|]

writeReadTuple :: EnumElement -> Doc
writeReadTuple el = [qc|("{eeHsName el}", pure {eeHsName el})|]
