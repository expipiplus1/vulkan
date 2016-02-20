{-# LANGUAGE QuasiQuotes #-}

module Write.Enum
  ( writeEnums
  ) where

import Data.Maybe(fromMaybe)
import Prelude hiding (Enum)
import Spec.Enum
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils

writeEnums :: [Enum] -> String
writeEnums es = [qc|-- * Enumerations

{vcat $ writeEnum <$> es}|] 

writeEnum :: Enum -> Doc
writeEnum e = [qc|-- ** {eName e}
{predocComment $ fromMaybe "" (eComment e)}
newtype {eName e} = {eName e} Int32
  deriving (Eq, Storable)
{vcat $ writeElement e <$> eElements e}
|]

writeElement :: Enum -> EnumElement -> Doc
writeElement e el = 
  [qc|{maybe "" predocComment (eeComment el)}
pattern {eeName el} = {eName e} {showsPrec 10 (eeValue el) ""}|] 
