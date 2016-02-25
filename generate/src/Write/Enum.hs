{-# LANGUAGE QuasiQuotes #-}

module Write.Enum
  ( writeEnum
  ) where

import Data.Maybe(fromMaybe)
import Prelude hiding (Enum)
import Spec.Enum
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils
import Write.WriteMonad

writeEnum :: Enum -> Write Doc
writeEnum e = do
  tellRequiredName (ExternalName (ModuleName "Data.Int") "Int32")  
  tellRequiredName (ExternalName (ModuleName "Foreign.Storable") "Storable(..)")  
  tellExtension "GeneralizedNewtypeDeriving"
  tellExtension "PatternSynonyms"
  pure [qc|-- ** {eName e}
{predocComment $ fromMaybe "" (eComment e)}
newtype {eName e} = {eName e} Int32
  deriving (Eq, Storable)
{vcat $ writeElement e <$> eElements e}
|]

writeElement :: Enum -> EnumElement -> Doc
writeElement e el = 
  [qc|{maybe "" predocComment (eeComment el)}
pattern {eeName el} = {eName e} {showsPrec 10 (eeValue el) ""}|] 
