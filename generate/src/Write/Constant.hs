{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}

module Write.Constant
  ( writeConstant
  ) where

import Data.Maybe(fromMaybe)
import Spec.Constant
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils
import Write.WriteMonad

writeConstant :: Constant -> Write Doc
writeConstant c = do
  tellExtension "PatternSynonyms"
  typeStringMay <- maybeWriteConstantType c
  let typeString = case typeStringMay of
                     Nothing -> mempty
                     Just d -> d
  patternString <- writeConstantPattern c
  pure [qc|{predocComment $ fromMaybe "" (cComment c)}
{patternString}{typeString}|]

writeConstantPattern :: Constant -> Write Doc
writeConstantPattern c 
  | IntegralValue i <- cValue c
  = pure [qc|pattern {cName c} = {i}|]
  | FloatValue f <- cValue c
  = pure [qc|pattern {cName c} = {f}|]
  | Word32Value i <- cValue c
  = do tellRequiredName (ExternalName (ModuleName "Data.Word") "Word32")
       pure [qc|pattern {cName c} = {showHex' i} :: Word32|]
  | Word64Value i <- cValue c
  = do tellRequiredName (ExternalName (ModuleName "Data.Word") "Word64")
       pure [qc|pattern {cName c} = {showHex' i} :: Word64|]

maybeWriteConstantType :: Constant -> Write (Maybe Doc)
maybeWriteConstantType c 
  | IntegralValue i <- cValue c
  , i >= 0
  = do tellExtension "DataKinds"
       pure $ Just [qc|
type {cName c} = {i}|]
  | otherwise
  = pure Nothing

