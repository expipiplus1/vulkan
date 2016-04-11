{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes   #-}

module Write.Constant
  ( writeConstant
  ) where

import           Data.Maybe                    (fromMaybe)
import           Spec.Constant
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.Utils
import           Write.WriteMonad

writeConstant :: Constant -> Write Doc
writeConstant c = do
  tellExtension "PatternSynonyms"
  typeStringMay <- maybeWriteConstantType c
  let typeString = fromMaybe mempty typeStringMay
  patternString <- writeConstantPattern c
  pure [qc|{predocComment $ fromMaybe "" (cComment c)}
{patternString}{typeString}|]

writeConstantPattern :: Constant -> Write Doc
writeConstantPattern c = case cValue c of
  IntegralValue i
    -> pure [qc|pattern {cHsName c} = {i}|]
  FloatValue f
    -> pure [qc|pattern {cHsName c} = {f}|]
  Word32Value i
    -> do tellRequiredName (ExternalName (ModuleName "Data.Word") "Word32")
          pure [qc|pattern {cHsName c} = {showHex' i} :: Word32|]
  Word64Value i
    -> do tellRequiredName (ExternalName (ModuleName "Data.Word") "Word64")
          pure [qc|pattern {cHsName c} = {showHex' i} :: Word64|]

maybeWriteConstantType :: Constant -> Write (Maybe Doc)
maybeWriteConstantType c
  | IntegralValue i <- cValue c
  , i >= 0
  = do tellExtension "DataKinds"
       pure $ Just [qc|
type {cHsName c} = {i}|]
  | otherwise
  = pure Nothing

