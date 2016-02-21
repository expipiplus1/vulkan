{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}

module Write.Constant
  ( writeConstants
  ) where

import Data.Maybe(fromMaybe)
import Spec.Constant
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils

writeConstants :: [Constant] -> String
writeConstants es = [qc|-- * Constants

{vcat $ writeConstant <$> es}|] 

writeConstant :: Constant -> Doc
writeConstant c = [qc|{predocComment $ fromMaybe "" (cComment c)}
{writeConstantPattern c}{case maybeWriteConstantType c of
  Nothing -> mempty
  Just d -> d}|]

writeConstantPattern :: Constant -> Doc
writeConstantPattern c 
  | IntegralValue i <- cValue c
  = [qc|pattern {cName c} = {i}|]
  | FloatValue f <- cValue c
  = [qc|pattern {cName c} = {f}|]
  | Word32Value i <- cValue c
  = [qc|pattern {cName c} = {showHex' i} :: Word32|]
  | Word64Value i <- cValue c
  = [qc|pattern {cName c} = {showHex' i} :: Word64|]

maybeWriteConstantType :: Constant -> Maybe Doc
maybeWriteConstantType c 
  | IntegralValue i <- cValue c
  = if i >= 0 
      then Just [qc|
type {cName c} = {i}|]
      else Nothing
  | otherwise
  = Nothing

