{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Constant
  ( writeAPIConstant
  ) where


import           Data.Text
import           Text.Printf
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.APIConstant
import           Write.Element

writeAPIConstant :: APIConstant -> WriteElement
writeAPIConstant ac@APIConstant {..} =
  let weName    = "APIConstant: " <> acName
      weDoc     = constantDoc ac
      weDepends = []
  in  case acValue of
        IntegralValue _ ->
          let weExtensions = ["PatternSynonyms", "DataKinds"]
              weImports    = []
              weProvides =
                [Pattern acName, TypeAlias acName]
          in  WriteElement {..}
        FloatValue _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Foreign.C.Types" ["CFloat"]]
              weProvides   = [Pattern acName]
          in  WriteElement {..}
        Word32Value _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Data.Word" ["Word32"]]
              weProvides   = [Pattern acName]
          in  WriteElement {..}
        Word64Value _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Data.Word" ["Word64"]]
              weProvides   = [Pattern acName]
          in  WriteElement {..}

constantDoc :: APIConstant -> Doc ()
constantDoc APIConstant{..} = case acValue of
  IntegralValue w -> [qci|
    type {acName} = {w}
|] <> line <> patterns acName "Integral a => a" w
  FloatValue f ->  patterns acName "CFloat" f
  Word32Value w -> patterns acName "Word32" (HexShow 8 w)
  Word64Value w -> patterns acName "Word64" (HexShow 16 w)

patterns
  :: Show a
  => Text
  -- ^ Name
  -> Text
  -- ^ Type
  -> a
  -- ^ Value
  -> Doc ()
patterns name t x = [qci|
  pattern {name} :: {t}
  pattern {name} = {x}
|]

data HexShow a = HexShow Int a

instance (Integral a, PrintfArg a) => Show (HexShow a) where
  show (HexShow w i) = printf ("0x%0" <> show w <> "x") i
