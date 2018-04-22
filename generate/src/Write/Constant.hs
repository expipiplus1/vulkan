{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Constant
  ( writeAPIConstant
  ) where


import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf

import           Spec.Savvy.APIConstant
import           Write.Element
import           Write.Util

writeAPIConstant :: APIConstant -> WriteElement
writeAPIConstant ac@APIConstant {..} =
  let weName    = "APIConstant: " <> acName
      weDoc     = constantDoc ac
      weDepends = []
  in  case acValue of
        IntegralValue _ ->
          let weExtensions = ["PatternSynonyms", "DataKinds"]
              weImports    = []
              weProvides = Unguarded <$>
                [TypeAlias acName, Pattern acName]
          in  WriteElement {..}
        FloatValue _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Foreign.C.Types" ["CFloat"]]
              weProvides   = [Unguarded $ Pattern acName]
          in  WriteElement {..}
        Word32Value _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Data.Word" ["Word32"]]
              weProvides   = [Unguarded $ Pattern acName]
          in  WriteElement {..}
        Word64Value _ ->
          let weExtensions = ["PatternSynonyms"]
              weImports    = [Import "Data.Word" ["Word64"]]
              weProvides   = [Unguarded $ Pattern acName]
          in  WriteElement {..}

constantDoc :: APIConstant -> DocMap -> Doc ()
constantDoc APIConstant{..} getDoc = case acValue of
  IntegralValue w -> [qci|
    {document getDoc (TopLevel acName)}
    type {acName} = {w}
|] <> line <> patterns getDoc acName "Integral a => a" w
  FloatValue f ->  patterns getDoc acName "CFloat" f
  Word32Value w -> patterns getDoc acName "Word32" (HexShow 8 w)
  Word64Value w -> patterns getDoc acName "Word64" (HexShow 16 w)

patterns
  :: Show a
  => DocMap
  -> Text
  -- ^ Name
  -> Text
  -- ^ Type
  -> a
  -- ^ Value
  -> Doc ()
patterns getDoc name t x = [qci|
  {document getDoc (Nested t name)}
  pattern {name} :: {t}
  pattern {name} = {x}
|]

data HexShow a = HexShow Int a

instance (Integral a, PrintfArg a) => Show (HexShow a) where
  show (HexShow w i) = printf ("0x%0" <> show w <> "x") i
