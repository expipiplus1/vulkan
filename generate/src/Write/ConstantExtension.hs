{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.ConstantExtension
  ( writeConstantExtension
  ) where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Feature

import           Write.Element

writeConstantExtension :: ConstantExtension -> WriteElement
writeConstantExtension ce@ConstantExtension {..} =
  let weImports    = []
      weDoc        = constantExtensionDoc ce
      weExtensions = ["PatternSynonyms"]
      weName       = "ConstantExtension: " <> ceName
      weProvides   = [Pattern ceName]
      weDepends    = case ceValue of
                       EnumValueAlias n -> [Pattern n]
                       _                -> []
  in  WriteElement {..}

constantExtensionDoc :: ConstantExtension -> Doc ()
constantExtensionDoc ConstantExtension{..} = [qci|
  pattern {ceName} = {case ceValue of
      EnumValueString s -> tShow s
      EnumValueInt i -> tShow i
      EnumValueAlias a -> a
  }
|]

tShow :: Show a => a -> Text
tShow = pack . show
