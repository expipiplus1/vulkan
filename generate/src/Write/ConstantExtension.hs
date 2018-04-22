{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.ConstantExtension
  ( writeConstantExtension
  ) where

import           Data.Maybe
import           Data.Text.Extra
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Feature

import           Write.Element
import           Write.Util

-- TODO: Reduce duplication here
writeConstantExtension
  :: (Text -> Maybe Text) -> ConstantExtension -> WriteElement
writeConstantExtension getEnumerantEnumName ce@ConstantExtension {..} =
  let weImports = case ceValue of
        EnumValueString _ -> [Import "Data.String" ["IsString"]]
        EnumValueInt    _ -> []
        EnumValueAlias  _ -> []
      weDoc        = constantExtensionDoc getEnumerantEnumName ce
      weExtensions = "PatternSynonyms" : case ceValue of
        EnumValueString _ -> ["OverloadedStrings"]
        EnumValueInt    _ -> []
        EnumValueAlias  _ -> []
      weName     = "ConstantExtension: " <> ceName
      weProvides = [Unguarded $ Pattern ceName]
      weDepends  = Unguarded <$> case ceValue of
        EnumValueAlias n ->
          [ PatternName n
          , TypeName (fromMaybe (error (show n)) (getEnumerantEnumName n))
          ]
        _ -> []
  in  WriteElement {..}

constantExtensionDoc
  :: (Text -> Maybe Text) -> ConstantExtension -> DocMap -> Doc ()
constantExtensionDoc getEnumerantEnumName ConstantExtension{..} getDoc = [qci|
  {document getDoc (TopLevel ceName)}
  pattern {ceName} :: {case ceValue of
      EnumValueString _ -> "(Eq a ,IsString a) => a" :: Doc a
      EnumValueInt    _ -> "Integral a => a"
      -- TODO: error handling here
      EnumValueAlias  a -> pretty (fromMaybe (error (show a)) (getEnumerantEnumName a))
  }
  pattern {ceName} = {case ceValue of
      EnumValueString s -> tShow s
      EnumValueInt    i -> tShow i
      EnumValueAlias  a -> a
  }
|]
