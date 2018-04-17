{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Extension
  ( writeExtension
  ) where


import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Enum
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Write.Element

writeExtension :: Extension -> WriteElement
writeExtension e@Extension {..}
  = let
      weName       = "Extension: " <> extName
      weDoc        = extensionDoc e
      weExtensions = ["PatternSynonyms"]
      weImports    = []
      weProvides =
        (Pattern . exName . snd <$> (rEnumExtensions =<< extRequirements))
      weDepends =
        ([TypeName, TermName] <*> (rEnumNames =<< extRequirements))
          ++ (TermName <$> (rCommandNames =<< extRequirements))
    in
      WriteElement {..}

extensionDoc :: Extension -> Doc ()
extensionDoc Extension{..} = [qci|
  {vcat $ uncurry enumExtensionDoc <$> (rEnumExtensions =<< extRequirements)}
|]

enumExtensionDoc :: Text -> EnumExtension -> Doc ()
enumExtensionDoc extendee EnumExtension{..} = [qci|
  -- | {exComment}
  pattern {exName} :: {extendee}
  pattern {exName} = {extendee} {exValue}
|]
