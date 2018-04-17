{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Extension
  ( writeExtension
  ) where


import           Data.Int
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Word
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf

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
      -- TODO: add the enum type to the depends
      weDepends =
        (TypeName . fst <$> (rEnumExtensions =<< extRequirements))
          ++ ([TypeName, TermName] <*> (rEnumNames =<< extRequirements))
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
  -- TODO: uncomment and add the enum type to the depends
  pattern {exName} :: {extendee}
  pattern {exName} = {extendee} {writeValue exValue}
|]

writeValue :: Either Int32 Word32 -> Doc ()
writeValue = \case
  Left i -> pretty $ showsPrec 10 i ""
  Right i -> pretty $ (printf "0x%08x" i :: String)
