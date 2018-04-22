{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.EnumExtension
  ( writeEnumExtension
  ) where


import           Data.Int
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Word
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf

import           Spec.Savvy.Enum
import           Write.Element
import           Write.Util

writeEnumExtension
  :: Text
  -- ^ The enum type
  -> EnumExtension
  -> WriteElement
writeEnumExtension enumName e@EnumExtension {..} =
  let weName       = "Enum Extension: " <> exName <> " : " <> enumName
      weDoc        = enumExtensionDoc enumName e
      weExtensions = ["PatternSynonyms"]
      weImports    = []
      weProvides   = [Unguarded $ Pattern exName]
      -- TODO: add the enum type to the depends
      weDepends    = [Unguarded $ TypeName enumName]
  in  WriteElement {..}

enumExtensionDoc :: Text -> EnumExtension -> DocMap -> Doc ()
enumExtensionDoc extendee EnumExtension{..} getDoc = [qci|
  {document getDoc (Nested extendee exName)}
  pattern {exName} :: {extendee}
  pattern {exName} = {extendee} {writeValue exValue}
|]

writeValue :: Either Int32 Word32 -> Doc ()
writeValue = \case
  Left i -> pretty $ showsPrec 10 i ""
  Right i -> pretty (printf "0x%08x" i :: String)
