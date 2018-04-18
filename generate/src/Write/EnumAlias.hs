{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.EnumAlias
  ( writeEnumAlias
  ) where


import           Data.Int
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Word
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf

import           Spec.Savvy.Alias
import           Spec.Savvy.Enum
import           Spec.Savvy.Feature
import           Write.Element

writeEnumAlias
  :: EnumAlias
  -> WriteElement
writeEnumAlias e@EnumAlias {..} =
  let weName       = "Enum Alias: " <> eaName
      weDoc        = enumAliasDoc e
      weExtensions = ["PatternSynonyms"]
      weImports    = []
      weProvides   = [Pattern eaName]
      -- TODO: add the enum type to the depends
      weDepends    = [TypeName eaExtends, PatternName eaAlias]
  in  WriteElement {..}

enumAliasDoc :: EnumAlias -> Doc ()
enumAliasDoc EnumAlias{..} = [qci|
  -- | {eaComment}
  pattern {eaName} :: {eaExtends}
  pattern {eaName} = {eaAlias}
|]
