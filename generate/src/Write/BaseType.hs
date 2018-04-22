{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.BaseType
  ( writeBaseType
  ) where

import           Data.Text                                (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.BaseType
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element                            hiding (TypeName)
import           Write.Util

writeBaseType :: BaseType -> Either [SpecError] WriteElement
writeBaseType bt@BaseType {..} = do
  (weDoc, weImports, weExtensions) <- hDoc bt
  let weName     = "BaseType: " <> btName
      weProvides = [Unguarded $ TypeAlias btName]
      weDepends  = Unguarded <$> typeDepends btType
  pure WriteElement {..}

hDoc :: BaseType -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
hDoc BaseType{..} = do
  (t, (is, es)) <- toHsType btType
  let d getDoc = [qci|
  {document getDoc (TopLevel btName)}
  type {btName} = {t}
|]
  pure (d, is, es)
