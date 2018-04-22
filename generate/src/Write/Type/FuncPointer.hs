{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Type.FuncPointer
  ( writeFuncPointer
  ) where

import           Data.Text                                (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.FuncPointer
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element
import           Write.Util

writeFuncPointer :: FuncPointer -> Either [SpecError] WriteElement
writeFuncPointer fp@FuncPointer {..} = do
  (weDoc, weImports, weExtensions) <- fpDoc fp
  let weName     = "FuncPointer: " <> fpName
      weProvides = Unguarded <$> [TypeAlias fpName]
      weDepends  = Unguarded <$> typeDepends fpType
  pure WriteElement {..}

fpDoc :: FuncPointer -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
fpDoc FuncPointer{..} = do
  (t, (is, es)) <- toHsType fpType
  let d getDoc = [qci|
  {document getDoc (TopLevel fpName)}
  type {fpName} = {t}
|]
  pure (d, is, es)

