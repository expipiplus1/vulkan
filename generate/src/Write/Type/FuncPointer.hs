{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Write.Type.FuncPointer
  ( writeFuncPointerType
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

writeFuncPointerType :: FuncPointer -> Either [SpecError] WriteElement
writeFuncPointerType fp@FuncPointer {..} = do
  (weDoc, weImports, weExtensions) <- fpDoc fp
  let weProvides = [Type fpName]
      weDepends  = typeDepends fpType
  pure WriteElement {..}

fpDoc :: FuncPointer -> Either [SpecError] (Doc (), [Import], [Text])
fpDoc FuncPointer{..} = do
  (t, (is, es)) <- toHsType fpType
  let d = [qci|
  -- |
  type {fpName} = {t}
|]
  pure (d, is, es)

