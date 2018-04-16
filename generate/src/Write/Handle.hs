{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Handle
  ( writeHandle
  ) where

import           Data.Text                                (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element

writeHandle :: Handle -> Either [SpecError] WriteElement
writeHandle h@Handle {..} = do
  (weDoc, weImports, weExtensions) <- hDoc h
  let weName     = "Handle: " <> hName
      weProvides = [Type hName]
      weDepends  = typeDepends hType
  pure WriteElement {..}

hDoc :: Handle -> Either [SpecError] (Doc (), [Import], [Text])
hDoc Handle{..} = do
  (t, (is, es)) <- toHsType hType
  let d = [qci|
  -- |
  type {hName} = {t}
|]
  pure (d, is, es)
