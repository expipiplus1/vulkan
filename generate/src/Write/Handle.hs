{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Handle
  ( writeHandle
  ) where

import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Write.Element                            hiding (TypeName)
import           Write.Util

writeHandle :: Handle -> Either [SpecError] WriteElement
writeHandle h@Handle {..} = do
  weDoc <- hDoc h
  let weName       = "Handle: " <> hName
      weExtensions = []
      weImports    = [Import "Foreign.Ptr" ["Ptr"]]
      weProvides   = [Unguarded $ TypeAlias hName]
      weDepends    = []
  pure WriteElement {..}

hDoc :: Handle -> Either [SpecError] (DocMap -> Doc ())
hDoc Handle{..} = do
  p <- case hType of
    Ptr (TypeName p) -> pure p
    _                -> Left [HandleToNonPointerType hName]
  pure (\getDoc -> [qci|
    -- | Dummy data to tag the 'Ptr' with
    data {p}
    {document getDoc (TopLevel hName)}
    type {hName} = Ptr {p}
|])
