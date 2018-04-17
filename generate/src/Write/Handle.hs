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

import           Write.Element                            hiding (TypeName)

writeHandle :: Handle -> Either [SpecError] WriteElement
writeHandle h@Handle {..} = do
  weDoc <- hDoc h
  let weName       = "Handle: " <> hName
      weProvides   = [TypeAlias hName]
      weExtensions = []
      weImports    = [Import "Foreign.Ptr" ["Ptr"]]
      weDepends    = []
  pure WriteElement {..}

hDoc :: Handle -> Either [SpecError] (Doc ())
hDoc Handle{..} = do
  p <- case hType of
    Ptr (TypeName p) -> pure p
    _                -> Left [HandleToNonPointerType hName]
  pure [qci|
    -- |
    data {p}
    type {hName} = Ptr {p}
|]
