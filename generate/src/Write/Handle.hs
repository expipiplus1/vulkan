{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Handle
  ( writeHandle
  ) where

import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Control.Monad.Except
import           Data.Text.Extra               as T

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import           Write.Util
import           Write.Monad

writeHandle :: Handle -> Write WriteElement
writeHandle h@Handle {..} =
  let go = do
        tellImport "Foreign.Ptr" "Ptr"
        tellExport (TypeAlias hName)
        hDoc h
  in  runWE ("Handle: " <> hName) $ do
        tellBootElem <=< liftWrite . runWE ("Handle boot: " <> hName) $ go
        go

hDoc :: Handle -> WE (DocMap -> Doc ())
hDoc Handle{..} = do
  p <- case hType of
    Ptr _ (TypeName p) -> pure p
    _                  ->
      throwError $ ("Handle to non pointer type" T.<+> hName)
  pure (\getDoc -> [qci|
    -- | Dummy data to tag the 'Ptr' with
    data {p}
    {document getDoc (TopLevel hName)}
    type {hName} = Ptr {p}
|])
