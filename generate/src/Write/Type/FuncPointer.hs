{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Type.FuncPointer
  ( writeFuncPointer
  ) where

import           Data.Text                                ( Text )
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Control.Monad

import           Spec.Savvy.Error
import           Spec.Savvy.FuncPointer
import           Spec.Savvy.Type

import           Write.Element                 as WE
import           Write.Util
import           Write.Monad

writeFuncPointer :: FuncPointer -> Write WriteElement
writeFuncPointer fp@FuncPointer {..} =
  let name = "FuncPointer boot: " <> fpName
  in  runWE name $ do
        tellBootElem
          <=< liftWrite
          .   runWE name
          .   makeSourceDepends [WE.TypeName "(:::)"]
          $   fpDoc fp
        fpDoc fp

fpDoc
  :: FuncPointer
  -> WE (DocMap -> Doc ())
fpDoc FuncPointer{..} = do
  t <- toHsType fpType
  tellExport (TypeAlias fpName)
  pure $ \getDoc -> [qci|
    {document getDoc (TopLevel fpName)}
    type {fpName} = {t}
  |]

