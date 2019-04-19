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

import           Write.Element                 as WE
import           Write.Util
import           Write.Marshal.Monad

writeFuncPointer :: FuncPointer -> Either [SpecError] WriteElement
writeFuncPointer fp@FuncPointer {..} = do
  let name = "FuncPointer boot: " <> fpName
  bootElem <-
    wrapMToWriteElements name Nothing
    . censorSourceDepends [(WE.TypeName "(:::)")]
    $ fpDoc fp

  wrapMToWriteElements name (Just bootElem) (fpDoc fp)

fpDoc
  :: FuncPointer
  -> WrapM (DocMap -> Doc ())
fpDoc FuncPointer{..} = do
  t <- toHsType fpType
  tellExport (Unguarded (TypeAlias fpName))
  pure $ \getDoc -> [qci|
    {document getDoc (TopLevel fpName)}
    type {fpName} = {t}
  |]

