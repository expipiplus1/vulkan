{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Command
  ( writeCommand
  ) where

import           Data.Maybe
import           Data.Text                                (Text)
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element

writeCommand :: Command -> Either [SpecError] WriteElement
writeCommand fp@Command {..} = do
  (weDoc, weImports, weExtensions) <- commandDoc fp
  let weProvides = [Type cName]
      weDepends  = typeDepends $ Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]
  pure WriteElement {..}

commandDoc :: Command -> Either [SpecError] (Doc (), [Import], [Text])
commandDoc Command {..} = do
  let proto = Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]
  (t, (is, es)) <- toHsType proto
  let d = [qci|
  -- | {fromMaybe "" cComment}
  foreign import ccall "{cName}" {cName} :: {t}
|]
  pure (d, is, es)

lowerArrayToPointer :: Type -> Type
lowerArrayToPointer = \case
    Array _ t -> Ptr t
    t         -> t
