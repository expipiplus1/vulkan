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
import           Spec.Savvy.Type                          hiding (TypeName)
import           Spec.Savvy.Type.Haskell

import           Write.Element
import           Write.Util

writeCommand
  :: (Text -> Maybe Text)
  -- ^ A function to get the target of an enum alias
  -> Command
  -> Either [SpecError] WriteElement
writeCommand getEnumName fp@Command {..} = do
  (weDoc, weImports, weExtensions) <- commandDoc fp
  let
    weName       = "Command: " <> cName
    protoDepends = typeDepends $ Proto
      cReturnType
      [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]
    weProvides = [Unguarded $ Term cName]
    weDepends =
      Unguarded
        <$> protoDepends
        <> -- The constructors for an enum type need to be in scope
            [ TypeName e
            | TypeName n <- protoDepends
            , Just e <- [getEnumName n]
            ]
  pure WriteElement {..}

commandDoc :: Command -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
commandDoc c@Command {..} = do
  (t, (is, es)) <- toHsType (commandType c)
  let d getDoc = [qci|
  {document getDoc (TopLevel cName)}
  foreign import ccall
  #if !defined(SAFE_FOREIGN_CALLS)
    unsafe
  #endif
    "{cName}" {cName} :: {t}
|]
  pure (d, is, es)
