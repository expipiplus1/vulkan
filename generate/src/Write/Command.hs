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
      [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
    weProvides = [ InvGuarded "NO_IMPORT_COMMANDS" $ Term cName
                 , Unguarded (TypeAlias ("FN_" <> cName))
                 , Unguarded (TypeAlias ("PFN_" <> cName))
                 ]
    weDepends =
      Unguarded
        <$> protoDepends
        <> -- The constructors for an enum type need to be in scope
            [ TypeName e
            | TypeName n <- protoDepends
            , Just e <- [getEnumName n]
            ]
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
  pure WriteElement {..}

commandDoc :: Command -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
commandDoc c@Command {..} = do
  (t, (is, es)) <- toHsType (commandType c)
  let d getDoc = [qci|
  #if !defined(NO_IMPORT_COMMANDS)
  {document getDoc (TopLevel cName)}
  foreign import ccall
  #if !defined(SAFE_FOREIGN_CALLS)
    unsafe
  #endif
    "{cName}" {cName} :: {t}
  #endif
  type FN_{cName} = {t}
  type PFN_{cName} = FunPtr FN_{cName}
|]
  pure (d, Import "Foreign.Ptr" ["FunPtr"] : is, es)
