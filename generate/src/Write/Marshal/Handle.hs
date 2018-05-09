{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Handle
  ( handleWrapper
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Function
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Marshal.Monad

handleWrapper :: Handle -> Either [SpecError] WriteElement
handleWrapper handle = do
  let weName        = hName handle T.<+> "wrapper"
      weBootElement = Nothing
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext (hName handle)))
           pure
           (runWrap $ wrapHandle handle)
  pure WriteElement {..}

wrapHandle :: Handle -> WrapM (DocMap -> Doc ())
  -- ^ Returns the docs for this handle, and any aliases
wrapHandle Handle {..} = do
  when (hHandleType == NonDispatchable)
    $ throwError [Other "Wrapping a non-dispatchable handle"]
  let marshalledName = dropVkType hName
  tellExport (Unguarded (TypeConstructor marshalledName))
  tellExport (Unguarded (Term marshalledName))
  tellDepend (Unguarded (WE.TypeName hName))
  cmdTable <- case hLevel of
    Just Instance       -> pure "InstanceCmds"
    Just PhysicalDevice -> pure "InstanceCmds"
    Just Device         -> pure "DeviceCmds"
    Nothing             -> throwError [Other "wrapping handle without a level"]
  tellDepend (Unguarded (WE.TypeName cmdTable))
  tellImport "Data.Function" "on"
  pure $ \_ -> [qci|
    data {marshalledName} = {marshalledName}
      \{ {T.lowerCaseFirst marshalledName}Handle :: {hName}
      , {T.lowerCaseFirst marshalledName}Cmds    :: {cmdTable}
      }
      deriving Show

    instance Eq {marshalledName} where
      (==) = (==) `on` {T.lowerCaseFirst marshalledName}Handle

    instance Ord {marshalledName} where
      compare = compare `on` {T.lowerCaseFirst marshalledName}Handle

  |]

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"