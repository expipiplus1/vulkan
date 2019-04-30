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
import           Spec.Savvy.Command
import           Spec.Savvy.Type(Type(TypeName))

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Monad
import           Write.Marshal.Util

handleWrapper :: Handle -> Write WriteElement
handleWrapper Handle {..} = do
  runWE (hName T.<+> "wrapper") $ do
    when (hHandleType == NonDispatchable)
      $ throwError "Wrapping a non-dispatchable handle"
    let marshalledName = dropVkType hName
    tellExport (TypeConstructor marshalledName)
    tellExport (Term marshalledName)
    tellDepend (WE.TypeName hName)
    cmdTable <- case hLevel of
      Just Instance       -> pure "InstanceCmds"
      Just PhysicalDevice -> pure "InstanceCmds"
      Just Device         -> pure "DeviceCmds"
      Nothing             -> throwError "wrapping handle without a level"
    tellDepend (WE.TypeName cmdTable)
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
