{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Handle
  ( handleWrapper
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Bool
import           Control.Monad
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char                                (isUpper, toUpper)
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.List                                (partition)
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Documentation
import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Marshal.Monad
import           Write.Marshal.Struct.Utils               (doesStructContainUnion)
import           Write.Marshal.Util
import           Write.Marshal.Wrap
import           Write.Util

handleWrapper :: Handle -> Either [SpecError] WriteElement
handleWrapper handle = do
  let weName        = hName handle T.<+> "wrapper"
      weBootElement = Nothing
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext (hName handle)))
           pure
           (runWrap $ wrapHandle handle)
  pure WriteElement {..}

wrapHandle
  :: Handle
  -> WrapM (DocMap -> Doc ())
  -- ^ Returns the docs for this handle, and any aliases
wrapHandle Handle{..} = do
  when (hHandleType == NonDispatchable) $
    throwError [Other "Wrapping a non-dispatchable handle"]
  let marshalledName = dropVkType hName
  tellExport (Unguarded (TypeConstructor marshalledName))
  tellExport (Unguarded (Term marshalledName))
  tellDepend (Unguarded (WE.TypeName hName))
  pure $ \_ -> [qci|
    data {marshalledName} = {marshalledName}
      \{ {T.lowerCaseFirst marshalledName}Handle :: {hName}
      }
      deriving (Eq, Ord, Show)
  |]

dropVkType :: Text -> Text
dropVkType = T.dropPrefix' "Vk"
