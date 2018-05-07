{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Handle
  ( Handle(..)
  , HandleType(..)
  , specHandles
  ) where

import           Control.Monad.Except
import           Data.Closure
import           Data.Either.Validation
import qualified Data.HashSet           as HashSet
import qualified Data.MultiMap          as MultiMap
import           Data.Text
import qualified Data.Text.Extra        as T
import           Language.C.Types.Parse
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data Handle = Handle
  { hName       :: Text
  , hType       :: Type
  , hAliases    :: [Text]
  , hParents    :: [Text]
  , hHandleType :: HandleType
  }
  deriving (Show)

data HandleType = NonDispatchable | Dispatchable
  deriving (Eq, Show)

specHandles
  :: (Text -> Either [SpecError] Text)
  -- ^ Preprocessor
  -> TypeParseContext
  -> P.Spec
  -> Validation [SpecError] [Handle]
specHandles preprocess pc P.Spec {..} =
  let parsedHandles = [ h | P.AHandleType h <- sTypes ]
      handleNames   = [ htName | P.HandleType {..} <- parsedHandles ]
      pc'           = CParserContext
        (cpcIdentName pc)
        (HashSet.filter ((`notElem` handleNames) . pack . unCIdentifier)
                        (cpcTypeNames pc)
        )
        (cpcParseIdent pc)
        (cpcIdentToString pc)
      handleAliases :: [(Text, Text)]
      handleAliases =
        [ (taAlias, taName)
        | P.AnAlias P.TypeAlias {..} <- sTypes
        , taCategory == "handle"
        ]
      aliasMap = MultiMap.fromList handleAliases
      getAliases s = closeNonReflexiveL (`MultiMap.lookup` aliasMap) [s]
  in  sequenceA
        [ eitherToValidation
          $   Handle htName
          <$> (stringToTypeExpected pc' htName =<< preprocess htType)
          <*> pure (getAliases htName)
          <*> pure htParents
          <*> macroToHandleType htMacro
        | P.HandleType {..} <- parsedHandles
        ]

macroToHandleType :: Text -> Either [SpecError] HandleType
macroToHandleType t
  | "VK_DEFINE_HANDLE" `T.isPrefixOf` t
  = pure Dispatchable
  | "VK_DEFINE_NON_DISPATCHABLE_HANDLE" `T.isPrefixOf` t
  = pure NonDispatchable
  | otherwise
  = throwError [Other "Couldn't determine handle type"]

