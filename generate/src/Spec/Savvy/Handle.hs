{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}

module Spec.Savvy.Handle
  ( Handle(..)
  , HandleType(..)
  , HandleLevel(..)
  , specHandles
  ) where

import           Control.Arrow
import           Control.Monad.Except
import           Data.Closure
import           Data.Either.Validation
import           Data.Foldable
import qualified Data.HashSet           as HashSet
import qualified Data.Map               as Map
import qualified Data.MultiMap          as MultiMap
import           Data.Text
import qualified Data.Text.Extra        as T
import           Data.Tuple
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
  , hLevel      :: Maybe HandleLevel
  }
  deriving (Show)

data HandleType = NonDispatchable | Dispatchable
  deriving (Eq, Show)

-- | The "level" of a handle, related to what it is descended from.
data HandleLevel
  = Instance
  | PhysicalDevice
  | Device
  deriving (Show, Eq)

specHandles
  :: (Text -> Either [SpecError] Text)
  -- ^ Preprocessor
  -> TypeParseContext
  -> P.Spec
  -> Validation [SpecError] [Handle]
specHandles preprocess pc P.Spec {..}
  = let
      parsedHandles = [ h | P.AHandleType h <- sTypes ]
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
      getLevel = handleLevel
        parsedHandles
        (`Map.lookup` Map.fromList (swap <$> handleAliases))
    in
      sequenceA
        [ eitherToValidation
          $   Handle htName
          <$> (stringToTypeExpected pc' htName =<< preprocess htType)
          <*> pure (getAliases htName)
          <*> pure htParents
          <*> macroToHandleType htMacro
          <*> pure (getLevel h)
        | h@P.HandleType {..} <- parsedHandles
        ]

macroToHandleType :: Text -> Either [SpecError] HandleType
macroToHandleType t
  | "VK_DEFINE_HANDLE" `T.isPrefixOf` t
  = pure Dispatchable
  | "VK_DEFINE_NON_DISPATCHABLE_HANDLE" `T.isPrefixOf` t
  = pure NonDispatchable
  | otherwise
  = throwError [Other "Couldn't determine handle type"]

handleLevel
  :: [P.HandleType] -> (Text -> Maybe Text) -> P.HandleType -> Maybe HandleLevel
handleLevel handles resolveAlias
  = let
      handleMap :: Text -> Maybe P.HandleType
      handleMap = (`Map.lookup` Map.fromList ((P.htName &&& id) <$> handles))
      level :: Text -> Maybe HandleLevel
      level = \case
        "VkInstance"       -> Just Instance
        "VkPhysicalDevice" -> Just PhysicalDevice
        "VkDevice"         -> Just Device
        handleName         -> do
          h <- handleMap handleName
          asum
            $ (level =<< resolveAlias (P.htName h))
            : (level <$> P.htParents h)
    in
      level . P.htName
