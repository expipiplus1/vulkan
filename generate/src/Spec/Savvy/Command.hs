{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Command
  ( Command(..)
  , Parameter(..)
  , CommandLevel(..)
  , specCommands
  , commandType
  , lowerArrayToPointer
  ) where

import           Control.Arrow
import           Control.Monad
import           Data.Closure
import           Data.Either.Validation
import           Data.Foldable
import qualified Data.Map               as Map
import qualified Data.MultiMap          as MultiMap
import           Data.Text
import           Data.Traversable

import qualified Spec.Command           as P
import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type
import qualified Spec.Spec              as P

data Command = Command
  { cName         :: Text
  , cReturnType   :: Type
  , cParameters   :: [Parameter]
  , cComment      :: Maybe Text
  , cAliases      :: [Text]
    -- ^ The closure of aliases to this command, doesn't include aliases in
    -- extensions
  , cCommandLevel :: Maybe CommandLevel
  }
  deriving (Show)

data Parameter = Parameter
  { pName :: Text
  , pType :: Type
  }
  deriving (Show)

-- | The "level" of a command, related to what it is dispatched from.
--
-- Some commands are part of no level such as vkCreateInstance
data CommandLevel
  = Instance
  | PhysicalDevice
  | Device
  deriving (Show, Eq)

specCommands :: TypeParseContext -> P.Spec -> [Handle] -> Validation [SpecError] [Command]
specCommands pc P.Spec {..} handles
  = let
      commandAliases :: [(Text, Text)]
      commandAliases =
        [ (caAlias, caName) | P.CommandAlias {..} <- sCommandAliases ]
      aliasMap :: MultiMap.MultiMap Text Text
      aliasMap = MultiMap.fromList commandAliases
      commandLevel' :: [Parameter] -> Maybe CommandLevel
      commandLevel' = commandLevel handles
    in
      for sCommands $ \P.Command {..} -> do
        ret <- eitherToValidation $ stringToTypeExpected pc cName cReturnType
        ps  <- for cParameters $ \P.Parameter {..} -> do
          t <- eitherToValidation $ stringToTypeExpected pc pName pType
          pure Parameter {pType = t, ..}
        pure
          $ let cAliases =
                  closeNonReflexive (`MultiMap.lookup` aliasMap) [cName]
                cCommandLevel = commandLevel' ps
            in  Command {cReturnType = ret, cParameters = ps, ..}

commandType :: Command -> Type
commandType Command {..} = Proto
  cReturnType
  [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]

lowerArrayToPointer :: Type -> Type
lowerArrayToPointer = \case
    Array _ t -> Ptr t
    t         -> t

commandLevel :: [Handle] -> [Parameter] -> Maybe CommandLevel
commandLevel handles =
  let handleMap :: Text -> Maybe Handle
      handleMap = (`Map.lookup` Map.fromList ((hName &&& id) <$> handles))
      handleLevel :: Text -> Maybe CommandLevel
      handleLevel = \case
        "VkInstance"       -> Just Instance
        "VkPhysicalDevice" -> Just PhysicalDevice
        "VkDevice"         -> Just Device
        handleName                  -> do
          h <- handleMap handleName
          asum $ handleLevel <$> hParents h
  in  \case
        Parameter _ (TypeName n) : _ -> handleLevel n
        _                            -> Nothing
