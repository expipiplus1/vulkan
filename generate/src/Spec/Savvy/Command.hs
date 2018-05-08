{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Command
  ( Command(..)
  , Parameter(..)
  , ParameterLength(..)
  , HandleLevel(..)
  , specCommands
  , commandType
  , lowerArrayToPointer
  ) where

import           Control.Arrow
import           Data.Closure
import           Data.Either.Validation
import           Data.Foldable
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.Text              (Text)
import qualified Data.Text.Extra        as T
import           Data.Traversable

import qualified Spec.Command           as P
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature     (Requirement (..))
import           Spec.Savvy.Handle
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import           Write.Element          (HaskellName (TermName))

data Command = Command
  { cName         :: Text
  , cReturnType   :: Type
  , cParameters   :: [Parameter]
  , cComment      :: Maybe Text
  , cAliases      :: [Text]
    -- ^ The closure of aliases to this command, doesn't include aliases in
    -- extensions
  , cCommandLevel :: Maybe HandleLevel
    -- ^ Some commands are part of no level such as vkCreateInstance
  , cPlatform     :: Maybe Text
    -- ^ The platform this command runs on if it is not universal
  , cSuccessCodes :: Maybe [Text]
  , cErrorCodes   :: Maybe [Text]
  }
  deriving (Show)

data Parameter = Parameter
  { pName       :: Text
  , pType       :: Type
  , pLength     :: Maybe ParameterLength
  , pIsOptional :: Maybe [Bool]
  }
  deriving (Show)

data ParameterLength
  = NullTerminated
  | NamedLength Text
  | NamedMemberLength Text Text
    -- ^ The length is specified by a member of another (struct) parameter, an
    -- example is vkAllocateCommandBuffers
  deriving (Show)

specCommands
  :: TypeParseContext
  -> P.Spec
  -> [Handle]
  -> [Extension]
  -> Validation [SpecError] [Command]
specCommands pc P.Spec {..} handles extensions
  = let
      commandAliases :: [(Text, Text)]
      commandAliases =
        [ (caAlias, caName) | P.CommandAlias {..} <- sCommandAliases ]
      aliasMap :: MultiMap.MultiMap Text Text
      aliasMap = MultiMap.fromList commandAliases
      commandLevel' :: [Parameter] -> Maybe HandleLevel
      commandLevel' = commandLevel handles
    in
      for sCommands $ \P.Command {..} -> do
        ret <- eitherToValidation $ stringToTypeExpected pc cName cReturnType
        ps  <- for cParameters $ \P.Parameter {..} -> eitherToValidation $ do
          t <- stringToTypeExpected pc pName pType
          let pLength = case pLengths of
                Nothing                  -> Nothing
                Just ["null-terminated"] -> Just NullTerminated
                Just [l] | [param, member] <- T.splitOn "::" l ->
                  Just (NamedMemberLength param member)
                Just [l] -> Just (NamedLength l)
                Just _ -> error "TODO: Multiple lengths"
          pure Parameter {pType = t, ..}
        pure
          $ let cAliases =
                  closeNonReflexiveL (`MultiMap.lookup` aliasMap) [cName]
                cCommandLevel = commandLevel' ps
                cPlatform     = listToMaybe
                  [ p
                  | e          <- extensions
                  , TermName n <- rRequiredNames =<< extRequirements e
                  , n == cName
                  , Just p <- [extPlatform e]
                  ]
            in  Command {cReturnType = ret, cParameters = ps, ..}

commandType :: Command -> Type
commandType Command {..} = Proto
  cReturnType
  [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]

lowerArrayToPointer :: Type -> Type
lowerArrayToPointer = \case
  Array c _ t -> Ptr c t
  t           -> t

commandLevel :: [Handle] -> [Parameter] -> Maybe HandleLevel
commandLevel handles =
  let handleMap :: Text -> Maybe Handle
      handleMap = (`Map.lookup` Map.fromList ((hName &&& id) <$> handles))
  in  \case
        Parameter _ (TypeName n) _ _ : _ -> hLevel =<< handleMap n
        _                              -> Nothing
