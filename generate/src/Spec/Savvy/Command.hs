{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Savvy.Command
  ( Command(..)
  , Parameter(..)
  , specCommands
  , commandType
  , lowerArrayToPointer
  ) where

import           Data.Closure
import           Data.Either.Validation
import qualified Data.MultiMap          as MultiMap
import           Data.Text
import           Data.Traversable
import qualified Spec.Command           as P
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import qualified Spec.Spec              as P
import qualified Spec.Type              as P

data Command = Command
  { cName       :: Text
  , cReturnType :: Type
  , cParameters :: [Parameter]
  , cComment    :: Maybe Text
  , cAliases    :: [Text]
    -- ^ The closure of aliases to this command, doesn't include aliases in
    -- extensions
  }
  deriving (Show)

data Parameter = Parameter
  { pName :: Text
  , pType :: Type
  }
  deriving (Show)

specCommands :: TypeParseContext -> P.Spec -> Validation [SpecError] [Command]
specCommands pc P.Spec {..} =
  let commandAliases :: [(Text, Text)]
      commandAliases =
        [ (caAlias, caName) | P.CommandAlias {..} <- sCommandAliases ]
      aliasMap :: MultiMap.MultiMap Text Text
      aliasMap = MultiMap.fromList commandAliases
  in  for sCommands $ \P.Command {..} -> do
        cReturnType <- eitherToValidation
          $ stringToTypeExpected pc cName cReturnType
        cParameters <- for cParameters $ \P.Parameter {..} -> do
          pType <- eitherToValidation $ stringToTypeExpected pc pName pType
          pure Parameter {..}
        pure $
          let cAliases = closeNonReflexive (`MultiMap.lookup` aliasMap) [cName]
          in Command {..}

commandType :: Command -> Type
commandType Command {..} = Proto
  cReturnType
  [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]

lowerArrayToPointer :: Type -> Type
lowerArrayToPointer = \case
    Array _ t -> Ptr t
    t         -> t
