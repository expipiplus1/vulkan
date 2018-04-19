{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Alias
  ( Aliases(..)
  , Alias(..)
  , AliasTarget
  , aliasTarget
  , specAliases
  ) where

import           Control.Applicative
import           Control.Arrow            ((&&&))
import           Control.Monad.Fix.Extra
import           Data.Either.Validation
import           Data.Foldable
import qualified Data.Map                 as Map
import           Data.Semigroup
import           Data.Text                (Text)
import           Data.Traversable
import           Prelude                  hiding (Enum)
import qualified Spec.Command             as P
import qualified Spec.Constant            as P
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Feature
import           Spec.Savvy.Handle
import           Spec.Savvy.Struct
import qualified Spec.Spec                as P
import qualified Spec.Type                as P

data Aliases = Aliases
  { commandAliases       :: [Alias Command]
  , enumAliases          :: [Alias Enum]
  , handleAliases        :: [Alias Handle]
  , structAliases        :: [Alias Struct]
  , constantAliases      :: [Alias APIConstant]
  , enumExtensionAliases :: [Alias (Enum, Text)]
    -- (Enum, enumerant name)
  }
  deriving (Show)

data Alias targetType = Alias
  { aName      :: Text
  , aAliasName :: Text
    -- ^ The name of the target
  , aAlias     :: AliasTarget targetType
  }
  deriving (Show)

data AliasTarget targetType
  = AnAlias (Alias targetType)
  | ATarget targetType
  deriving (Show)

aliasTarget :: Alias t -> Either [SpecError] t
aliasTarget = aliasTargetSeen []

aliasTargetSeen :: [Text] -> Alias t -> Either [SpecError] t
aliasTargetSeen seen Alias {..}
  | aName `elem` seen = Left [AliasLoop (aName : seen)]
  | otherwise = case aAlias of
    AnAlias a -> aliasTargetSeen (aName : seen) a
    ATarget t -> pure t

specAliases
  :: P.Spec
  -> [Command]
  -> [Enum]
  -> [Handle]
  -> [Struct]
  -> [APIConstant]
  -> [Requirement]
  -> Validation [SpecError] Aliases
specAliases P.Spec {..} commands enums handles structs constants requirements
  = do
    let typeAliases  = [ ta | P.AnAlias ta <- sTypes ]
        bitmaskTypes = [ bmt | P.ABitmaskType bmt <- sTypes ]
    commandAliases <- makeAliases
      [ (caAlias, caName) | P.CommandAlias {..} <- sCommandAliases ]
      commands
      cName
    enumAliases <-
      (<>)
      <$> makeTypeAliases    typeAliases "enum"       enums eName
      <*> makeBitmaskAliases typeAliases bitmaskTypes enums eName
    handleAliases   <- makeTypeAliases typeAliases "handle" handles hName
    structAliases   <- makeTypeAliases typeAliases "struct" structs sName
    constantAliases <- makeAliases
      [ (P.unConstantAlias name, alias)
        -- ^ TODO: These are constructed backwards :(
      | P.Constant alias (Left name) _ <- sConstants
      ]
      constants
      acName
    enumExtensionAliases <- eitherToValidation $ do
      let enumMap              = Map.fromList ((eName &&& id) <$> enums)
          enumExtensionAliases = rEnumAliases =<< requirements
      enumerantEnums <-
        validationToEither $ for enumExtensionAliases $ \EnumAlias {..} ->
          case Map.lookup eaExtends enumMap of
            Nothing -> Failure [UnknownExtendedEnum eaExtends]
            Just e  -> pure (e, eaAlias)
      validationToEither $ makeAliases
        [ (eaAlias, eaName) | EnumAlias {..} <- enumExtensionAliases ]
        enumerantEnums
        snd
    pure Aliases {..}

makeBitmaskAliases
  :: [P.TypeAlias]
  -> [P.BitmaskType]
  -> [a]
  -- ^ Targets
  -> (a -> Text)
  -- ^ Get target name
  -> Validation [SpecError] [Alias a]
makeBitmaskAliases aliases bmts = makeAliases
  ([ (taAlias, taName) | P.TypeAlias {..} <- aliases, taCategory == "bitmask" ]
  <> [ (alias, bmtName)
     | P.BitmaskType {..} <- bmts
     , Just alias             <- [bmtRequires]
     ]
  )

makeTypeAliases
  :: [P.TypeAlias]
  -> Text
  -- ^ Category
  -> [a]
  -- ^ Targets
  -> (a -> Text)
  -- ^ Get target name
  -> Validation [SpecError] [Alias a]
makeTypeAliases aliases category = makeAliases
  [ (taAlias, taName)
  | P.TypeAlias {..} <- aliases
  , taCategory == category
  ]

makeAliases
  :: [(Text, Text)]
  -- ^ Aliases (target, name)
  -> [a]
  -- ^ Targets
  -> (a -> Text)
  -- ^ Get target name
  -> Validation [SpecError] [Alias a]
makeAliases aliases targets targetName
  = let tMap = Map.fromList ((targetName &&& id) <$> targets)
    in
      eitherToValidation
      $ fixLookupM (snd <$> aliases) aName
      $ \findAlias -> validationToEither $ for aliases $ \(alias, name) -> maybe
          (Failure [UnknownAliasTarget alias name])
          (Success . Alias name alias)
          ((ATarget <$> Map.lookup alias tMap) <|> (AnAlias <$> findAlias alias))
