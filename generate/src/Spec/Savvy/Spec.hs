{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Spec
  ( Spec(..)
  , spec
  ) where

import           Control.Applicative
import           Data.Either.Validation
import           Data.List
import qualified Data.Map                 as Map
import           Data.Monoid              (Endo (..))
import           Data.Text                (Text)
import           Prelude                  hiding (Enum)
import           Spec.Savvy.Alias
import           Spec.Savvy.APIConstant
import           Spec.Savvy.BaseType
import           Spec.Savvy.Command
import           Spec.Savvy.Define
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.FuncPointer
import           Spec.Savvy.Handle
import           Spec.Savvy.HeaderVersion
import           Spec.Savvy.Platform
import           Spec.Savvy.Preprocess
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Packing
import           Spec.Savvy.TypeAlias
import qualified Spec.Spec                as P

data Spec = Spec
  { sHeaderVersion :: Word
  , sEnums         :: [Enum]
  , sTypeAliases   :: [TypeAlias]
  , sConstants     :: [APIConstant]
  , sFuncPointers  :: [FuncPointer]
  , sHandles       :: [Handle]
  , sFeatures      :: Features
  , sExtensions    :: [Extension]
  , sCommands      :: [Command]
  , sStructs       :: [Struct]
  , sAliases       :: Aliases
  , sBaseTypes     :: [BaseType]
  , sPlatforms     :: [Platform]
  }
  deriving (Show)

spec :: P.Spec -> Either [SpecError] Spec
spec s = do
  let defines = specDefines s
  preprocess <- createPreprocessor defines
  pc         <- specParserContext s
  (sHeaderVersion, sEnums, sTypeAliases, sConstants, sFuncPointers, sHandles, sFeatures, allExtensions, sBaseTypes) <-
    validationToEither
    $   (,,,,,,,,)
    <$> specHeaderVersion preprocess
    <*> specEnums s
    <*> specTypeAliases s
    <*> specConstants s
    <*> specFuncPointers pc s
    <*> specHandles preprocess pc s
    <*> specFeatures s
    <*> specExtensions s
    <*> specBaseTypes pc s
  let
    getType t =
      (TypeName <$> getAlias1 sTypeAliases t)
        <|> getFuncPointer sFuncPointers t
        <|> getEnum        sEnums        t
        <|> getHandle      sHandles      t
    tc = TypeContext pc
                     (Endo (typeSize getType (getConstantValue sConstants)))
                     (Endo (typeAlignment getType))
                     preprocess

    enabledExtensions = filter ((/= Disabled) . extSupported) allExtensions
    sExtensions       = enabledExtensions
    sPlatforms        = P.sPlatforms s

    requirements =
      (extRequirements =<< sExtensions)
        ++ (   fRequirements
           =<< [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
           )

  (sCommands, sStructs) <-
    validationToEither
    $   (,)
    <$> specCommands pc s sHandles sExtensions
    <*> specStructs tc s

  sAliases <- validationToEither
    $ specAliases s sCommands sEnums sHandles sStructs sConstants requirements
  pure Spec {..}

getConstantValue :: [APIConstant] -> Text -> Maybe Word
getConstantValue cs t = do
  c <- find ((== t) . acName) cs
  case acValue c of
    IntegralValue k -> Just k
    _               -> Nothing

getHandle :: [Handle] -> Text -> Maybe Type
getHandle hs = (`Map.lookup` m)
  where m = Map.fromList [ (hName, hType) | Handle {..} <- hs ]

getAlias1 :: [TypeAlias] -> Text -> Maybe Text
getAlias1 as = (`Map.lookup` m)
  where m = Map.fromList [ (taName, taAlias) | TypeAlias {..} <- as ]

getFuncPointer :: [FuncPointer] -> Text -> Maybe Type
getFuncPointer fs t = fpType <$> find ((== t) . fpName) fs

getEnum :: [Enum] -> Text -> Maybe Type
getEnum es = (`Map.lookup` m)
  where
    m = Map.fromList
      [ (name, type')
      | Enum {..} <- es
      , name <- eName : eAliases
      , let type' = case eType of
              EnumTypeBitmask -> TypeName "uint32_t"
              EnumTypeEnum    -> Int
      ]
