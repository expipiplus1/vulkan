{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards   #-}

module Spec.Savvy.Platform
  ( module Spec.Platform
  , PlatformGuardInfo(..)
  , getModuleGuardInfo
  ) where

import           Control.Arrow          ((&&&))
import           Data.Either.Validation
import qualified Data.Map               as Map
import           Data.Text (Text)
import qualified Data.Text.Extra              as T
import           Data.Semigroup ((<>))
import           Data.Char
import           Control.Bool

import           Spec.Platform
import           Spec.Savvy.Error
import           Spec.Savvy.Extension

data PlatformGuardInfo = PlatformGuardInfo
  { pgiModuleName :: Text
  , pgiPlatform   :: Text
  , pgiGuard      :: Text
  }
  deriving(Eq, Ord)

getModuleGuardInfo
  :: [Extension] -> [Platform] -> Validation [SpecError] [PlatformGuardInfo]
  -- TODO: better representation
  -- ^ (moduleName, guard)
getModuleGuardInfo exts platforms =
  let guardMap = Map.fromList $ (pName &&& pProtect) <$> platforms
      platformToGuard :: Text -> Validation [SpecError] Text
      platformToGuard p = case Map.lookup p guardMap of
        Nothing -> Failure [UnknownPlatform p]
        Just g  -> pure g
  in  sequenceA
        [ PlatformGuardInfo (makeModuleName "Extensions" extName) platform
            <$> platformToGuard platform
        | Extension {..} <- exts
        , Just platform  <- [extPlatform]
        , makeModuleName <- [toModuleName, toCModuleName]
        ]

-- TODO: reduce duplication
toCModuleName :: Text -> Text -> Text
toCModuleName feature n = T.intercalate
  "."
  [ "Graphics"
  , "Vulkan"
  , "C"
  , sectionNameToModuleBaseName feature
  , sectionNameToModuleBaseName n
  ]

toModuleName :: Text -> Text -> Text
toModuleName feature n = T.intercalate
  "."
  [ "Graphics"
  , "Vulkan"
  , sectionNameToModuleBaseName feature
  , sectionNameToModuleBaseName n
  ]

sectionNameToModuleBaseName :: Text -> Text
sectionNameToModuleBaseName = \case
  "Types not directly used by the API" -> "OtherTypes"
  "Header boilerplate"                 -> "OtherTypes"
  t
    | "Promoted from " `T.isPrefixOf` t
    -> T.intercalate "_"
      . fmap (T.replace "+" "and")
      . T.words
      . T.takeWhile (/= ',')
      $ t
    | Just e <- T.dropPrefix "Originally based on" t
    -> ("Promoted_From_" <>) . head . T.words $ e
    | otherwise
    -> T.concat
      . fmap T.upperCaseFirst
      . filter isAllowed
      . T.words
      . T.filter ((not . isPunctuation) <||> (== '_'))
      $ t
    where
      isAllowed n = n `notElem` forbiddenWords
      forbiddenWords = ["commands", "API"]
