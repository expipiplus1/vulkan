{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Seed
  ( specSeeds
  ) where

import           Control.Bool
import           Data.Char
import           Data.Text                 (Text)
import qualified Data.Text.Extra           as T
import           Data.Text.Prettyprint.Doc
import           Text.Regex.Applicative

import           Spec.Savvy.Enum
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.Spec
import           Write.Element
import           Write.Partition

specSeeds :: Spec -> [ModuleSeed]
specSeeds s =
  bespokeSeeds
    ++ featureToSeeds (vulkan10Feature (sFeatures s))
    ++ featureToSeeds (vulkan11Feature (sFeatures s))
    ++ (extensionToSeed <$> sExtensions s)
    ++ [dynamicLoaderSeed] -- It's very important for this to come last

bespokeSeeds :: [ModuleSeed]
bespokeSeeds =
  [ ModuleSeed "Graphics.Vulkan.NamedType" [TypeName "(:::)"] Nothing
  , ModuleSeed
    (toModuleName "Core10" "Core")
    [ TypeName "VkResult"
    , TypeName "VkStructureType"
    , PatternName "VK_TRUE"
    , PatternName "VK_FALSE"
    , TypeName "VkFlags"
    , TypeName "VkFormat"
    , TypeName "VkBool32"
    , TypeName "VkObjectType"
    ]
    Nothing
  ]

dynamicLoaderSeed :: ModuleSeed
dynamicLoaderSeed =
  ModuleSeed "Graphics.Vulkan.Dynamic" [TypeName "DeviceCmds"] Nothing

featureToSeeds :: Feature -> [ModuleSeed]
featureToSeeds Feature {..} =
  [ ModuleSeed (toModuleName (featureModuleName fName) name) rRequiredNames Nothing
  | Requirement {..} <- fRequirements
  , Just name        <- [rComment]
  , name
    `notElem` [ "Header boilerplate"
              , "Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API."
              ]
  , not ("has no API" `T.isSuffixOf` name)
  ]

extensionToSeed :: Extension -> ModuleSeed
extensionToSeed Extension {..} = ModuleSeed
  (toModuleName "Extensions" extName)
  (requiredNames <> providedValues)
  extPlatform
  where
    requiredNames = rRequiredNames =<< extRequirements
    providedValues =
      PatternName . exName . snd <$> (rEnumExtensions =<< extRequirements)

featureModuleName :: Text -> Text
featureModuleName = \case
  t
    | Just (major, minor) <- match parseVersion (T.unpack t)
    -> "Core" <> major <> minor
    | otherwise
    -> t
  where
    digits :: RE Char Text
    digits = T.pack <$> many (psym isDigit)
    parseVersion :: RE Char (Text, Text)
    parseVersion = "VK_VERSION_" *> ((,) <$> digits <*> ("_" *> digits))

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
