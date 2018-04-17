{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Write.Seed
  ( specSeeds
  ) where

import           Control.Bool
import           Data.Char
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

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
    -- Put the extensions before the 1.1 release so they grab names with
    -- priority
    ++ (extensionToSeed <$> sExtensions s)
    ++ featureToSeeds (vulkan11Feature (sFeatures s))

bespokeSeeds :: [ModuleSeed]
bespokeSeeds =
  [ ModuleSeed
      (toModuleName "VK_VERSION_1_0" "Core")
      [ TypeName "VkResult"
      , TypeName "VkStructureType"
      , PatternName "VK_TRUE"
      , PatternName "VK_FALSE"
      ]
  ]

featureToSeeds :: Feature -> [ModuleSeed]
featureToSeeds Feature {..} =
  [ ModuleSeed (toModuleName fName name) rRequiredNames
  | Requirement {..} <- fRequirements
  , Just name <- [rComment]
  , name /= "Header boilerplate"
  , not ("has no API" `T.isSuffixOf` name)
  ]

extensionToSeed :: Extension -> ModuleSeed
extensionToSeed Extension {..} = ModuleSeed
  (toModuleName "Extensions" extName)
  (requiredNames <> providedValues)
  where
    requiredNames = rRequiredNames =<< extRequirements
    providedValues =
      PatternName . exName . snd <$> (rEnumExtensions =<< extRequirements)

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
  sectionName                          -> moduleNameSpaces sectionName
    where
      moduleNameSpaces =
        T.concat
          . fmap upperCaseFirst
          . filter isAllowed
          . T.words
          . T.filter ((not . isPunctuation) <||> (== '_'))
      isAllowed n = n `notElem` forbiddenWords
      forbiddenWords = ["commands", "API"]

upperCaseFirst :: Text -> Text
upperCaseFirst = onFirst toUpper

onFirst :: (Char -> Char) -> Text -> Text
onFirst f = \case
  Cons c cs -> Cons (f c) cs
  t         -> t

pattern Cons :: Char -> Text -> Text
pattern Cons c cs <- (T.uncons -> Just (c, cs))
  where Cons c cs = T.cons c cs

-- extensionNameToModuleName :: String -> ModuleName
-- extensionNameToModuleName extensionName
--   | "VK":category:n:ns <- splitOn "_" extensionName
--   = ModuleName $ "Graphics.Vulkan." ++
--                  pascalCase category ++ "." ++
--                  pascalCase (unwords (n:ns))
--   | otherwise
--   = error ("extension name in unexpected format: " ++ extensionName)
