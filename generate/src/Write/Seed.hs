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
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
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

bespokeSeeds :: [ModuleSeed]
bespokeSeeds =
  [ ModuleSeed
      (toModuleName "Version10" "Core")
      [ TypeName "VkResult"
      , TypeName "VkStructureType"
      , PatternName "VK_TRUE"
      , PatternName "VK_FALSE"
      , TypeName "VkFlags"
      , TypeName "VkFormat"
      , TypeName "VkBool32"
      , TypeName "VkObjectType"
      ]
  ]

featureToSeeds :: Feature -> [ModuleSeed]
featureToSeeds Feature {..} =
  [ ModuleSeed (toModuleName (featureModuleName fName) name) rRequiredNames
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
  where
    requiredNames = rRequiredNames =<< extRequirements
    providedValues =
      PatternName . exName . snd <$> (rEnumExtensions =<< extRequirements)

featureModuleName :: Text -> Text
featureModuleName = \case
  t
    | Just (major, minor) <- match parseVersion (T.unpack t)
    -> "Version" <> major <> minor
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
    | Just e <- dropPrefix "Originally based on" t
    -> ("Promoted_From_" <>) . head . T.words $ e
    | otherwise
    -> T.concat
      . fmap upperCaseFirst
      . filter isAllowed
      . T.words
      . T.filter ((not . isPunctuation) <||> (== '_'))
      $ t
    where
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

dropPrefix :: Text -> Text -> Maybe Text
dropPrefix prefix s = if prefix `T.isPrefixOf` s
                        then Just (T.drop (T.length prefix) s)
                        else Nothing

dropSuffix :: Text -> Text -> Maybe Text
dropSuffix suffix s = if suffix `T.isSuffixOf` s
                        then Just (T.take (T.length s - T.length suffix) s)
                        else Nothing

-- | If the suffix doesn't match: return the original string
dropSuffix' :: Text -> Text -> Text
dropSuffix' suffix s = fromMaybe s (dropSuffix suffix s)

-- extensionNameToModuleName :: String -> ModuleName
-- extensionNameToModuleName extensionName
--   | "VK":category:n:ns <- splitOn "_" extensionName
--   = ModuleName $ "Graphics.Vulkan." ++
--                  pascalCase category ++ "." ++
--                  pascalCase (unwords (n:ns))
--   | otherwise
--   = error ("extension name in unexpected format: " ++ extensionName)
