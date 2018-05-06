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
import           Data.Maybe
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
  bespokeSeedsHighPriority
    ++ featureToSeeds (vulkan10Feature (sFeatures s))
    ++ featureToSeeds (vulkan11Feature (sFeatures s))
    ++ (extensionToSeed <$> sExtensions s)
    ++ [dynamicLoaderSeed] -- It's important for this to come after the C
                           -- modules to avoid pulling in every type
    ++ featureToMarshalledSeeds (vulkan10Feature (sFeatures s))
    ++ featureToMarshalledSeeds (vulkan11Feature (sFeatures s))
    ++ (extensionToMarshalledSeed <$> sExtensions s)
    ++ bespokeSeedsLowPriority

bespokeSeedsHighPriority :: [ModuleSeed]
bespokeSeedsHighPriority =
  [ ModuleSeed "Graphics.Vulkan.NamedType" [TypeName "(:::)"]           Nothing
  , ModuleSeed "Graphics.Vulkan.Exception" [TypeName "VulkanException"] Nothing
  , ModuleSeed
    (toCModuleName "Core10" "Core")
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
  , ModuleSeed
    (toModuleName "Core10" "Core")
    [ TypeName "Result"
    , TypeName "StructureType"
    , PatternName "VK_TRUE"
    , PatternName "VK_FALSE"
    , TypeName "Flags"
    , TypeName "Format"
    , TypeName "Bool32"
    , TypeName "ObjectType"
    ]
    Nothing
  ]

bespokeSeedsLowPriority :: [ModuleSeed]
bespokeSeedsLowPriority =
  [ ModuleSeed
    "Graphics.Vulkan.Marshal.Utils"
    [ TypeName "ToCStruct"
    , TypeName "FromCStruct"
    , TypeName "SomeVkStruct"
    , TermName "SomeVkStruct"
    , TermName "withCStructPtr"
    , TermName "fromCStructPtr"
    , TermName "fromCStructPtrElem"
    , TermName "fromSomeVkStruct"
    , TermName "fromSomeVkStructChain"
    , TermName "withSomeVkStruct"
    , TermName "withVec"
    ]
    Nothing
  , ModuleSeed "Graphics.Vulkan.Marshal.Utils.Peek"
               [TermName "peekVkStruct"]
               Nothing
  ]

dynamicLoaderSeed :: ModuleSeed
dynamicLoaderSeed =
  ModuleSeed "Graphics.Vulkan.Dynamic" [TypeName "DeviceCmds"] Nothing

featureToSeeds :: Feature -> [ModuleSeed]
featureToSeeds Feature {..} =
  [ ModuleSeed (toCModuleName (featureModuleName fName) name) rRequiredNames Nothing
  | Requirement {..} <- fRequirements
  , Just name        <- [rComment]
  , name
    `notElem` [ "Header boilerplate"
              , "Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API."
              ]
  , not ("has no API" `T.isSuffixOf` name)
  ]

featureToMarshalledSeeds :: Feature -> [ModuleSeed]
featureToMarshalledSeeds Feature {..} =
  [ ModuleSeed (toModuleName (featureModuleName fName) name)
               (mapMaybe toMarshalledName rRequiredNames)
               Nothing
  | Requirement {..} <- fRequirements
  , Just name        <- [rComment]
  , name
    `notElem` [ "Header boilerplate"
              , "Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API."
              ]
  , not ("has no API" `T.isSuffixOf` name)
  ]

toMarshalledName :: HaskellName -> Maybe HaskellName
toMarshalledName = \case
  TermName    n -> TermName . T.lowerCaseFirst <$> T.dropPrefix "vk" n
  TypeName    n -> TypeName . T.upperCaseFirst <$> T.dropPrefix "Vk" n
  PatternName n -> pure $ PatternName n

extensionToSeed :: Extension -> ModuleSeed
extensionToSeed Extension {..} = ModuleSeed
  (toCModuleName "Extensions" extName)
  (requiredNames <> providedValues)
  extPlatform
  where
    requiredNames = rRequiredNames =<< extRequirements
    providedValues =
      PatternName . exName . snd <$> (rEnumExtensions =<< extRequirements)

extensionToMarshalledSeed :: Extension -> ModuleSeed
extensionToMarshalledSeed Extension {..} = ModuleSeed
  (toModuleName "Extensions" extName)
  requiredNames
  extPlatform
  where
    requiredNames = mapMaybe toMarshalledName (rRequiredNames =<< extRequirements)

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
