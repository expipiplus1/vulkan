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
import           Data.Foldable
import           Data.Maybe
import qualified Data.MultiMap             as MultiMap
import           Data.Text                 (Text)
import qualified Data.Text.Extra           as T
import           Data.Text.Prettyprint.Doc
import           Prelude                   hiding (Enum)
import           Text.Regex.Applicative

import           Spec.Savvy.Enum
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.Spec
import           Write.Element
import           Write.Partition

specSeeds :: Spec -> [ModuleSeed]
specSeeds s =
  let aliasNames = enumAliasNames (sEnums s)
      bespokeSeedNames =
        msName <$> (bespokeSeedsHighPriority ++ bespokeSeedsLowPriority)
      -- Filter any extracted seeds for which we have bespoke seeds
      nonBespokeSeeds =
        filter ((`notElem` bespokeSeedNames) . msName)
          $  featureToSeeds (vulkan10Feature (sFeatures s))
          ++ featureToSeeds (vulkan11Feature (sFeatures s))
          ++ (extensionToSeed <$> sExtensions s)
          ++ [dynamicLoaderSeed] -- It's important for this to come after the C
                                 -- modules to avoid pulling in every type
          ++ featureToMarshalledSeeds aliasNames (vulkan10Feature (sFeatures s))

          ++ featureToMarshalledSeeds aliasNames (vulkan11Feature (sFeatures s))
          ++ (extensionToMarshalledSeed <$> sExtensions s)
  in  bespokeSeedsHighPriority ++ nonBespokeSeeds ++ bespokeSeedsLowPriority

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
    , TypeName "VkVendorId"
    ]
    Nothing
  , ModuleSeed
    (toModuleName "Core10" "Core")
    [ TypeName "Result"
    , TypeName "StructureType"
    , TypeName "Format"
    , TypeName "ObjectType"
    , TermName "bool32ToBool"
    , TermName "boolToBool32"
    ]
    Nothing
  , ModuleSeed (toCModuleName "Core11" "Version")
               [PatternName "VK_API_VERSION_1_1"]
               Nothing
  ]

bespokeSeedsLowPriority :: [ModuleSeed]
bespokeSeedsLowPriority =
  [ ModuleSeed "Graphics.Vulkan.Marshal.Utils" [TermName "withVec"] Nothing
  , ModuleSeed
    "Graphics.Vulkan.Marshal.SomeVkStruct"
    [ TypeName "ToCStruct"
    , TypeName "FromCStruct"
    , TypeName "SomeVkStruct"
    , TermName "SomeVkStruct"
    , TermName "fromSomeVkStruct"
    , TermName "fromSomeVkStructChain"
    , TermName "withSomeVkStruct"
    , TermName "peekVkStruct"
    , TermName "withCStructPtr"
    , TermName "fromCStructPtr"
    , TermName "fromCStructPtrElem"
    ]
    Nothing
  , ModuleSeed
    (toModuleName "Core10" "Version")
    [ PatternName "VK_MAKE_VERSION"
    , PatternName "VK_API_VERSION_1_0"
    , PatternName "VK_MAKE_VERSION"
    , TermName "_VK_VERSION_MAJOR"
    , TermName "_VK_VERSION_MINOR"
    , TermName "_VK_VERSION_PATCH"
    ]
    Nothing
  , ModuleSeed
    (toModuleName "Core11" "Version")
    [ PatternName "VK_API_VERSION_1_1"
    ]
    Nothing
  ]

dynamicLoaderSeed :: ModuleSeed
dynamicLoaderSeed =
  ModuleSeed "Graphics.Vulkan.C.Dynamic" [TypeName "DeviceCmds"] Nothing

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

featureToMarshalledSeeds :: (HaskellName -> [HaskellName]) -> Feature -> [ModuleSeed]
featureToMarshalledSeeds aliasNames Feature {..} =
  [ ModuleSeed (toModuleName (featureModuleName fName) name)
               (mapMaybe toMarshalledName requiredNames)
               Nothing
  | Requirement {..} <- fRequirements
  , let requiredNames = rRequiredNames ++ (aliasNames =<< rRequiredNames)
  , Just name        <- [rComment]
  , name
    `notElem` [ "Header boilerplate"
              , "Types not directly used by the API. Include e.g. structs that are not parameter types of commands, but still defined by the API."
              ]
  , not ("has no API" `T.isSuffixOf` name)
  ]

toMarshalledName :: HaskellName -> Maybe HaskellName
toMarshalledName = \case
  TermName    n -> asum
    [ TermName . T.lowerCaseFirst <$> T.dropPrefix "vk" n
    , pure $ TermName n
    ]
  TypeName    n -> asum
    [ TypeName . T.upperCaseFirst <$> T.dropPrefix "Vk" n
    , pure $ TypeName n
    ]
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

enumAliasNames :: [Enum] -> HaskellName -> [HaskellName]
enumAliasNames es = (`MultiMap.lookup` m)
  where
    m = MultiMap.fromList
      [ (k, v)
      | Enum {..} <- es
      , let k = TypeName eName
      , v <- TypeName <$> eAliases
      ]
