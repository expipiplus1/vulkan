{-# LANGUAGE ApplicativeDo     #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards   #-}


module Write.Spec
  ( writeSpec
  ) where

import           Data.Either.Validation
import           Data.Foldable
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

import           Say
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.Spec
-- import           Spec.Savvy.Type
import qualified Spec.Spec                 as P
-- import           Spec.Type
-- import           Write.Bitmask
import           Write.Alias
import           Write.Bespoke
import           Write.Command
import           Write.Constant
import           Write.ConstantExtension
import           Write.Element
import           Write.Extension
import           Write.Feature
import           Write.Handle
import           Write.HeaderVersion
import           Write.Partition
import           Write.Struct
import           Write.Type.Enum
import           Write.Type.FuncPointer

writeSpec :: P.Spec -> IO ()
writeSpec s = do
  case spec s of
    Left  es -> traverse_ (sayErr . prettySpecError) es
    Right s  -> case specWriteElements s of
      Failure es -> do
        sayErr "Failed to generate write elements:"
        traverse_ (sayErr . prettySpecError) es
      Success ws -> do
        let seeds =
              -- Put the extensions before the 1.1 release so they grab names
              -- with priority
              bespokeSeeds
                ++ featureToSeeds (vulkan10Feature (sFeatures s))
                ++ (extensionToSeed <$> sExtensions s)
                ++ featureToSeeds (vulkan11Feature (sFeatures s))
        case partitionElements ws seeds of
          Left es -> do
            sayErr "Failed to partition write elements:"
            traverse_ (sayErr . prettySpecError) es
          -- Right ps -> traverse_ (say . moduleSummary) ps
          Right ps -> traverse_ sayShow ps

bespokeSeeds :: [ModuleSeed]
bespokeSeeds =
  [ ModuleSeed
      "Core"
      [ Type "VkResult"
      , Type "VkStructureType"
      , Pattern "VK_TRUE"
      , Pattern "VK_FALSE"
      ]
  ]

featureToSeeds :: Feature -> [ModuleSeed]
featureToSeeds Feature {..} =
  [ ModuleSeed name rRequiredNames
  | Requirement {..} <- fRequirements
  , Just name <- [rComment]
  , name /= "Header boilerplate"
  , not ("has no API" `T.isSuffixOf` name)
  ]

extensionToSeed :: Extension -> ModuleSeed
extensionToSeed Extension {..} = ModuleSeed extName
                                            (requiredNames <> providedValues)
  where
    requiredNames = rRequiredNames =<< extRequirements
    providedValues =
      Pattern . exName . snd <$> (rEnumExtensions =<< extRequirements)


tShow :: Show a => a -> Text
tShow = T.pack . show

specWriteElements :: Spec -> Validation [SpecError] [WriteElement]
specWriteElements Spec {..} = do
  let
    wHeaderVersion = writeHeaderVersion sHeaderVersion
    wEnums         = writeEnum <$> sEnums
    wExtensions    = writeExtension <$> sExtensions
    wFeatures =
      writeFeature <$> [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
    wConstants = writeAPIConstant <$> sConstants
    reqs =
      (extRequirements =<< sExtensions)
        ++ (   fRequirements
           =<< [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
           )
    wConstantExtensions = (fmap writeConstantExtension . rConstants =<< reqs)
  wFuncPointers <- eitherToValidation $ traverse writeFuncPointer sFuncPointers
  wHandles      <- eitherToValidation $ traverse writeHandle sHandles
  wCommands     <- eitherToValidation $ traverse writeCommand sCommands
  wStructs      <- eitherToValidation $ traverse writeStruct sStructs
  wAliases      <- writeAliases sAliases
  pure $ concat
    [ [wHeaderVersion]
    , bespokeWriteElements
    , wEnums
    , wExtensions
    , wConstants
    , wConstantExtensions
    , wFuncPointers
    , wHandles
    , wCommands
    , wStructs
    , wFeatures
    , wAliases
    ]
