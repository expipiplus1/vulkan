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
import qualified Spec.Spec                 as P
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
import           Write.Module
import           Write.Partition
import           Write.Seed
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
        let seeds = specSeeds s
        case partitionElements ws seeds of
          Left es -> do
            sayErr "Failed to partition write elements:"
            traverse_ (sayErr . prettySpecError) es
          -- Right ps -> traverse_ (say . moduleSummary) ps
          Right ms ->
            for_ (zip ms (writeModules ms)) $ \(m, s) -> do
              writeFile (T.unpack ((<> ".hs") . T.replace "." "/" $ mName m)) (show s)

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
