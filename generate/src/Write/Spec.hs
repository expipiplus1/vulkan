{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Spec
  ( writeSpec
  ) where

import           Data.Bifunctor
import System.Directory
import System.FilePath
import           Data.Either.Extra
import           Data.Either.Validation
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Say

import           Spec.Savvy.Alias
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.BaseType
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Feature
import           Spec.Savvy.Spec
import qualified Spec.Spec                 as P
import           Write.Alias
import           Write.BaseType
import           Write.Bespoke
import           Write.Command
import           Write.Constant
import           Write.ConstantExtension
import           Write.Element
import           Write.EnumAlias
import           Write.EnumExtension
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
          Right ms -> saveModules ms >>= \case
            [] -> pure ()
            es -> do
              sayErr "Failed to write files:"
              traverse_ (sayErr . prettySpecError) es

saveModules :: [Module] -> IO [SpecError]
saveModules ms = concat <$> (traverse saveModule (zip ms (writeModules ms)))
  where
    saveModule :: (Module, Doc ()) -> IO [SpecError]
    saveModule (Module {..}, doc) = do
      let filename = (T.unpack ((<> ".hs") . T.replace "." "/" $ mName))
          dir      = takeDirectory filename
      createDirectoryIfMissing True     dir
      writeFile                filename (show doc)
      pure []

tShow :: Show a => a -> Text
tShow = T.pack . show

specWriteElements :: Spec -> Validation [SpecError] [WriteElement]
specWriteElements Spec {..} = do
  let
    -- All requirement in features and specs
    reqs =
      (extRequirements =<< sExtensions)
        ++ (   fRequirements
           =<< [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
           )

    getEnumAliasTarget :: Text -> Maybe Text
    getEnumAliasTarget n = do
      a <- find ((== n) . aName) (enumAliases sAliases)
      eitherToMaybe (eName <$> aliasTarget a)

    getEnumerantEnumName :: Text -> Maybe Text
    getEnumerantEnumName enumerantName = listToMaybe
      [ eName e
      | e <- sEnums
      , enumerantName
        `elem` ((eeName <$> eElements e) <> (exName <$> eExtensions e))
      ]

    wHeaderVersion  = writeHeaderVersion sHeaderVersion
    wEnums          = writeEnum <$> sEnums
    -- Take the nub here to deal with duplicate extensions
    wEnumExtensions = uncurry writeEnumExtension <$> nubOrdOn
      (second exName)
      [ (eName e, ex) | e <- sEnums, ex <- eExtensions e ]
    wEnumAliases = [] -- writeEnumAlias <$> [ ea | r <- reqs, ea <- rEnumAliases r ]
    wConstants   =
      let isAllowedConstant c = acName c `notElem` ["VK_TRUE", "VK_FALSE"]
      in writeAPIConstant <$> filter isAllowedConstant sConstants
    wConstantExtensions =
      (fmap (writeConstantExtension getEnumerantEnumName) . rConstants =<< reqs)
  wFuncPointers <- eitherToValidation $ traverse writeFuncPointer sFuncPointers
  wHandles      <- eitherToValidation $ traverse writeHandle sHandles
  wCommands     <- eitherToValidation
    $ traverse (writeCommand getEnumAliasTarget) sCommands
  wStructs   <- eitherToValidation $ traverse writeStruct sStructs
  wAliases   <- writeAliases sAliases
  wBaseTypes <-
    let isAllowedBaseType bt = btName bt /= "VkBool32"
    in eitherToValidation $ traverse writeBaseType (filter isAllowedBaseType sBaseTypes)
  pure $ concat
    [ [wHeaderVersion]
    , bespokeWriteElements
    , wEnums
    , wEnumExtensions
    , wEnumAliases
    , wConstants
    , wConstantExtensions
    , wFuncPointers
    , wHandles
    , wCommands
    , wStructs
    , wAliases
    , wBaseTypes
    ]
