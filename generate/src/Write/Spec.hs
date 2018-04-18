{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Spec
  ( writeSpec
  ) where

import           Data.Bifunctor
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
          Right ms -> for_ (zip ms (writeModules ms)) $ \(m, s) -> do
            writeFile (T.unpack ((<> ".hs") . T.replace "." "/" $ mName m))
                      (show s)

tShow :: Show a => a -> Text
tShow = T.pack . show

specWriteElements :: Spec -> Validation [SpecError] [WriteElement]
specWriteElements Spec {..} = do
  let
    wHeaderVersion  = writeHeaderVersion sHeaderVersion
    wEnums          = writeEnum <$> sEnums
    -- Take the nub here to deal with duplicate extensions
    wEnumExtensions = uncurry writeEnumExtension <$> nubOrdOn
      (second exName)
      [ (eName e, ex) | e <- sEnums, ex <- eExtensions e ]
    reqs =
      (extRequirements =<< sExtensions)
        ++ (   fRequirements
           =<< [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
           )
    wEnumAliases        = [] -- writeEnumAlias <$> [ ea | r <- reqs, ea <- rEnumAliases r ]
    wConstants          = writeAPIConstant <$> sConstants
    wConstantExtensions = (fmap writeConstantExtension . rConstants =<< reqs)
  wFuncPointers <- eitherToValidation $ traverse writeFuncPointer sFuncPointers
  wHandles      <- eitherToValidation $ traverse writeHandle sHandles
  let getEnumAliasTarget :: Text -> Maybe Text
      getEnumAliasTarget n = do
        a <- find ((== n) . aName) (enumAliases sAliases)
        eitherToMaybe (eName <$> aliasTarget a)
  wCommands <- eitherToValidation
    $ traverse (writeCommand getEnumAliasTarget) sCommands
  wStructs   <- eitherToValidation $ traverse writeStruct sStructs
  wAliases   <- writeAliases sAliases
  wBaseTypes <- eitherToValidation $ traverse writeBaseType sBaseTypes
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
