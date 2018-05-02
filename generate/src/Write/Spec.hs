{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Spec
  ( writeSpec
  ) where

import           Debug.Trace

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Either.Extra
import           Data.Either.Validation
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc
import           Say
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified System.IO.Strict          as Strict
import           System.ProgressBar

import           Documentation
import           Spec.Savvy.Alias
import           Spec.Savvy.APIConstant
import           Spec.Savvy.BaseType
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Feature
import           Spec.Savvy.Handle
import           Spec.Savvy.Platform
import           Spec.Savvy.Spec
import           Spec.Savvy.Struct
import           Write.Alias
import           Write.BaseType
import           Write.Bespoke
import           Write.Cabal
import           Write.Command
import           Write.Constant
import           Write.ConstantExtension
import           Write.Element
import           Write.EnumExtension
import           Write.Handle
import           Write.HeaderVersion
import           Write.Loader
import           Write.Marshal.Struct
import           Write.Module
import           Write.Module.Aggregate
import           Write.Partition
import           Write.Seed
import           Write.Struct
import           Write.Type.Enum
import           Write.Type.FuncPointer
import           Write.Wrapper

-- TODO: Better error handling
writeSpec
  :: (Documentee -> Maybe Documentation)
  -- ^ Documentation
  -> FilePath
  -- ^ Output Directory
  -> FilePath
  -- ^ Cabal output path
  -> Spec
  -> IO ()
writeSpec docs outDir cabalPath s = (printErrors =<<) $ runExceptT $ do
  ws <- ExceptT . pure . validationToEither $ specWriteElements s
  wrapperWriteElements <-
    ExceptT . pure . validationToEither $ specWrapperWriteElements s
  let
    seeds          = specSeeds s
    -- wrapperModule = Module "Graphics.Vulkan.Wrapped" (snd wrapperWriteElements) [] []
    wrapperModule  = Module "Graphics.Vulkan.Wrapped" [] [] []
    enabledStructs = filter
      ((`notElem` ignoredUnexportedNames) . TypeName . sName)
      (sStructs s)
    structWrapperModule = Module
      "Graphics.Vulkan.Marshal"
      (vkStructWriteElement (enabledStructs) : fst wrapperWriteElements)
      []
      []
  partitionedModules <- ExceptT . pure $ partitionElements ws seeds
  let ms = structWrapperModule : wrapperModule : partitionedModules
  platformGuards <- ExceptT . pure . validationToEither $ getModuleGuardInfo
    (sExtensions s)
    (sPlatforms s)
  let aggs = makeAggregateModules platformGuards ms
  liftIO $ writeFile
    cabalPath
    (show (writeCabal (ms ++ aggs) (sPlatforms s) platformGuards))
  liftIO (saveModules docs outDir (ms ++ aggs)) >>= \case
    [] -> pure ()
    es -> throwError es

printErrors :: Either [SpecError] () -> IO ()
printErrors = \case
  Left es -> traverse_ (sayErr . prettySpecError) es
  Right a -> pure a

saveModules
  :: (Documentee -> Maybe Documentation)
  -> FilePath
  -- ^ Output directory
  -> [Module]
  -> IO [SpecError]
saveModules getDoc outDir ms = concat
  <$> withProgress 1 saveModule (zip ms (writeModules getDoc ms))
  where
    saveModule :: (Module, Doc ()) -> IO [SpecError]
    saveModule (Module {..}, doc) = do
      let filename =
            outDir </> T.unpack ((<> ".hs") . T.replace "." "/" $ mName)
          dir = takeDirectory filename
      createDirectoryIfMissing True     dir
      writeFileIfChanged       filename (show doc)
      pure []

    writeFileIfChanged filename contents = readFileMay filename >>= \case
      Just existing | existing == contents -> pure ()
      _ -> writeFile filename contents

    readFileMay :: FilePath -> IO (Maybe String)
    readFileMay f = (Just <$> Strict.readFile f) `catchIOError` const (pure Nothing)

specWrapperWriteElements
  :: Spec -> Validation [SpecError] ([WriteElement], [WriteElement])
specWrapperWriteElements Spec {..} = do
  let isHandle = (`Set.member` Set.fromList (hName <$> sHandles))
      isBitmask =
        (`Set.member` Set.fromList
          [ n
          | Enum {..} <- sEnums
          , n         <- eName : eAliases
          , eType == EnumTypeBitmask
          ]
        )
      isStruct        = (`Set.member` Set.fromList (sName <$> sStructs))
      -- TODO: Filter in a better way
      enabledCommands = filter
        ((`notElem` ignoredUnexportedNames) . TermName . cName)
        sCommands

      enabledStructs =
        filter ((`notElem` ignoredUnexportedNames) . TypeName . sName) sStructs

  let (commandMarshalErrors, commandWrappers) =
        partitionEithers $ commandWrapper isHandle isBitmask <$> enabledCommands
      (structMarshalErrors, structWrappers) =
        partitionEithers
          $   structWrapper isHandle isBitmask isStruct enabledStructs
          <$> enabledStructs
  -- TODO: Warn properly on unwrapped command
  _ <- traverse_ traceShowM structMarshalErrors
  pure (structWrappers, commandWrappers)

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
      fmap (writeConstantExtension getEnumerantEnumName) . rConstants =<< reqs
  wFuncPointers <- eitherToValidation $ traverse writeFuncPointer sFuncPointers
  wHandles      <- eitherToValidation $ traverse writeHandle sHandles
  wCommands     <- eitherToValidation
    $ traverse (writeCommand getEnumAliasTarget) sCommands
  wStructs   <- eitherToValidation $ traverse writeStruct sStructs
  wAliases   <- writeAliases sAliases
  wBaseTypes <-
    let isAllowedBaseType bt = btName bt /= "VkBool32"
    in eitherToValidation $ traverse writeBaseType (filter isAllowedBaseType sBaseTypes)
  wLoader <- eitherToValidation $ writeLoader getEnumAliasTarget sPlatforms sCommands
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
    , [wLoader]
    ]
