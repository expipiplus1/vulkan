{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Spec
  ( writeSpec
  ) where

import           Control.Arrow                            ( (&&&) )
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Either.Extra
import           Data.Either.Validation
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map                      as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc         hiding ( brackets )
import           Data.Text.Prettyprint.Doc.Render.String
import           Say
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified System.IO.Strict              as Strict
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
import           Write.Marshal.Aliases
import           Write.Marshal.Bracket
import           Write.Marshal.Exception
import           Write.Marshal.Handle
import           Write.Marshal.SomeVkStruct
import           Write.Marshal.Struct
import           Write.Marshal.Struct.Utils
import           Write.Marshal.Util
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
  cWriteElements <- ExceptT . pure . validationToEither $ specCWriteElements s
  marshalledWriteElements <-
    ExceptT . pure . validationToEither $ specWrapperWriteElements s

  let seeds = specSeeds s
      -- TODO:
      Just vkResultEnum = find ((== "VkResult") . eName) (sEnums s)
      ws =
        [ vkExceptionWriteElement docs vkResultEnum
          ]
          ++ cWriteElements
          ++ marshalledWriteElements
  partitionedModules <- ExceptT . pure $ partitionElements ws seeds
  platformGuards     <- ExceptT . pure . validationToEither $ getModuleGuardInfo
    (sExtensions s)
    (sPlatforms s)
  let aggs = makeAggregateModules platformGuards partitionedModules
  liftIO $ writeFile
    cabalPath
    (show
      (writeCabal (partitionedModules ++ aggs) (sPlatforms s) platformGuards)
    )
  liftIO (saveModules docs outDir (partitionedModules ++ aggs)) >>= \case
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
    saveModule :: (Module, (Doc (), Maybe (Doc ()))) -> IO [SpecError]
    saveModule (Module {..}, (doc, docBoot)) = do
      let filename =
            outDir </> T.unpack ((<> ".hs") . T.replace "." "/" $ mName)
          bootFilename = filename -<.> "hs-boot"
          dir          = takeDirectory filename
      createDirectoryIfMissing True     dir
      writeFileIfChanged       filename (renderWide doc)
      case docBoot of
        Nothing -> pure ()
        Just d  -> writeFileIfChanged bootFilename (show d)
      pure []

    writeFileIfChanged filename contents = readFileMay filename >>= \case
      Just existing | existing == contents -> pure ()
      _ -> writeFile filename contents

    readFileMay :: FilePath -> IO (Maybe String)
    readFileMay f =
      (Just <$> Strict.readFile f) `catchIOError` const (pure Nothing)

-- Render a Doc with very wide columns
renderWide :: Doc () -> String
renderWide = renderString . layoutPretty (LayoutOptions Unbounded)

specWrapperWriteElements :: Spec -> Validation [SpecError] [WriteElement]
specWrapperWriteElements spec@Spec {..} = do
  let
    getHandle = (`Map.lookup` Map.fromList ((hName &&& id) <$> sHandles))
    isBitmask =
      (`Set.member` Set.fromList
        [ n
        | Enum {..} <- sEnums
        , n         <- eName : eAliases
        , eType == EnumTypeBitmask
        ]
      )
    isStruct s = any (`Set.member` Set.fromList (sName <$> sStructs))
                     (s : [ a | Just a <- [getAlias1 sTypeAliases s] ])

    -- TODO: Filter in a better way
    enabledCommands =
      filter ((`notElem` ignoredUnexportedNames) . TermName . cName) sCommands

    enabledStructs = filter
      ( (`notElem` (ignoredUnexportedNames ++ unmarshalledTypes))
      . TypeName
      . sName
      )
      sStructs

    dispatchableHandles =
      [ h | h@Handle { hHandleType = Dispatchable } <- sHandles ]

    getStructDispatchableHandle = doesStructContainDispatchableHandle
      (getHandle <=< simpleTypeName)
      sStructs

    resolveAlias :: Text -> Text
    resolveAlias t = fromMaybe t (getAlias1 sTypeAliases t)


  bracketAndCommandWrappers <- eitherToValidation $ do
    (bracketConstructors, bs) <- unzip <$> brackets sHandles
    let getBrackets :: Text -> Maybe HaskellName
        getBrackets commandName = listToMaybe
          [ bName
          | (createName, bName) <- bracketConstructors
          , TermName commandName == createName
          ]
    cs <- traverse
      (commandWrapper getHandle
                      isBitmask
                      isStruct
                      getStructDispatchableHandle
                      resolveAlias
                      getBrackets
      )
      enabledCommands
    pure $ bs : cs
  structWrappers <- eitherToValidation $ traverse
    (structWrapper getHandle isBitmask isStruct enabledStructs)
    enabledStructs
  handleWrappers <- eitherToValidation
    $ traverse handleWrapper dispatchableHandles
  aliases        <- writeAliases (makeMarshalledAliases spec)
  someVkStructWE <- eitherToValidation
    $ someVkStructWriteElement getHandle sPlatforms enabledStructs
  peekStructWE <- eitherToValidation
    $ vkPeekStructWriteElement getHandle sPlatforms enabledStructs
  pure
    (  concat
        [ concat structWrappers
        , concat bracketAndCommandWrappers
        , handleWrappers
        , aliases
        ]
    ++ [vkStructWriteElement, peekStructWE, someVkStructWE]
    )

specCWriteElements :: Spec -> Validation [SpecError] [WriteElement]
specCWriteElements s@Spec {..} = do
  let
    -- All requirement in features and specs
    reqs =
      (extRequirements =<< sExtensions)
        ++ (   fRequirements
           =<< [vulkan10Feature sFeatures, vulkan11Feature sFeatures]
           )

    getEnumAliasTarget :: Text -> Maybe Text
    getEnumAliasTarget n = do
      a <- find ((== n) . aName ) (fst <$> enumAliases sAliases)
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
    wConstants =
      let isAllowedConstant c = acName c `notElem` ["VK_TRUE", "VK_FALSE"]
      in  writeAPIConstant <$> filter isAllowedConstant sConstants
    wConstantExtensions =
      fmap (writeConstantExtension getEnumerantEnumName) . rConstants =<< reqs
  wFuncPointers <- eitherToValidation $ traverse writeFuncPointer sFuncPointers
  wHandles      <- eitherToValidation $ traverse writeHandle sHandles
  wCommands     <- eitherToValidation
    $ traverse (writeCommand getEnumAliasTarget) sCommands
  wStructs           <- eitherToValidation $ traverse writeStruct sStructs
  wAliases           <- writeAliases sAliases
  wBaseTypes         <-
    let isAllowedBaseType bt = btName bt /= "VkBool32"
    in  eitherToValidation
          $ traverse writeBaseType (filter isAllowedBaseType sBaseTypes)
  wLoader <- eitherToValidation $ writeLoader sPlatforms sCommands
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
    , wLoader
    ]
