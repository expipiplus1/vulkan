{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Partition
  ( ModuleSeed(..)
  , Module(..)
  , partitionElements
  , ignoredUnexportedNames
  ) where

import           Control.Monad
import           Data.Closure
import           Data.Either
import           Data.Either.Validation
import           Data.Foldable
import           Data.Function
import           Data.Functor.Extra
import           Data.List.Extra
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.Semigroup
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text.Extra        as T
import           Prelude                hiding (mod)

import           Spec.Savvy.Error
import           Write.Element
import           Write.Module

data ModuleSeed = ModuleSeed
  { msName     :: Text
  , msSeeds    :: [HaskellName]
  , msPlatform :: Maybe Text
    -- ^ Is this module only available on a particular platform
  }
  deriving (Show)

partitionElements
  :: [WriteElement] -> [ModuleSeed] -> Either [SpecError] [Module]
partitionElements wes ss = validationToEither $ do
  let -- All the names explicitly exported by seeds.
    allSeedNames :: Set.Set HaskellName
    allSeedNames =
      Set.fromList $ weProvidesHN =<< providingElements =<< msSeeds =<< ss

    closeSeed :: ModuleSeed -> Set.Set HaskellName
    closeSeed ModuleSeed {..}
      = let
          step name =
            let
              ds = Set.map unGuarded
                $ Set.fromList (weDepends =<< providingElements name)
            in      --  Don't allow other modules to grab exports from another seed
                ds `Set.difference` allSeedNames
        in  close
              step
              (Set.fromList
                (  msSeeds
                ++ [ export
                   | (moduleName, export) <- explicitlyPlacedNames
                   , moduleName == msName
                   ]
                )
              )

    providingElements :: HaskellName -> [WriteElement]
    providingElements = (`MultiMap.lookup` m)
      where
        m =
          MultiMap.fromList [ (name, we) | we <- wes, name <- weProvidesHN we ]

    nameExport :: HaskellName -> Maybe (Guarded Export)
    nameExport n = case providingElements n of
      []    -> Nothing
      x : _ -> find ((== n) . unExport . unGuarded) (weProvides x)

    modules =
      [ Module msName closure [] seedReexports []
      | m@ModuleSeed {..} <- ss
      , let
        names   = Set.toList (closeSeed m)
        closure = nubOrdOn weProvides $ providingElements =<< names
        seedReexports = nubOrd $ mapMaybe nameExport msSeeds
      ]
    prioritizedModules = prioritizeModules modules
  _ <- assertUniqueSeedNames ss
  _ <- assertExactlyOneSeeds wes ss
  _ <- assertExactlyOneExport wes prioritizedModules
  _ <- assertNoEmptyModules prioritizedModules
  _ <- assertAllDependenciesSatisfied prioritizedModules
  pure prioritizedModules

-- | Make sure there are no duplicate exports, if something is exported in an
-- earlier module make it a reexport in a later module
prioritizeModules :: [Module] -> [Module]
prioritizeModules = catMaybes . fmap fst . scanl go (Nothing, mempty)
  where
    go
      :: (Maybe Module, Set.Set HaskellName)
      -- ^ The previous module, the set of already exported names
      -> Module
      -- ^ The module to remove elements from
      -> (Maybe Module, Set.Set HaskellName)
      -- ^ The module with names removed, the enlarged set of haskell names
    go (_, disallowedNames) mod =
      let mod' = makeReexports disallowedNames mod
          disallowedNames' =
            disallowedNames `Set.union` moduleExportedNames mod'
      in  (Just mod', disallowedNames')

makeReexports :: Set.Set HaskellName -> Module -> Module
makeReexports dis Module {..} =
  let isAllowed we@WriteElement {..} = all (`Set.notMember` dis) (weProvidesHN we)
      (wes, res) = partitionEithers
        -- TODO: This should preserve guards
        [ if isAllowed we then Left we else Right (unGuarded <$> weProvides we)
        | we <- mWriteElements
        ]
      seedReexports = mSeedReexports \\ (weProvides =<< wes)
  in  Module {mWriteElements = wes, mReexports = concat res, mSeedReexports = seedReexports, ..}

moduleExportedNames :: Module -> Set.Set HaskellName
moduleExportedNames Module {..} =
  Set.fromList [ n | we@WriteElement {..} <- mWriteElements, n <- weProvidesHN we ]

----------------------------------------------------------------
-- Quirks
----------------------------------------------------------------

ignoredUnexportedNames :: [HaskellName]
ignoredUnexportedNames =
  -- These are in a "disabled" extension:
  [ TermName "vkGetSwapchainGrallocUsageANDROID"
  , TermName "vkAcquireImageANDROID"
  , TermName "vkQueueSignalReleaseImageANDROID"
  , TypeName "VkNativeBufferANDROID"
  , TermName "VkNativeBufferANDROID"
  , PatternName "VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID"
  ]

explicitlyPlacedNames :: [(Text, HaskellName)]
explicitlyPlacedNames =
  [ ( "Graphics.Vulkan.C.Core10.CommandBufferBuilding"
    , TypeName "VkDrawIndirectCommand"
    )
  , ( "Graphics.Vulkan.C.Core10.CommandBufferBuilding"
    , TypeName "VkDrawIndexedIndirectCommand"
    )
  , ( "Graphics.Vulkan.C.Core10.CommandBufferBuilding"
    , TypeName "VkDispatchIndirectCommand"
    )
  , ( "Graphics.Vulkan.Core10.CommandBufferBuilding"
    , TypeName "DrawIndirectCommand"
    )
  , ( "Graphics.Vulkan.Core10.CommandBufferBuilding"
    , TypeName "DrawIndexedIndirectCommand"
    )
  , ( "Graphics.Vulkan.Core10.CommandBufferBuilding"
    , TypeName "DispatchIndirectCommand"
    )
  ]

----------------------------------------------------------------
-- Checks
----------------------------------------------------------------

-- | Make sure the values required by seeds occur exacly once in the write elements
assertExactlyOneSeeds
  :: [WriteElement] -> [ModuleSeed] -> Validation [SpecError] ()
assertExactlyOneSeeds wes seeds = do
  let exportMap = genExportMapWriteElements wes

  -- Check that every reference has exactly one match
  _ <- for_ seeds $ \ModuleSeed {..} -> for_ msSeeds $ \seed ->
    case exportMap seed of
        []   -> Failure [UnexportedNameWriteElement (T.tShow seed) (T.tShow msName)]
        [_]  -> Success ()
        wes' -> Failure [MultipleExportError1 (T.tShow seed) wes']

  pure ()

-- | Make sure there are no exports left behind and that everything is exported
-- exactly once
assertExactlyOneExport
  :: [WriteElement] -> [Module] -> Validation [SpecError] ()
assertExactlyOneExport wes mods =
  let allExports = weProvidesHN =<< wes
      exportMap  = genExportMap mods
      checkName :: HaskellName -> Validation [SpecError] ()
      checkName n = case exportMap n of
        []    -> if n `elem` ignoredUnexportedNames
                   then Success ()
                   else Failure [UnexportedName (T.tShow n)]
        [_]   -> Success ()
        mods' -> Failure [MultipleExportError2 (T.tShow n) mods']
  in  for_ allExports checkName

-- | Make sure there are no modules exporting nothing
assertNoEmptyModules :: [Module] -> Validation [SpecError] ()
assertNoEmptyModules = traverse_ $ \Module {..} ->
  if null mWriteElements && null mSeedReexports
    then Failure [ModuleWithoutExports mName]
    else Success ()

assertAllDependenciesSatisfied :: [Module] -> Validation [SpecError] ()
assertAllDependenciesSatisfied ms
  = let
      allDepends :: [HaskellName]
      allDepends = nubOrd $ fmap unGuarded . weDepends =<< mWriteElements =<< ms
      allExports :: [HaskellName]
      allExports =
        nubOrd . fmap (unExport . unGuarded) $ weProvides =<< mWriteElements =<< ms
      dependsMap :: HaskellName -> [Text]
      dependsMap =
        let m = MultiMap.fromList
              [ (d, weName we)
              | we <- mWriteElements =<< ms
              , d  <- unGuarded <$> weDepends we
              ]
        in  (`MultiMap.lookup` m)
    in
      case allDepends \\ allExports of
        [] -> pure ()
        xs -> Failure
          (xs <&> (\x -> RequiredExportMissing (T.tShow x) (dependsMap x)))

genExportMap :: [Module] -> HaskellName -> [(Text, Text)]
genExportMap ms = (`MultiMap.lookup` m)
  where
    m = MultiMap.fromList
      [ (hsName, (mName, weName))
      | Module {..}       <- ms
      , we@WriteElement {..} <- mWriteElements
      , hsName            <- weProvidesHN we
      ]

genExportMapWriteElements :: [WriteElement] -> HaskellName -> [Text]
genExportMapWriteElements wes = (`MultiMap.lookup` m)
  where
    m = MultiMap.fromList
      [ (hsName, weName)
      | we@WriteElement {..} <- wes
      , hsName            <- weProvidesHN we
      ]

assertUniqueSeedNames :: [ModuleSeed] -> Validation [SpecError] ()
assertUniqueSeedNames ms =
  let names          = msName <$> ms
      duplicateNames = nubOrd names \\ names
  in  case duplicateNames of
        [] -> pure ()
        ns -> Failure [DuplicateSeedNames ns]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

weProvidesHN :: WriteElement -> [HaskellName]
weProvidesHN = fmap (unExport . unGuarded) . weProvides
