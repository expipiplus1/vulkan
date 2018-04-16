{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Partition
  ( ModuleSeed(..)
  , Module(..)
  , partitionElements
  , moduleSummary
  ) where

import           Control.Arrow          ((&&&))
import           Control.Monad
import           Data.Closure
import           Data.Either.Validation
import           Data.Foldable
import           Data.Function
import           Data.List.Extra
import           Data.Maybe
import qualified Data.MultiMap          as MultiMap
import           Data.Semigroup
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Prelude                hiding (mod)

import           Spec.Savvy.Error
import           Write.Element

data ModuleSeed = ModuleSeed
  { msName  :: Text
  , msSeeds :: [HaskellName]
  }
  deriving (Show)

data Module = Module
  { mName          :: Text
  , mWriteElements :: [WriteElement]
  }
  deriving (Show)

partitionElements
  :: [WriteElement] -> [ModuleSeed] -> Either [SpecError] [Module]
partitionElements wes ss = validationToEither $ do
  let allSeedNames :: [HaskellName]
      allSeedNames = [ n | ModuleSeed {..} <- ss, n <- msSeeds ]

      closeSeed :: ModuleSeed -> [HaskellName]
      closeSeed ModuleSeed {..} =
        let step name =
              [ d
              | we <- providingElements [name]
              , d  <- weDepends we
              , d `notElem` allSeedNames
              ]
        in  close step msSeeds

      providingElements :: [HaskellName] -> [WriteElement]
      providingElements names =
        [ we | we <- wes, not . null $ intersect names (weProvides we) ]
      modules =
        [ Module msName closure
        | m@ModuleSeed {..} <- ss
        , let names   = closeSeed m
              closure = providingElements names
        ]
      prioritizedModules = prioritizeModules modules
  _ <- assertExactlyOneSeeds wes ss
  _ <- assertExactlyOneExport wes prioritizedModules
  _ <- assertNoEmptyModules prioritizedModules
  pure prioritizedModules

-- | Make sure there are no duplicate exports, if something is exported in an
-- earlier module, remove it from later modules.
prioritizeModules :: [Module] -> [Module]
prioritizeModules = catMaybes . fmap fst . scanl go (Nothing, mempty)
  where
    go
      :: (Maybe Module, Set.Set HaskellName)
      -- ^ The previous module, the set of already exported names
      -> Module
      -- ^ The module to remove elements from
      -> (Maybe Module, Set.Set HaskellName)
      -- ^ The module with names removed, the bigger set of haskell names
    go (_, disallowedNames) mod =
      let mod' = removeNames disallowedNames mod
          disallowedNames' =
            disallowedNames `Set.union` moduleExportedNames mod'
      in  (Just mod', disallowedNames')

removeNames :: Set.Set HaskellName -> Module -> Module
removeNames dis Module {..} =
  let isAllowed WriteElement {..} = all (`Set.notMember` dis) weProvides
      wes = [ we | we <- mWriteElements, isAllowed we ]
  in  Module {mWriteElements = wes, ..}

moduleExportedNames :: Module -> Set.Set HaskellName
moduleExportedNames Module {..} =
  Set.fromList [ n | WriteElement {..} <- mWriteElements, n <- weProvides ]

----------------------------------------------------------------
-- Checks
----------------------------------------------------------------

ignoredUnexportedNames :: [HaskellName]
ignoredUnexportedNames =
  [ Term "vkGetSwapchainGrallocUsageANDROID"
  , Term "vkAcquireImageANDROID"
  , Term "vkQueueSignalReleaseImageANDROID"
  , Type "VkNativeBufferANDROID"
  , Term "VkNativeBufferANDROID"
  ]

-- | Make sure the values required by seeds occur exacly once in the write elements
assertExactlyOneSeeds
  :: [WriteElement] -> [ModuleSeed] -> Validation [SpecError] ()
assertExactlyOneSeeds wes seeds = do
  let exportMap = genExportMapWriteElements wes

  -- Check that every reference has exactly one match
  _ <- for_ seeds $ \ModuleSeed {..} -> for_ msSeeds $ \seed ->
    case exportMap seed of
        []   -> Failure [UnexportedNameWriteElement (tShow seed) (tShow msName)]
        [_]  -> Success ()
        wes' -> Failure [MultipleExportError1 (tShow seed) wes']

  pure ()

-- | Make sure there are no exports left behind and that everything is exported
-- exactly once
assertExactlyOneExport
  :: [WriteElement] -> [Module] -> Validation [SpecError] ()
assertExactlyOneExport wes mods =
  let allExports = [ e | WriteElement {..} <- wes, e <- weProvides ]
      exportMap  = genExportMap mods
      checkName :: HaskellName -> Validation [SpecError] ()
      checkName n = case exportMap n of
        []    -> if n `elem` ignoredUnexportedNames
                   then Success ()
                   else Failure [UnexportedName (tShow n)]
        [_]   -> Success ()
        mods' -> Failure [MultipleExportError2 (tShow n) mods']
  in  for_ allExports checkName

-- | Make sure there are no modules exporting nothing
assertNoEmptyModules :: [Module] -> Validation [SpecError] ()
assertNoEmptyModules = traverse_ $ \Module {..} -> case mWriteElements of
  [] -> Failure [ModuleWithoutExports mName]
  _  -> Success ()

-- assertNoDuplicates :: [Module] -> Validation [SpecError] ()
-- assertNoDuplicates modules =
--   let exportMap = genExportMap modules
--   in  traverse_ (uncurry checkExport) exportMap

-- checkExport :: HaskellName -> [Text] -> Validation [SpecError] ()
-- checkExport name modules = if length modules > 1
--   then Failure [MultipleExportError (tShow name) modules]
--   else pure ()

genExportMap :: [Module] -> HaskellName -> [(Text, Text)]
genExportMap ms = (`MultiMap.lookup` m)
  where
    m = MultiMap.fromList
      [ (hsName, (mName, weName))
      | Module {..}       <- ms
      , WriteElement {..} <- mWriteElements
      , hsName            <- weProvides
      ]

genExportMapWriteElements :: [WriteElement] -> HaskellName -> [Text]
genExportMapWriteElements wes = (`MultiMap.lookup` m)
  where
    m = MultiMap.fromList
      [ (hsName, weName)
      | WriteElement {..} <- wes
      , hsName            <- weProvides
      ]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

moduleSummary :: Module -> Text
moduleSummary Module {..} = tShow mName <> ": " <> tShow
  (filter (not . isPattern) . concatMap weProvides $ mWriteElements)

isPattern :: HaskellName -> Bool
isPattern = \case
  Pattern _ -> True
  _         -> False


tShow :: Show a => a -> Text
tShow = T.pack . show

