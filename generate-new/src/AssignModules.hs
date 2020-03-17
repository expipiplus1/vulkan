{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module AssignModules
  ( assignModules
  ) where

import           Relude                  hiding ( runState
                                                , execState
                                                , evalState
                                                , State
                                                , modify'
                                                , ask
                                                , get
                                                , put
                                                , gets
                                                )
import           Data.Vector                    ( Vector )
import qualified Data.Vector.Extra             as V
import           Data.Version
import qualified Data.List.Extra               as List
import qualified Data.Map                      as Map
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as Set
import qualified Data.Text.Extra               as T
import           Polysemy
import           Polysemy.NonDet
import           Polysemy.Reader
import           Polysemy.State
import           Algebra.Graph.AdjacencyIntMap
                                         hiding ( empty )

import           Write.Segment                  ( Segment(..) )
import           Error
import           Spec.Types
import           Render.Element
import           Render.Spec
import           Haskell

-- | The following rules are applied in order:
--
-- - If a render element specifies where it should go, put it there
-- - Handles are put in a "Handles" module
-- - Enums are put in an "Enums" module
-- - If the features or extensions of a spec mention a name directly then it is
--   put in that module
-- - Constants are put in a "Constants" module
-- - If a type is mentioned directly by a command it is put in the same module
--   as the command
-- - An error is thrown if we matched none of the above

assignModules
  :: forall r
   . (HasErr r, HasRenderParams r)
  => Spec
  -> RenderedSpec RenderElement
  -> Sem r [(ModName, Vector RenderElement)]
assignModules spec@Spec {..} rs@RenderedSpec {..} = do
  let indexed = run . evalState (0 :: Int) $ traverse
        (\r -> do
          i <- get @Int
          modify' @Int succ
          pure (i, r)
        )
        rs
      flat = fromList . toList $ indexed
      lookupRe i = case flat V.!? i of
        Nothing     -> throw "Unable to find element at index"
        Just (_, e) -> pure e
  (exporterMap, rel) <- buildRelation (fromList . toList $ indexed)
  let getExporter n =
        note ("Unable to find " <> show n <> " in any render element")
          $ Map.lookup n exporterMap
      allNames     = Map.keys exporterMap
      initialState = mempty :: S

  exports <- execState initialState
    $ assign (raise . getExporter) rel (transitiveClosure rel) spec indexed

  --
  -- Check that everything is exported
  --
  forV indexed $ \(i, re) -> case IntMap.lookup i exports of
    Nothing -> do
      let exportedNames = exportName <$> toList (reExports re)
      forV_ (List.nubOrd exportedNames List.\\ unexportedNames)
        $ \n -> throw $ show n <> " is not exported from any module"
    Just _ -> pure ()

  let declaredNames =
        (Map.fromListWith (<>))
          $ (   (IntMap.toList exports)
            <&> \(n, ExportLocation d _) -> (d, Set.singleton n)
            )

  Map.toList
    <$> traverseV (traverseV lookupRe . fromList . Set.toList) declaredNames

data ExportLocation = ExportLocation
  { elDeclaringModule    :: ModName
  , elReExportingModules :: [ModName]
  }

type S = IntMap ExportLocation

-- The type is put into the first rule that applies, and reexported from any
-- subsequent matching rules in that feature.
--
-- - For each feature
--   - All reachable enums are put under the Feature.Enums module
--   - Commands are put into their respective components
--   - All types directly used by a command are put into the same module
--
-- - For each extension
--   -
assign
  :: forall r
   . (HasErr r, Member (State S) r, HasRenderParams r)
  => (HName -> Sem r Int)
  -> AdjacencyIntMap
  -> AdjacencyIntMap
  -> Spec
  -> RenderedSpec (Int, RenderElement)
  -> Sem r ()
assign getExporter rel closedRel Spec {..} rs@RenderedSpec {..} = do
  RenderParams {..} <- ask

  let allEnums        = IntMap.fromList (toList rsEnums)
      allHandles      = IntMap.fromList (toList rsHandles)
      allFuncPointers = IntMap.fromList (toList rsFuncPointers)

  ----------------------------------------------------------------
  -- Explicit elements
  ----------------------------------------------------------------
  forV_ rs $ \(i, re) -> forV_ (reExplicitModule re) $ \m -> export m i

  ----------------------------------------------------------------
  -- API Constants
  ----------------------------------------------------------------
  let constantModule = ModName $ modPrefix <> ".Core10.APIConstants"
  forV_ rsAPIConstants $ \(i, _) -> export constantModule i

  let
    exportReachable namedSubmodule prefix world reachable = do
      let reachableWorld = world `IntMap.restrictKeys` reachable
      forV_ (IntMap.toList reachableWorld) $ \(i, re) -> do
        modName <- if namedSubmodule
          then do
            name <- firstTypeName re
            pure (ModName (prefix <> "." <> name))
          else pure (ModName prefix)
        export @r modName i

    exportRequire separateTypeSubmodule prefix commentToName Require {..} = do
      let componentModule = commentToName rComment

      commands   <- traverseV getExporter rCommandNames
      types      <- traverseV getExporter rTypeNames
      enumValues <- traverseV getExporter rEnumValueNames

      when separateTypeSubmodule $ do
        let directExporters = commands <> types <> enumValues

        let reachable =
              Set.unions . toList $ (`postIntSet` closedRel) <$> directExporters

        ----------------------------------------------------------------
        -- Reachable Enums
        ----------------------------------------------------------------
        exportReachable True  (prefix <> ".Enums")   allEnums   reachable

        ----------------------------------------------------------------
        -- Reachable Handles
        ----------------------------------------------------------------
        exportReachable False (prefix <> ".Handles") allHandles reachable

        ----------------------------------------------------------------
        -- Reachable FuncPointers
        ----------------------------------------------------------------
        exportReachable False
                        (prefix <> ".FuncPointers")
                        allFuncPointers
                        reachable

      ----------------------------------------------------------------
      -- Commands
      ----------------------------------------------------------------
      forV_ commands $ export componentModule

      ----------------------------------------------------------------
      -- Explicit Types
      ----------------------------------------------------------------
      forV_ types $ export componentModule

      ----------------------------------------------------------------
      -- Enum Values
      ----------------------------------------------------------------
      forV_ enumValues $ export componentModule

      ----------------------------------------------------------------
      -- Types directly referred to by the commands and types
      ----------------------------------------------------------------
      forV_ (commands <> types <> enumValues)
        $ \i -> exportMany componentModule (postIntSet i rel)


  forV_ specFeatures $ \Feature {..} -> do
    let prefix =
          modPrefix <> ".Core" <> (foldMap show (versionBranch fVersion))

    forV_ (sortOn (isNothing . rComment) . toList $ fRequires)
      $ exportRequire True prefix (featureCommentToModuleName prefix)

  forV_ specExtensions $ \Extension {..} -> forV_ exRequires $ exportRequire
    False
    extensionModulePrefix
    (const (extensionNameToModuleName exName))

  ----------------------------------------------------------------
  -- Assign aliases to be with their targets if possible
  ----------------------------------------------------------------
  forV_ specAliases $ \Alias {..} -> do
    let mkName = case aType of
          TypeAlias    -> TyConName
          TermAlias    -> TermName
          PatternAlias -> ConName
    i <- getExporter (mkName aName)
    j <- getExporter (mkName aTarget)
    gets @S (IntMap.lookup j) >>= \case
      Just jMod -> modify' (IntMap.insert i jMod)
      Nothing   -> pure ()

  ----------------------------------------------------------------
  -- Not everything will have been exported by now as it isn't immediately
  -- reachable, assign everything reachable within several steps
  ----------------------------------------------------------------
  let
    exportRequire2 prefix commentToName Require {..} = do
      let componentModule = commentToName rComment

      commands <- traverseV getExporter rCommandNames
      types    <- traverseV getExporter rTypeNames

      ----------------------------------------------------------------
      -- Types indirectly referred to by the commands or types
      ----------------------------------------------------------------
      forV_ (commands <> types)
        $ \i -> exportManyNoReexport componentModule (postIntSet i closedRel)

  forV_ specFeatures $ \Feature {..} -> do
    let prefix =
          modPrefix <> ".Core" <> (foldMap show (versionBranch fVersion))

    forV_ fRequires $ exportRequire2 prefix (featureCommentToModuleName prefix)

  forV_ specExtensions $ \Extension {..} -> do
    forV_ exRequires $ exportRequire2
      extensionModulePrefix
      (const (extensionNameToModuleName exName))

  pure ()

firstTypeName :: HasErr r => RenderElement -> Sem r Text
firstTypeName re =
  case V.mapMaybe getTyConName (exportName <$> (reExports re)) of
    V.Empty   -> throw "Unable to get type name from RenderElement"
    x V.:<| _ -> pure x

export :: Member (State S) r => ModName -> Int -> Sem r ()
export m i = modify' (IntMap.alter ins i)
 where
  ins = \case
    Nothing                    -> Just (ExportLocation m [])
    Just (ExportLocation t rs) -> Just (ExportLocation t (m : rs))

exportMany :: Member (State S) r => ModName -> IntSet -> Sem r ()
exportMany m is =
  let newMap = IntMap.fromSet (const (ExportLocation m [])) is
  in  modify' (\s -> IntMap.unionWith ins s newMap)
 where
  ins (ExportLocation r rs) (ExportLocation n _) = ExportLocation r (n : rs)

exportManyNoReexport :: Member (State S) r => ModName -> IntSet -> Sem r ()
exportManyNoReexport m is =
  let newMap = IntMap.fromSet (const (ExportLocation m [])) is
  in  modify' (\s -> IntMap.unionWith ins s newMap)
  where ins (ExportLocation r rs) (ExportLocation n _) = ExportLocation r rs

----------------------------------------------------------------
-- Making module names
----------------------------------------------------------------

modPrefix :: Text
modPrefix = "Graphics.Vulkan"

extensionModulePrefix :: Text
extensionModulePrefix = modPrefix <> ".Extensions"

extensionNameToModuleName :: Text -> ModName
extensionNameToModuleName = ModName . ((extensionModulePrefix <> ".") <>)

featureCommentToModuleName :: Text -> Maybe Text -> ModName
featureCommentToModuleName prefix = \case
  Nothing -> ModName prefix
  Just t ->
    ModName
      $ ((prefix <> ".") <>)
      . mconcat
      . fmap (T.upperCaseFirst . replaceSymbols)
      . dropLast "commands"
      . T.words
      . T.replace "Promoted from" "Promoted_From_"
      . T.replace "Originally based on" "Originally_Based_On_"
      . T.takeWhile (/= ',')
      . removeParens
      . featureCommentMap
      $ t

featureCommentMap :: Text -> Text
featureCommentMap = \case
  "These types are part of the API and should always be defined, even when no enabled features require them."
    -> "OtherTypes"
  t -> t

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

buildRelation
  :: HasErr r
  => Vector (Int, RenderElement)
  -> Sem r (Map HName Int, AdjacencyIntMap)
buildRelation elements = do
  let elementExports RenderElement {..} =
        reInternal <> reExports <> V.concatMap exportWith reExports
      elementImports = fmap importName . toList . reLocalImports
      numbered       = elements
      allNames       = sortOn
        fst
        [ (n, i)
        | (i, x) <- toList numbered
        , n <- fmap exportName . V.toList . elementExports $ x
        ]
      nameMap = Map.fromAscList allNames
      lookup n = case Map.lookup n nameMap of
        Nothing -> throw $ "Unable to find " <> show n <> " in any vertex"
        Just i  -> pure i

  es <- concat
    <$> forV numbered (\(n, x) -> forV (elementImports x) (fmap (n, ) . lookup))

  let relation = vertices (fst <$> toList numbered) `overlay` edges es

  pure $ (nameMap, relation)

getTyConName :: HName -> Maybe Text
getTyConName = \case
  TyConName n -> Just n
  _ -> Nothing

removeParens :: Text -> Text
removeParens t =
  let (x, y) = T.breakOn "(" t in x <> T.takeWhileEnd (/= ')') y

replaceSymbols :: Text -> Text
replaceSymbols = \case
  "+" -> "And"
  t   -> t

dropLast :: Eq a => a -> [a] -> [a]
dropLast x l = case nonEmpty l of
  Nothing -> []
  Just xs -> if last xs == x then init xs else toList xs

----------------------------------------------------------------
-- Ignored unexported names
----------------------------------------------------------------

unexportedNames :: [HName]
unexportedNames =
  [ TermName "vkGetSwapchainGrallocUsageANDROID"
  , TermName "vkGetSwapchainGrallocUsage2ANDROID"
  , TermName "vkAcquireImageANDROID"
  , TermName "vkQueueSignalReleaseImageANDROID"
  , TyConName "VkNativeBufferUsage2ANDROID"
  , TyConName "VkNativeBufferANDROID"
  , TyConName "VkSwapchainImageCreateInfoANDROID"
  , TyConName "VkPhysicalDevicePresentationPropertiesANDROID"
    -- TODO: Export these
  , TyConName "VkSemaphoreCreateFlagBits"
  ]



