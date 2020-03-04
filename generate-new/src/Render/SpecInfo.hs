module Render.SpecInfo
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , asks
                                                , runReader
                                                , Handle
                                                )
import qualified Data.Map                      as Map
import           Polysemy.Reader
import           Polysemy
import           Algebra.Graph.Relation
import           CType
import           Error

import           Spec.Types
import           Render.Element

type HasSpecInfo r = MemberWithError (Reader SpecInfo) r

type SizeMap = CType -> Maybe (Int, Int)

data SpecInfo = SpecInfo
  { siIsUnion       :: Text -> Maybe Union
  , siIsStruct      :: Text -> Maybe Struct
  , siIsHandle      :: Text -> Maybe Handle
  , siContainsUnion :: Text -> [Union]
  , siTypeSize      :: SizeMap
  , siGetConstructorParent :: Text -> Maybe Text
    -- ^ If we want the constructors for @s@ in scope then we should import
    -- @(siGetConstructorParent s)(..)@, Due to type synonyms this is not
    -- always the same name. As this deals with names as they appear in the
    -- generated source it takes and returns names after going through the
    -- mapping in RenderParams.
  }

withSpecInfo
  :: HasRenderParams r
  => Spec
  -> (CType -> Maybe (Int, Int))
  -> Sem (Reader SpecInfo ': r) a
  -> Sem r a
withSpecInfo Spec {..} siTypeSize r = do
  RenderParams {..} <- ask
  let
    mkLookup n f =
      let m = Map.fromList [ (n s, s) | s <- toList f ] in (`Map.lookup` m)
    siIsUnion          = mkLookup sName specUnions
    siIsStruct         = mkLookup sName specStructs
    siIsHandle         = mkLookup hName specHandles
    typeParentRelation = edges
      [ (t, sName)
      | Struct {..} <- toList specStructs
      , m           <- toList (smType <$> sMembers)
      , t           <- getAllTypeNames m
      ]
    ancestorRelation = transitiveClosure typeParentRelation
    siContainsUnion t =
      [ u | u <- toList specUnions, hasEdge (sName u) t ancestorRelation ]
    constructorMap =
      Map.fromList
        $  [ (n, n)
           | Struct {..} <- toList specStructs
           , let n = mkTyName sName
           ]
        <> [ (n, n)
           | Struct {..} <- toList specUnions
           , let n = mkTyName sName
           ]
        <> [ (n, n) | Enum {..} <- toList specEnums, let n = mkTyName eName ]
        <> [ (n, n)
           | Handle {..} <- toList specHandles
           , NonDispatchable == hDispatchable
           , let n = mkTyName hName
           ]
    -- TODO: Handle alias cycles!
    aliasMap = Map.fromList
      [ (aName, aTarget)
      | Alias {..} <- toList specAliases
      , TypeAlias == aType
      ]
    siGetConstructorParent n = case Map.lookup n aliasMap of
      Nothing -> Map.lookup n constructorMap
      Just n' -> siGetConstructorParent n'
  runReader SpecInfo { .. } r

getStruct :: HasSpecInfo r => Text -> Sem r (Maybe Struct)
getStruct t = ($ t) <$> asks siIsStruct

getUnion :: HasSpecInfo r => Text -> Sem r (Maybe Union)
getUnion t = ($ t) <$> asks siIsUnion

containsUnion :: HasSpecInfo r => Text -> Sem r [Union]
containsUnion t = ($ t) <$> asks siContainsUnion

getHandle :: HasSpecInfo r => Text -> Sem r (Maybe Handle)
getHandle t = ($ t) <$> asks siIsHandle

getTypeSize :: (HasErr r, HasSpecInfo r) => CType -> Sem r (Int, Int)
getTypeSize t =
  note ("Unable to get size for " <> show t) =<< ($ t) <$> asks siTypeSize

getConstructorParent :: HasSpecInfo r => Text -> Sem r (Maybe Text)
getConstructorParent s = ($ s) <$> asks siGetConstructorParent

