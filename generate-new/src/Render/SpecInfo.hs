module Render.SpecInfo
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , asks
                                                , runReader
                                                , Handle
                                                )
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Polysemy.Reader
import           Polysemy
import           Algebra.Graph.Relation
import           Algebra.Graph.ToGraph
import           CType
import           Error

import           Spec.Types
import           Render.Element

type HasSpecInfo r = MemberWithError (Reader SpecInfo) r

type SizeMap = CType -> Maybe (Int, Int)

data SpecInfo = SpecInfo
  { siIsUnion                   :: CName -> Maybe Union
  , siIsStruct                  :: CName -> Maybe Struct
  , siIsHandle                  :: CName -> Maybe Handle
  , siIsCommand                 :: CName -> Maybe Command
  , siIsEnum                    :: CName -> Maybe Enum'
  , siContainsUnion             :: CName -> [Union]
  , siTypeSize                  :: SizeMap
  , siAppearsInPositivePosition :: CName -> Bool
  , siAppearsInNegativePosition :: CName -> Bool
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
      let m = Map.fromList [ (n s, s) | s <- toList f ]
      in  (`Map.lookup` m) . resolveAlias
    siIsUnion          = mkLookup sName specUnions
    siIsStruct         = mkLookup sName specStructs
    siIsHandle         = mkLookup hName specHandles
    siIsCommand        = mkLookup cName specCommands
    siIsEnum           = mkLookup eName specEnums
    typeParentRelation = edges
      [ (t, sName)
      | Struct {..} <- toList specStructs
      , m           <- toList (smType <$> sMembers)
      , t           <- getAllTypeNames m
      ]
    containsUnionMap = Map.fromListWith
      (<>)
      [ (t, [u])
      | u <- toList specUnions
      , t <- reachable (sName u) typeParentRelation
      ]
    siContainsUnion = fromMaybe mempty . (`Map.lookup` containsUnionMap)
    aliasMap = Map.fromList
      [ (aName, aTarget)
      | Alias {..} <- toList specAliases
      , TypeAlias == aType
      ]
    -- TODO: Handle alias cycles!
    resolveAlias :: CName -> CName
    resolveAlias n = maybe n resolveAlias (Map.lookup n aliasMap)
    negativeTypes = Set.fromList
      [ t
      | Command {..}   <- toList specCommands
      , Parameter {..} <- toList cParameters
      , t              <- getAllTypeNames pType
      ]
    positiveTypes = Set.fromList
      [ t
      | Command {..} <- toList specCommands
      , t            <- getAllTypeNames cReturnType
      ]
    siAppearsInNegativePosition = (`Set.member` negativeTypes)
    siAppearsInPositivePosition = (`Set.member` positiveTypes)
  runReader SpecInfo { .. } r

getStruct :: HasSpecInfo r => CName -> Sem r (Maybe Struct)
getStruct t = ($ t) <$> asks siIsStruct

getUnion :: HasSpecInfo r => CName -> Sem r (Maybe Union)
getUnion t = ($ t) <$> asks siIsUnion

containsUnion :: HasSpecInfo r => CName -> Sem r [Union]
containsUnion t = ($ t) <$> asks siContainsUnion

getHandle :: HasSpecInfo r => CName -> Sem r (Maybe Handle)
getHandle t = ($ t) <$> asks siIsHandle

getCommand :: HasSpecInfo r => CName -> Sem r (Maybe Command)
getCommand t = ($ t) <$> asks siIsCommand

getEnum :: HasSpecInfo r => CName -> Sem r (Maybe Enum')
getEnum t = ($ t) <$> asks siIsEnum

getTypeSize :: (HasErr r, HasSpecInfo r) => CType -> Sem r (Int, Int)
getTypeSize t =
  note ("Unable to get size for " <> show t) =<< ($ t) <$> asks siTypeSize

appearsInPositivePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInPositivePosition s = ($ s) <$> asks siAppearsInPositivePosition

appearsInNegativePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInNegativePosition s = ($ s) <$> asks siAppearsInNegativePosition

containsDispatchableHandle :: HasSpecInfo r => Struct -> Sem r Bool
containsDispatchableHandle = fmap (not . null) . dispatchableHandles

dispatchableHandles :: HasSpecInfo r => Struct -> Sem r [Handle]
dispatchableHandles Struct {..} =
  fmap (filter ((== Dispatchable) . hDispatchable) . catMaybes)
    . traverse getHandle
    $ [ t | StructMember {..} <- toList sMembers, t <- getAllTypeNames smType ]


