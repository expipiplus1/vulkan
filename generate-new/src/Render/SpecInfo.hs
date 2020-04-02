module Render.SpecInfo
  where

import           Relude                  hiding ( Handle )
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Polysemy.Input
import           Polysemy
import           Algebra.Graph.Relation
import           Algebra.Graph.ToGraph
import           CType
import           Error

import           Spec.Types
import           Render.Element

type HasSpecInfo r = MemberWithError (Input SpecInfo) r

type SizeMap = CType -> Maybe (Int, Int)

data SpecInfo = SpecInfo
  { siIsUnion                   :: CName -> Maybe Union
  , siIsStruct                  :: CName -> Maybe Struct
  , siIsHandle                  :: CName -> Maybe Handle
  , siIsCommand                 :: CName -> Maybe Command
  , siIsDisabledCommand         :: CName -> Maybe Command
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
  -> Sem (Input SpecInfo ': r) a
  -> Sem r a
withSpecInfo Spec {..} siTypeSize r = do
  RenderParams {..} <- input
  let
    mkLookup n f =
      let m = Map.fromList [ (n s, s) | s <- toList f ]
      in  (`Map.lookup` m) . resolveAlias
    siIsUnion   = mkLookup sName specUnions
    siIsStruct  = mkLookup sName specStructs
    siIsHandle  = mkLookup hName specHandles
    siIsCommand = mkLookup cName specCommands
    siIsDisabledCommand =
      let disabledCommandNames = Set.fromList
            [ c
            | Extension {..} <- toList specDisabledExtensions
            , Require {..}   <- toList exRequires
            , c              <- toList rCommandNames
            ]
      in  \n ->
            if Set.member n disabledCommandNames then siIsCommand n else Nothing
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
    aliasMap        = Map.fromList
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
  runInputConst SpecInfo { .. } r

getStruct :: HasSpecInfo r => CName -> Sem r (Maybe Struct)
getStruct t = ($ t) <$> inputs siIsStruct

getUnion :: HasSpecInfo r => CName -> Sem r (Maybe Union)
getUnion t = ($ t) <$> inputs siIsUnion

containsUnion :: HasSpecInfo r => CName -> Sem r [Union]
containsUnion t = ($ t) <$> inputs siContainsUnion

getHandle :: HasSpecInfo r => CName -> Sem r (Maybe Handle)
getHandle t = ($ t) <$> inputs siIsHandle

getCommand :: HasSpecInfo r => CName -> Sem r (Maybe Command)
getCommand t = ($ t) <$> inputs siIsCommand

getDisabledCommand :: HasSpecInfo r => CName -> Sem r (Maybe Command)
getDisabledCommand t = ($ t) <$> inputs siIsDisabledCommand

getEnum :: HasSpecInfo r => CName -> Sem r (Maybe Enum')
getEnum t = ($ t) <$> inputs siIsEnum

getTypeSize :: (HasErr r, HasSpecInfo r) => CType -> Sem r (Int, Int)
getTypeSize t =
  note ("Unable to get size for " <> show t) =<< ($ t) <$> inputs siTypeSize

appearsInPositivePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInPositivePosition s = ($ s) <$> inputs siAppearsInPositivePosition

appearsInNegativePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInNegativePosition s = ($ s) <$> inputs siAppearsInNegativePosition

containsDispatchableHandle :: HasSpecInfo r => Struct -> Sem r Bool
containsDispatchableHandle = fmap (not . null) . dispatchableHandles

dispatchableHandles :: HasSpecInfo r => Struct -> Sem r [Handle]
dispatchableHandles Struct {..} =
  fmap (filter ((== Dispatchable) . hDispatchable) . catMaybes)
    . traverse getHandle
    $ [ t | StructMember {..} <- toList sMembers, t <- getAllTypeNames smType ]

inputs :: Member (Input a) r => (a -> b) -> Sem r b
inputs f = f <$> input
