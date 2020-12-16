module Render.SpecInfo where

import           Algebra.Graph.Relation
import           Algebra.Graph.ToGraph
import           CType
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Error
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle )

import           Data.List.Extra                ( nubOrd )
import qualified Data.Vector                   as V
import           Render.Element
import           Spec.Types

type HasSpecInfo r = MemberWithError (Input SpecInfo) r

type SizeMap = CType -> Maybe (Int, Int)

-- TODO: Rename to HeaderInfo
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
  , siGetAliases                :: CName -> [CName]
  , siExtensionType             :: Text -> Maybe ExtensionType
  , siExtensionDeps             :: Text -> [Text]
  }

instance Semigroup SpecInfo where
  s1 <> s2 = SpecInfo
    { siIsUnion                   = first siIsUnion
    , siIsStruct                  = first siIsStruct
    , siIsHandle                  = first siIsHandle
    , siIsCommand                 = first siIsCommand
    , siIsDisabledCommand         = first siIsDisabledCommand
    , siIsEnum                    = first siIsEnum
    , siContainsUnion             = applyBoth siContainsUnion (liftA2 (<>))
    , siTypeSize                  = first siTypeSize
    , siAppearsInPositivePosition = applyBoth siAppearsInPositivePosition
                                              (liftA2 (||))
    , siAppearsInNegativePosition = applyBoth siAppearsInNegativePosition
                                              (liftA2 (||))
    , siGetAliases                = applyBoth siGetAliases (liftA2 (<>))
    , siExtensionType             = first siExtensionType
    , siExtensionDeps             = applyBoth siExtensionDeps (liftA2 (<>))
    }
   where
    first :: (SpecInfo -> (a -> Maybe b)) -> a -> Maybe b
    first f = applyBoth f (liftA2 (<|>))
    applyBoth :: (SpecInfo -> a) -> (a -> a -> b) -> b
    applyBoth f x = x (f s1) (f s2)

instance Monoid SpecInfo where
  mempty = SpecInfo (const Nothing)
                    (const Nothing)
                    (const Nothing)
                    (const Nothing)
                    (const Nothing)
                    (const Nothing)
                    (const [])
                    (const Nothing)
                    (const False)
                    (const False)
                    (const [])
                    (const Nothing)
                    (const [])

specSpecInfo :: Spec t -> (CType -> Maybe (Int, Int)) -> SpecInfo
specSpecInfo Spec {..} siTypeSize =
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
      in  \n -> if Set.member n disabledCommandNames
            then siIsCommand n
            else Nothing
    siIsEnum = mkLookup eName specEnums
    typeParentRelation =
      edges
        $  [ (t, sName)
           | Struct {..} <- toList specStructs
           , m           <- toList (smType <$> sMembers)
           , t           <- getAllTypeNames m
           ]
        <> [ (t, sName)
           | Struct {..} <- toList specUnions
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
    aliasMap =
      Map.fromList [ (aName, aTarget) | Alias {..} <- toList specAliases ]
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
    siGetAliases :: CName -> [CName]
    siGetAliases =
      let reverseAliasMap = Map.fromListWith
            (<>)
            [ (aTarget, [aName]) | Alias {..} <- toList specAliases ]
          go n = do
            a <- Map.lookupDefault [] n reverseAliasMap
            a : go a
      in  go
    siExtensionType =
      let exMap = Map.fromList
            [ (exName, exType) | Extension {..} <- toList specExtensions ]
      in  (`Map.lookup` exMap)
    siExtensionDeps =
      let
        depMap = Map.fromList
          [ (exName, V.toList exDependencies)
          | Extension {..} <- toList specExtensions
          ]
        get n = fromMaybe mempty (Map.lookup n depMap)
        close n =
          let immediateDeps = get n
          in  concat (immediateDeps : (close <$> immediateDeps))
      in
        nubOrd . close
  in
    SpecInfo { .. }

withSpecInfo
  :: HasRenderParams r
  => Spec t
  -> (CType -> Maybe (Int, Int))
  -> Sem (Input SpecInfo ': r) a
  -> Sem r a
withSpecInfo spec typeSize r =
  let si = specSpecInfo spec typeSize in runInputConst si r

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

getTypeSize :: (HasErr r, HasSpecInfo r) => CType -> Sem r (Int, Int)
getTypeSize t =
  note ("Unable to get size for " <> show t) . ($ t) =<< inputs siTypeSize

appearsInPositivePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInPositivePosition s = ($ s) <$> inputs siAppearsInPositivePosition

appearsInNegativePosition :: HasSpecInfo r => CName -> Sem r Bool
appearsInNegativePosition s = ($ s) <$> inputs siAppearsInNegativePosition

getAliases :: HasSpecInfo r => CName -> Sem r [CName]
getAliases s = ($ s) <$> inputs siGetAliases
