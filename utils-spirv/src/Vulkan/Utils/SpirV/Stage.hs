{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Per-shader /stage signatures/ and the type-level checks that compose them.

A 'ShaderSig' captures, at the type level, a shader stage's interface (its @in@
and @out@ variables, by location) and its resource layouts (descriptor blocks by
@(set, binding)@, and push constants) — each as a normalized 'SigOffsetMap'
("Vulkan.Utils.SpirV.Signature"). 'reflectStageSig' emits one from a compiled
@.spv@; the value-level 'StageInfo' it is promoted from is also returned by
'stageInfoOf' for use as the oracle and for building Vulkan create-infos.

The composition checks are thin type-level /matchers/ (the heavy work stayed
value-level): 'MatchInterface' requires every fragment input to have a
matching-typed vertex output at the same location.

'reflectPipelineLayoutSig' goes one step further: it promotes the /merged/
pipeline-layout signature (descriptor blocks by @(set, binding)@ + push) across a
family of shaders — the type-level counterpart of the runtime merge in
"Vulkan.Utils.SpirV.Descriptors".
-}
module Vulkan.Utils.SpirV.Stage
  ( -- * Stage signatures
    ShaderSig (..)
  , StageKind (..)

    -- * Value-level extraction (the oracle)
  , StageInfo (..)
  , stageInfoOf
  , matchInterface
  , Linked (..)
  , linkStages

    -- * Type-level checks
  , MatchInterface
  , CompatibleResources

    -- * Merged pipeline-layout signature
  , LayoutSig (..)
  , LayoutInfo (..)
  , mergeLayout
  , KnownLayoutSig (..)

    -- * Generation
  , reflectStageSig
  , reflectPipelineLayoutSig
  ) where

import Data.Kind (Constraint)
import Data.List (foldl', sortOn)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError)
import Language.Haskell.TH

import Data.SpirV.Enum (BuiltIn (..))
import Data.SpirV.Reflect.InterfaceVariable qualified as InterfaceVariable
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified
import Data.SpirV.Reflect.TypeDescription (TypeDescription)

import Vulkan.Utils.SpirV.Layout (OffsetMap, fromFields, leafFieldType, normalize)
import Vulkan.Utils.SpirV.Reflect (reflectFileQ)
import Vulkan.Utils.SpirV.Reflect.OffsetMaps (pushOffsetMap, resourceOffsetMaps)
import Vulkan.Utils.SpirV.Signature (KnownKeyedSigs (..), KnownMaybeSig (..), SigOffsetMap, natT, promoteList, promoteMaybe, promoteOffsetMap)
import Vulkan.Utils.SpirV.Types (LayoutMode (..), classifyType)

-- | Which pipeline stage a shader is.
data StageKind = VertexStage | FragmentStage | ComputeStage
  deriving (Eq, Show)

{- | A stage's type-level signature: stage, inputs and outputs (location ↦ layout),
resources (@(set, binding)@ ↦ block layout) and an optional push-constant layout.
-}
data ShaderSig
  = ShaderSig
      StageKind
      [(Nat, SigOffsetMap)]
      [(Nat, SigOffsetMap)]
      [((Nat, Nat), SigOffsetMap)]
      (Maybe SigOffsetMap)

-- Value-level extraction. -------------------------------------------------------

{- | The value-level form of a 'ShaderSig', reflected from a 'Module'. This is the
oracle the type level is promoted from, and the source for Vulkan create-infos.
-}
data StageInfo = StageInfo
  { stage :: StageKind
  , inputs :: [(Word32, OffsetMap)]
  , outputs :: [(Word32, OffsetMap)]
  , resources :: [((Word32, Word32), OffsetMap)]
  , push :: Maybe OffsetMap
  }
  deriving (Eq, Show)

-- | Reflect a module's stage signature.
stageInfoOf :: Module -> StageInfo
stageInfoOf m =
  StageInfo
    { stage = stageKindOf m.shader_stage
    , inputs = interfaceOffsetMaps m.input_variables
    , outputs = interfaceOffsetMaps m.output_variables
    , resources = sortOn fst (resourceOffsetMaps m)
    , push =
        case V.toList m.push_constants of
          (pc : _) -> pushOffsetMap pc
          [] -> Nothing
    }

-- | Interface variables as @(location, offset map)@, built-ins dropped, ascending.
interfaceOffsetMaps :: V.Vector InterfaceVariable.InterfaceVariable -> [(Word32, OffsetMap)]
interfaceOffsetMaps vs =
  sortOn
    fst
    -- a non-built-in reads as Nothing (ffi) or the @BuiltIn (-1)@ sentinel (yaml)
    [ (v.location, g)
    | v <- V.toList vs
    , v.built_in `elem` [Nothing, Just (BuiltIn (-1))]
    , Just td <- [v.type_description]
    , Just g <- [interfaceOffsetMap td]
    ]

-- | The offset map of a single interface variable (a scalar/vector at offset 0).
interfaceOffsetMap :: TypeDescription -> Maybe OffsetMap
interfaceOffsetMap td = do
  numeric <- classifyType td
  pure (normalize (fromFields Std430Layout Nothing [("v", leafFieldType numeric)]))

stageKindOf :: Int -> StageKind
stageKindOf n
  | n == 0x01 = VertexStage
  | n == 0x10 = FragmentStage
  | otherwise = ComputeStage

{- | The interface oracle: every input of the second stage must have an
equal-typed output in the first. (Use as @matchInterface vert frag@.)
-}
matchInterface :: StageInfo -> StageInfo -> Either String ()
matchInterface producer consumer =
  mapM_ check consumer.inputs
  where
    outs = producer.outputs
    check (loc, want) =
      case lookup loc outs of
        Just have
          | have == want -> Right ()
          | otherwise ->
              Left ("interface mismatch at location " <> show loc)
        Nothing ->
          Left ("input at location " <> show loc <> " has no matching output")

{- | A linked vertex+fragment pipeline's combined signature (value-level oracle):
the merged resources (shared @(set, binding)@s deduped) and push constants, the
vertex stage's inputs (vertex attributes) and the fragment stage's outputs
(colour attachments).
-}
data Linked = Linked
  { resources :: [((Word32, Word32), OffsetMap)]
  , push :: Maybe OffsetMap
  , inputs :: [(Word32, OffsetMap)]
  , outputs :: [(Word32, OffsetMap)]
  }
  deriving (Eq, Show)

{- | Link a vertex and a fragment stage: the interface must match, shared bindings
and push constants must agree, and the resources union. Mirrors the type-level
'MatchInterface' + 'CompatibleResources'.
-}
linkStages :: StageInfo -> StageInfo -> Either String Linked
linkStages vert frag = do
  matchInterface vert frag
  resources <- mergeAssoc showKey vert.resources frag.resources
  push <- mergePush vert.push frag.push
  pure
    Linked
      { resources = sortOn fst resources
      , push
      , inputs = vert.inputs
      , outputs = frag.outputs
      }
  where
    showKey (s, b) = "(set " <> show s <> ", binding " <> show b <> ")"

-- | Union two keyed offset-map lists; a shared key whose maps differ is an error.
mergeAssoc :: (Eq k) => (k -> String) -> [(k, OffsetMap)] -> [(k, OffsetMap)] -> Either String [(k, OffsetMap)]
mergeAssoc shw xs ys = foldr step (Right xs) ys
  where
    step (k, g) acc = do
      m <- acc
      case lookup k m of
        Nothing -> Right ((k, g) : m)
        Just g'
          | g' == g -> Right m
          | otherwise -> Left ("incompatible layouts at " <> shw k)

mergePush :: Maybe OffsetMap -> Maybe OffsetMap -> Either String (Maybe OffsetMap)
mergePush Nothing b = Right b
mergePush a Nothing = Right a
mergePush (Just a) (Just b)
  | a == b = Right (Just a)
  | otherwise = Left "incompatible push-constant layouts between stages"

-- Merged pipeline-layout signature. ---------------------------------------------

{- | A merged pipeline-layout signature at the type level: the descriptor blocks by
@(set, binding)@ and the push-constant block, unified across a family of shaders.
The type-level mirror of 'LayoutInfo'.
-}
data LayoutSig = LayoutSig [((Nat, Nat), SigOffsetMap)] (Maybe SigOffsetMap)

{- | The value-level merged layout 'reflectPipelineLayoutSig' promotes (its oracle):
each descriptor block's 'OffsetMap' by @(set, binding)@ ascending, and the
push-constant block.
-}
data LayoutInfo = LayoutInfo
  { resources :: [((Word32, Word32), OffsetMap)]
  , push :: Maybe OffsetMap
  }
  deriving (Eq, Show)

{- | Merge several stages into one pipeline-layout signature: descriptor blocks are
unioned (a shared @(set, binding)@ whose layouts disagree is a 'Left') and the
push-constant blocks unified. Unlike 'linkStages' there is no interface check — one
layout is shared across pipelines whose vertex\/fragment interfaces differ.
-}
mergeLayout :: [Module] -> Either String LayoutInfo
mergeLayout modules = do
  resources <- foldl' addResources (Right []) infos
  push <- foldl' addPush (Right Nothing) infos
  pure LayoutInfo{resources = sortOn fst resources, push}
  where
    infos = map stageInfoOf modules
    addResources acc info = acc >>= mergeAssoc showKey info.resources
    addPush acc info = acc >>= \p -> mergePush p info.push
    showKey (s, b) = "(set " <> show s <> ", binding " <> show b <> ")"

{- | Reflect a type-level 'LayoutSig' back to its value 'LayoutInfo' — the oracle
view, for validating the promotion and (later) driving bind-site checks.
-}
class KnownLayoutSig (sig :: LayoutSig) where
  layoutSigVal :: Proxy sig -> LayoutInfo

instance (KnownKeyedSigs rs, KnownMaybeSig p) => KnownLayoutSig ('LayoutSig rs p) where
  layoutSigVal _ = LayoutInfo{resources = keyedSigsVal (Proxy @rs), push = maybeSigVal (Proxy @p)}

-- Type-level checks. ------------------------------------------------------------

{- | Holds when every input of @f@ has a matching-typed output of @v@ at the same
location (extra outputs of @v@ are allowed). A mismatch is a compile error.
-}
type family MatchInterface (v :: ShaderSig) (f :: ShaderSig) :: Constraint where
  MatchInterface ('ShaderSig _ _ vouts _ _) ('ShaderSig _ fins _ _ _) = MatchAll fins vouts

type family MatchAll (ins :: [(Nat, SigOffsetMap)]) (outs :: [(Nat, SigOffsetMap)]) :: Constraint where
  MatchAll '[] _ = ()
  MatchAll ('(loc, g) ': rest) outs = (MatchOne loc g (Lookup loc outs), MatchAll rest outs)

type family MatchOne (loc :: Nat) (want :: SigOffsetMap) (have :: Maybe SigOffsetMap) :: Constraint where
  MatchOne _ g ('Just g) = ()
  MatchOne loc want ('Just have) =
    TypeError
      ( 'Text "Interface mismatch at location "
          ':<>: 'ShowType loc
          ':<>: 'Text ":"
          ':$$: 'Text "  vertex out: "
          ':<>: 'ShowType have
          ':$$: 'Text "  fragment in: "
          ':<>: 'ShowType want
      )
  MatchOne loc _ 'Nothing =
    TypeError
      ('Text "Fragment input at location " ':<>: 'ShowType loc ':<>: 'Text " has no matching vertex output.")

type family Lookup (k :: Nat) (xs :: [(Nat, SigOffsetMap)]) :: Maybe SigOffsetMap where
  Lookup _ '[] = 'Nothing
  Lookup k ('(k, v) ': _) = 'Just v
  Lookup k ('(_, _) ': rest) = Lookup k rest

{- | Holds when every descriptor block and push constant the two stages /share/
(same @(set, binding)@) has the same layout. Disjoint resources are fine; a
shared binding with differing layouts is a compile error.
-}
type family CompatibleResources (v :: ShaderSig) (f :: ShaderSig) :: Constraint where
  CompatibleResources ('ShaderSig _ _ _ vres vpush) ('ShaderSig _ _ _ fres fpush) =
    (CompatAll vres fres, CompatPush vpush fpush)

type family CompatAll (xs :: [((Nat, Nat), SigOffsetMap)]) (ys :: [((Nat, Nat), SigOffsetMap)]) :: Constraint where
  CompatAll '[] _ = ()
  CompatAll ('(k, g) ': rest) ys = (CompatOne k g (LookupR k ys), CompatAll rest ys)

type family CompatOne (k :: (Nat, Nat)) (g :: SigOffsetMap) (m :: Maybe SigOffsetMap) :: Constraint where
  CompatOne _ _ 'Nothing = ()
  CompatOne _ g ('Just g) = ()
  CompatOne k _ ('Just _) =
    TypeError
      ( 'Text "Resource layout mismatch at set/binding "
          ':<>: 'ShowType k
          ':<>: 'Text " between stages."
      )

type family CompatPush (a :: Maybe SigOffsetMap) (b :: Maybe SigOffsetMap) :: Constraint where
  CompatPush 'Nothing _ = ()
  CompatPush _ 'Nothing = ()
  CompatPush ('Just g) ('Just g) = ()
  CompatPush ('Just _) ('Just _) =
    TypeError ('Text "Push-constant layouts differ between stages.")

type family LookupR (k :: (Nat, Nat)) (xs :: [((Nat, Nat), SigOffsetMap)]) :: Maybe SigOffsetMap where
  LookupR _ '[] = 'Nothing
  LookupR k ('(k, v) ': _) = 'Just v
  LookupR k ('(_, _) ': rest) = LookupR k rest

-- Generation. -------------------------------------------------------------------

-- | Reflect a compiled @.spv@ and emit @type \<name\> = \<promoted ShaderSig\>@.
reflectStageSig :: String -> FilePath -> Q [Dec]
reflectStageSig name path = do
  m <- reflectFileQ path
  pure [TySynD (mkName name) [] (promoteStageInfo (stageInfoOf m))]

promoteStageInfo :: StageInfo -> Type
promoteStageInfo si =
  PromotedT 'ShaderSig
    `AppT` promoteStage si.stage
    `AppT` promoteList (promoteLocated promoteOffsetMap) si.inputs
    `AppT` promoteList (promoteLocated promoteOffsetMap) si.outputs
    `AppT` promoteList (promoteKeyed promoteOffsetMap) si.resources
    `AppT` promoteMaybe promoteOffsetMap si.push

promoteStage :: StageKind -> Type
promoteStage = \case
  VertexStage -> PromotedT 'VertexStage
  FragmentStage -> PromotedT 'FragmentStage
  ComputeStage -> PromotedT 'ComputeStage

promoteLocated :: (a -> Type) -> (Word32, a) -> Type
promoteLocated f (loc, x) = pair (natT loc) (f x)

promoteKeyed :: (a -> Type) -> ((Word32, Word32), a) -> Type
promoteKeyed f ((s, b), x) = pair (pair (natT s) (natT b)) (f x)

pair :: Type -> Type -> Type
pair a b = PromotedTupleT 2 `AppT` a `AppT` b

{- | Reflect the @.spv@ of a pipeline's shaders and emit
@type \<name\> = \<promoted merged layout signature\>@ — the descriptor blocks by
@(set, binding)@ and push constants merged across the stages ('mergeLayout' is the
oracle). A layout disagreement between stages fails the splice. The type-level
counterpart of 'Vulkan.Utils.SpirV.Descriptors.mergedDescriptorSetLayoutInfos'.
-}
reflectPipelineLayoutSig :: String -> [FilePath] -> Q [Dec]
reflectPipelineLayoutSig name paths = do
  modules <- traverse reflectFileQ paths
  case mergeLayout modules of
    Left err -> fail ("reflectPipelineLayoutSig: " <> err)
    Right info -> pure [TySynD (mkName name) [] (promoteLayoutSig info)]

promoteLayoutSig :: LayoutInfo -> Type
promoteLayoutSig info =
  PromotedT 'LayoutSig
    `AppT` promoteList (promoteKeyed promoteOffsetMap) info.resources
    `AppT` promoteMaybe promoteOffsetMap info.push
