{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Khronos.Render where

import qualified Data.HashMap.Strict           as Map
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Vector.TopTraverse
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.Input
import           Relude                  hiding ( Enum
                                                , Handle
                                                )

import           Bespoke
import           Bespoke.Utils
import           CType
import           Documentation
import           Error
import           Khronos.ExtensionDepElements
import           Khronos.SPIRVElements
import qualified Khronos.Versions.OpenXR       as Xr
import           Marshal
import           Render.Alias
import           Render.Atom
import           Render.Command
import           Render.Constant
import           Render.Dynamic
import           Render.Element
import           Render.Element.Write
import           Render.Enum
import           Render.FuncPointer
import           Render.Handle
import           Render.Names
import           Render.Spec.Extends
import qualified Render.Spec.Versions          as Vk
import           Render.SpecInfo
import           Render.State                   ( HasRenderState )
import           Render.Stmts
import           Render.Struct
import           Render.Union
import           Render.VkException
import           Spec.Parse

data RenderedSpec a = RenderedSpec
  { rsHandles            :: Vector a
  , rsAtoms              :: Vector a
  , rsStructsAndUnions   :: Vector a
  , rsCommands           :: Vector a
  , rsEnums              :: Vector a
  , rsAliases            :: Vector a
  , rsFuncPointers       :: Vector a
  , rsAPIConstants       :: Vector a
  , rsExtensionConstants :: Vector a
  , rsOthers             :: Vector a
  }
  deriving (Functor, Foldable, Traversable)

renderSpec
  :: forall t r
   . ( HasErr r
     , HasTypeInfo r
     , HasRenderParams r
     , HasStmts r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderState r
     , KnownSpecFlavor t
     )
  => Spec t
  -> (Documentee -> Maybe Documentation)
  -> (  Vector MarshaledCommand
     -> Vector Handle
     -> Sem r (Vector (CName, CName, RenderElement))
     )
  -> Vector (MarshaledStruct AStruct)
  -> Vector (MarshaledStruct AUnion)
  -> Vector MarshaledCommand
  -> Sem r (RenderedSpec RenderElement)
renderSpec spec@Spec {..} getDoc brackets ss us cs = do
  RenderParams {..} <- input

  -- TODO: neaten
  -- If a struct containing an extendable struct appears in a positive position
  -- then the SomeVkStruct thing will have to be rethought.
  -- Probably solvable by passing in a list of extensions to the calling
  -- command, and still returning `SomeStruct`.
  _                 <- sequenceV
    [ getStruct t >>= traverse
        (\s2 -> forV
          (sMembers s2)
          (\m2 ->
            when (smName m2 == "pNext")
              $  whenM (appearsInPositivePosition (sName s1))
              $  throw
              $  (unCName (sName s1) <> "." <> unCName (smName m1))
              <> " >>>> "
              <> (unCName (sName s2) <> "." <> unCName (smName m2))
          )
        )
    | s1 <- toList specStructs
    , m1 <- toList (sMembers s1)
    , t  <- getAllTypeNames (smType m1)
    ]

  vkResult <-
    case
      [ e | e@Enum {..} <- toList specEnums, TypeName eName == successCodeType ]
    of
      []  -> throw "Couldn't find error code type enumeration"
      [r] -> pure r
      rs  -> throw ("Found multiple error code enumerations: " <> show rs)

  bs <- brackets cs specHandles
  let bracketMap      = Map.fromList [ (n, b) | (n, _, b) <- toList bs ]
      renderCommand'  = commandWithBrackets (`Map.lookup` bracketMap)
      filterConstants = V.filter ((`notElem` forbiddenConstants) . constName)

  -- These have to be rendered separately as the order matters, we keep track
  -- of which Storable instances are available.
  structsAndUnions <- renderStructsAndUnions ss us

  sequenceV RenderedSpec
    { rsHandles            = renderHandle <$> specHandles
    , rsAtoms              = renderAtom <$> specAtoms
    , rsStructsAndUnions   = pure <$> structsAndUnions
    , rsCommands           = renderCommand' <$> cs
    , rsEnums              = renderEnum <$> specEnums
    , rsAliases            = renderAlias <$> specAliases
    , rsFuncPointers       = renderFuncPointer <$> specFuncPointers
    , rsAPIConstants       = renderConstant <$> filterConstants specAPIConstants
    , rsExtensionConstants = renderConstant
                               <$> filterConstants specExtensionConstants
    , rsOthers             =
      bespokeElements (specFlavor @t)
      <> V.singleton (renderDynamicLoader (specFlavor @t) cs)
      <> V.singleton marshalUtils
      <> V.singleton hasObjectTypeClass
      <> V.singleton (vkExceptionRenderElement (specFlavor @t) getDoc vkResult)
      <> case sSpecFlavor @t of
           SSpecVk -> Vk.specVersions spec
           SSpecXr -> Xr.specVersions spec
      <> V.singleton (structExtends spec)
      <> case specFlavor @t of
           SpecVk -> V.singleton
             (renderSPIRVElements specSPIRVExtensions specSPIRVCapabilities)
           SpecXr -> mempty
      <> V.singleton (renderExtensionDepElements specExtensions)
    }

-- | Render a command along with any associated bracketing function
commandWithBrackets
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , Member Fixpoint r
     , HasRenderedNames r
     , HasRenderState r
     )
  => (CName -> Maybe RenderElement)
  -> MarshaledCommand
  -> Sem r RenderElement
commandWithBrackets getBracket cmd = do
  r <- renderCommand cmd
  pure $ case getBracket (mcName cmd) of
    Nothing -> r
    Just b  -> r <> b

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

renderStructsAndUnions
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     , HasRenderState r
     )
  => Vector (MarshaledStruct AStruct)
  -> Vector (MarshaledStruct AUnion)
  -> Sem r (Vector RenderElement)
renderStructsAndUnions ss us = traverseInTopOrder
  (either msName msName)
  (either (immediateDepends . msStruct) (immediateDepends . msStruct))
  (either renderStruct renderUnion)
  ((Left <$> ss) <> (Right <$> us))

immediateDepends :: StructOrUnion t s c -> [CName]
immediateDepends Struct {..} =
  [ n
  | sName `notElem` cycleBreakers
  , StructMember {..} <- V.toList sMembers
  , n                 <- getAllTypeNames smType
  ]

-- These are the only structs with cycles, don't bother using their storable
-- instances in poking as they're only used once
cycleBreakers :: [CName]
cycleBreakers =
  [ "VkBaseInStructure"
  , "VkBaseOutStructure"
  , "XrBaseInStructure"
  , "XrBaseOutStructure"
  ]
