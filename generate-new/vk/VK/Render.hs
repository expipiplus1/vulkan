{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module VK.Render where

import qualified Data.HashMap.Strict           as Map
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Vector.TopTraverse
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.Input
import           Relude                  hiding ( Enum )

import           Bespoke
import           Bespoke.Utils
import           CType
import           Documentation
import           Error
import           Marshal
import           Render.Alias
import           Render.CStruct
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
import           Render.Spec.Versions
import           Render.SpecInfo
import           Render.Stmts
import           Render.Struct
import           Render.Union
import           Render.VkException
import           Spec.Parse

import           Render.State                   ( HasRenderState )
import           VK.Bracket
import           VK.SPIRVElements

data RenderedSpec a = RenderedSpec
  { rsHandles            :: Vector a
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
  :: ( HasErr r
     , HasTypeInfo r
     , HasRenderParams r
     , HasStmts r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderState r
     )
  => Spec
  -> (Documentee -> Maybe Documentation)
  -> Vector (MarshaledStruct AStruct)
  -> Vector (MarshaledStruct AUnion)
  -> Vector MarshaledCommand
  -> Sem r (RenderedSpec RenderElement)
renderSpec spec@Spec {..} getDoc ss us cs = do
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
    , rsStructsAndUnions   = pure <$> structsAndUnions
    , rsCommands           = renderCommand' <$> cs
    , rsEnums              = renderEnum <$> specEnums
    , rsAliases            = renderAlias <$> specAliases
    , rsFuncPointers       = renderFuncPointer <$> specFuncPointers
    , rsAPIConstants       = renderConstant <$> filterConstants specAPIConstants
    , rsExtensionConstants = renderConstant
                               <$> filterConstants specExtensionConstants
    , rsOthers             =
      bespokeElements
      <> V.singleton (renderDynamicLoader cs)
      <> cStructDocs
      <> V.singleton marshalUtils
      <> V.singleton zeroClass
      <> V.singleton hasObjectTypeClass
      <> V.singleton (vkExceptionRenderElement getDoc vkResult)
      <> specVersions spec
      <> V.singleton (structExtends spec)
      <> V.singleton
           (renderSPIRVElements specSPIRVExtensions specSPIRVCapabilities)
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
cycleBreakers = ["VkBaseInStructure", "VkBaseOutStructure"]
