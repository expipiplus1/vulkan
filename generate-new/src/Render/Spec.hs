{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Render.Spec
  where

import           Relude                  hiding ( Enum )
import           Polysemy
import           Polysemy.Input
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import qualified Data.HashMap.Strict           as Map

import           Bespoke
import           Bespoke.Utils
import           Bracket
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
import           Render.SpecInfo
import           Render.Spec.Versions
import           Render.Spec.Extends
import           Render.Stmts
import           Render.Struct
import           Render.Union
import           Render.VkException
import           Spec.Parse
import           CType
import           Documentation

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

  bs <- brackets specHandles
  let bracketMap      = Map.fromList [ (n, b) | (n, _, b) <- toList bs ]
      renderCommand'  = commandWithBrackets (`Map.lookup` bracketMap)
      filterConstants = V.filter ((`notElem` forbiddenConstants) . constName)
  sequenceV RenderedSpec
    { rsHandles            = renderHandle <$> specHandles
    , rsStructsAndUnions   = fmap renderStruct ss <> fmap renderUnion us
    , rsCommands           = renderCommand' <$> cs
    , rsEnums              = renderEnum <$> specEnums
    , rsAliases            = renderAlias <$> specAliases
    , rsFuncPointers       = renderFuncPointer <$> specFuncPointers
    , rsAPIConstants       = renderConstant <$> filterConstants specAPIConstants
    , rsExtensionConstants = renderConstant
                               <$> filterConstants specExtensionConstants
    , rsOthers             = bespokeElements
                             <> V.singleton (renderDynamicLoader cs)
                             <> cStructDocs
                             <> V.singleton marshalUtils
                             <> V.singleton zeroClass
                             <> V.singleton (vkExceptionRenderElement getDoc vkResult)
                             <> specVersions spec
                             <> V.singleton (structExtends spec)
    }

-- | Render a command along with any associated bracketing function
commandWithBrackets
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasStmts r
     , HasRenderedNames r
     )
  => (CName -> Maybe RenderElement)
  -> MarshaledCommand
  -> Sem r RenderElement
commandWithBrackets getBracket cmd = do
  r <- renderCommand cmd
  pure $ case getBracket (mcName cmd) of
    Nothing -> r
    Just b  -> r <> b
