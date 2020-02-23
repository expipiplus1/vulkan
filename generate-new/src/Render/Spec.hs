module Render.Spec
  where

import           Relude                  hiding ( Reader )
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           Bespoke
import           Bespoke.Utils
import           Error
import           Marshal
import           Render.Alias
import           Render.Command
import           Render.Constant
import           Render.Dynamic
import           Render.Element
import           Render.Element.Write
import           Render.Enum
import           Render.FuncPointer
import           Render.Handle
import           Render.Struct
import           Render.CStruct
import           Render.Union
import           Render.SpecInfo
import           Spec.Parse

renderSpec
  :: (HasErr r, HasTypeInfo r, MemberWithError (Reader RenderParams) r)
  => Spec
  -> Vector (MarshaledStruct AStruct)
  -> Vector (MarshaledStruct AUnion)
  -> Vector MarshaledCommand
  -> Sem r (Vector RenderElement)
renderSpec s@Spec {..} ss us cs =
  withSpecInfo s $ liftA2 (<>) bespokeElements $ sequenceV
    (  fmap renderHandle      specHandles
    <> fmap renderStruct      ss
    <> fmap renderUnion       us
    <> fmap renderCommand     cs
    <> fmap renderEnum        specEnums
    <> fmap renderAlias       specAliases
    <> fmap renderFuncPointer specFuncPointers
    <> fmap
         renderConstant
         (V.filter ((`notElem` forbiddenConstants) . constName) specConstants)
    <> V.singleton (renderDynamicLoader cs)
    <> cStructDocs
    <> V.singleton marshalUtils
    <> V.singleton zeroClass
    )

