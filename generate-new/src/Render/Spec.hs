module Render.Spec
  where

import           Relude                  hiding ( Reader )
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

import           Bespoke
import           Error
import           Marshal
import           Render.Alias
import           Render.Command
import           Render.Element
import           Render.Enum
import           Render.FuncPointer
import           Render.Handle
import           Render.Struct
import           Render.Constant
import           Render.Type
import           Render.Union
import           Render.Dynamic
import           Spec.Parse

renderSpec
  :: (HasErr r, HasTypeInfo r, MemberWithError (Reader RenderParams) r)
  => Spec
  -> Vector MarshaledStruct
  -> Vector MarshaledCommand
  -> Sem r (Vector RenderElement)
renderSpec Spec {..} ss cs = liftA2 (<>) bespokeElements $ sequenceV
  (  fmap renderHandle      specHandles
  <> fmap renderStruct      ss
  <> fmap renderUnion       specUnions
  <> fmap renderCommand     cs
  <> fmap renderEnum        specEnums
  <> fmap renderAlias       specAliases
  <> fmap renderFuncPointer specFuncPointers
  <> fmap
       renderConstant
       (V.filter ((`notElem` forbiddenConstants) . constName) specConstants)
  <> V.singleton (renderDynamicLoader cs)
  )

