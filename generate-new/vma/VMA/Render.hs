module VMA.Render
  where

import           Relude                  hiding ( Handle )
import           Data.Vector                    ( Vector )
import           Polysemy
import           Polysemy.Fixpoint

import           Render.SpecInfo
import           Render.Enum
import           Render.Struct
import           Render.Element
import           Render.Handle
import           Render.FuncPointer
import           Render.Command
import           Marshal.Struct
import           Marshal.Command
import           Spec.Types
import           Error
import           Render.Names

renderHeader
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , Member Fixpoint r
     )
  => Vector (a, Enum')
  -> Vector (a, MarshaledStruct AStruct)
  -> Vector (a, Handle)
  -> Vector (a, FuncPointer)
  -> Vector (a, MarshaledCommand)
  -> Sem r (Vector (a, RenderElement))
renderHeader enums structs handles funcPointers commands = sequenceV
  (  (traverse renderEnum <$> enums)
  <> (traverse renderStruct <$> structs)
  <> (traverse renderHandle <$> handles)
  <> (traverse renderFuncPointer <$> funcPointers)
  <> (traverse renderCommand <$> commands)
  )

