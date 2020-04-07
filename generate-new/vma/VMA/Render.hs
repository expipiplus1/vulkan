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
  => Vector Enum'
  -> Vector (MarshaledStruct AStruct)
  -> Vector Handle
  -> Vector FuncPointer
  -> Vector MarshaledCommand
  -> Sem r (Vector RenderElement)
renderHeader enums structs handles funcPointers commands = sequenceV
  (  (renderEnum <$> enums)
  <> (renderStruct <$> structs)
  <> (renderHandle <$> handles)
  <> (renderFuncPointer <$> funcPointers)
  <> (renderCommand <$> commands)
  )

