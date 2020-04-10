module VMA.Render
  where

import           Relude                  hiding ( Handle )
import qualified Data.Map                      as Map
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

import           VMA.Bracket

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
renderHeader enums structs handles funcPointers commands = do
  bs <- brackets (snd <$> commands)
  let bracketMap      = Map.fromList [ (n, b) | (n, _, b) <- toList bs ]
      renderCommand'  = commandWithBrackets (`Map.lookup` bracketMap)
  sequenceV
    (  (traverse renderEnum <$> enums)
    <> (traverse renderStruct <$> structs)
    <> (traverse renderHandle <$> handles)
    <> (traverse renderFuncPointer <$> funcPointers)
    <> (traverse renderCommand' <$> commands)
    )

-- | Render a command along with any associated bracketing function
commandWithBrackets
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , Member Fixpoint r
     )
  => (CName -> Maybe RenderElement)
  -> MarshaledCommand
  -> Sem r RenderElement
commandWithBrackets getBracket cmd = do
  r <- renderCommand cmd
  pure $ case getBracket (mcName cmd) of
    Nothing -> r
    Just b  -> r <> b
