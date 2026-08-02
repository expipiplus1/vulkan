-- | The pixel tile the glitch pass dices the frame into, shared with its dispatch.
module Glitch.Grid
  ( tile
  ) where

-- | One invocation per pixel, @tile@² of them per workgroup.
tile :: Int
tile = 8
