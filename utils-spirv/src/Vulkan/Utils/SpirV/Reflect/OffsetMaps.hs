{-| Extract normalized 'OffsetMap's from a reflected 'Module''s resources.

Both the runtime descriptor/push-constant builders
("Vulkan.Utils.SpirV.Descriptors") and the compile-time stage-signature
machinery ("Vulkan.Utils.SpirV.Stage") need the same thing: the std140\/std430
'OffsetMap' of each buffer-typed descriptor binding and of each push-constant block.
That extraction — pick the layout mode from the descriptor type, pull the block
'TypeDescription', run 'offsetMapOf' — lives here once so the two callers can't drift.
-}
module Vulkan.Utils.SpirV.Reflect.OffsetMaps
  ( resourceOffsetMap
  , resourceOffsetMaps
  , pushOffsetMap
  , pushOffsetMaps
  ) where

import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import Data.Word (Word32)

import Data.SpirV.Reflect.BlockVariable (BlockVariable)
import Data.SpirV.Reflect.BlockVariable qualified
import Data.SpirV.Reflect.DescriptorBinding (DescriptorBinding)
import Data.SpirV.Reflect.DescriptorBinding qualified
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified

import Vulkan.Utils.SpirV.Layout (OffsetMap, layoutForDescriptor, offsetMapOf)
import Vulkan.Utils.SpirV.Types (LayoutMode (..))

{- | The @((set, binding), offset map)@ of a single buffer-typed descriptor
binding. 'Nothing' for non-buffer descriptors (samplers, images, …) and for blocks
whose offset map can't be computed.
-}
resourceOffsetMap :: DescriptorBinding -> Maybe ((Word32, Word32), OffsetMap)
resourceOffsetMap b = do
  mode <- layoutForDescriptor b.descriptor_type
  td <- b.type_description
  g <- eitherToMaybe (offsetMapOf mode td)
  pure ((b.set, b.binding), g)

-- | The reflected block offset map of each buffer-typed binding, keyed by @(set, binding)@.
resourceOffsetMaps :: Module -> [((Word32, Word32), OffsetMap)]
resourceOffsetMaps = mapMaybe resourceOffsetMap . V.toList . (.descriptor_bindings)

-- | The std430 offset map of a push-constant block, if it can be computed.
pushOffsetMap :: BlockVariable -> Maybe OffsetMap
pushOffsetMap pc = pc.type_description >>= eitherToMaybe . offsetMapOf Std430Layout

-- | The reflected offset map of each push-constant block, keyed by @(offset, size)@.
pushOffsetMaps :: Module -> [((Word32, Word32), OffsetMap)]
pushOffsetMaps = mapMaybe keyed . V.toList . (.push_constants)
  where
    keyed pc = (,) (pc.absolute_offset, pc.size) <$> pushOffsetMap pc

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just
