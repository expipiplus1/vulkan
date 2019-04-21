{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( DiscardRectangleModeEXT
  , pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
  , pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
  , withCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , PhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , PipelineDiscardRectangleStateCreateFlagsEXT
  , withCStructPipelineDiscardRectangleStateCreateInfoEXT
  , fromCStructPipelineDiscardRectangleStateCreateInfoEXT
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
  , cmdSetDiscardRectangleEXT
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  , pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT(..)
  , VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateFlagsEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  , vkCmdSetDiscardRectangleEXT
  , pattern VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT
  , pattern VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  , pattern VK_EXT_DISCARD_RECTANGLES_EXTENSION_NAME
  , pattern VK_EXT_DISCARD_RECTANGLES_SPEC_VERSION
  )


-- | VkDiscardRectangleModeEXT - Specify the discard rectangle mode
--
-- = See Also
--
-- No cross-references are available
type DiscardRectangleModeEXT = VkDiscardRectangleModeEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT'
-- specifies that a fragment within any discard rectangle satisfies the
-- test.
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT :: (a ~ DiscardRectangleModeEXT) => a
pattern DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT = VK_DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT'
-- specifies that a fragment not within any of the discard rectangles
-- satisfies the test.
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT :: (a ~ DiscardRectangleModeEXT) => a
pattern DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT = VK_DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT


-- | VkPhysicalDeviceDiscardRectanglePropertiesEXT - Structure describing
-- discard rectangle limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceDiscardRectanglePropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDiscardRectanglePropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceDiscardRectanglePropertiesEXT = PhysicalDeviceDiscardRectanglePropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDiscardRectanglePropertiesEXT" "maxDiscardRectangles"
  maxDiscardRectangles :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDiscardRectanglePropertiesEXT' and
-- marshal a 'PhysicalDeviceDiscardRectanglePropertiesEXT' into it. The 'VkPhysicalDeviceDiscardRectanglePropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDiscardRectanglePropertiesEXT :: PhysicalDeviceDiscardRectanglePropertiesEXT -> (VkPhysicalDeviceDiscardRectanglePropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceDiscardRectanglePropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDiscardRectanglePropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceDiscardRectanglePropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT pPNext (maxDiscardRectangles (marshalled :: PhysicalDeviceDiscardRectanglePropertiesEXT))))

-- | A function to read a 'VkPhysicalDeviceDiscardRectanglePropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDiscardRectanglePropertiesEXT'.
fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT :: VkPhysicalDeviceDiscardRectanglePropertiesEXT -> IO PhysicalDeviceDiscardRectanglePropertiesEXT
fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT c = PhysicalDeviceDiscardRectanglePropertiesEXT <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDiscardRectanglePropertiesEXT)))
                                                                                                       <*> pure (vkMaxDiscardRectangles (c :: VkPhysicalDeviceDiscardRectanglePropertiesEXT))

instance Zero PhysicalDeviceDiscardRectanglePropertiesEXT where
  zero = PhysicalDeviceDiscardRectanglePropertiesEXT Nothing
                                                     zero


-- | VkPipelineDiscardRectangleStateCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- No cross-references are available
type PipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT


-- | VkPipelineDiscardRectangleStateCreateInfoEXT - Structure specifying
-- discard rectangle
--
-- == Valid Usage
--
-- Unresolved directive in VkPipelineDiscardRectangleStateCreateInfoEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPipelineDiscardRectangleStateCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineDiscardRectangleStateCreateInfoEXT = PipelineDiscardRectangleStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "flags"
  flags :: PipelineDiscardRectangleStateCreateFlagsEXT
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "discardRectangleMode"
  discardRectangleMode :: DiscardRectangleModeEXT
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineDiscardRectangleStateCreateInfoEXT" "pDiscardRectangles"
  discardRectangles :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineDiscardRectangleStateCreateInfoEXT' and
-- marshal a 'PipelineDiscardRectangleStateCreateInfoEXT' into it. The 'VkPipelineDiscardRectangleStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineDiscardRectangleStateCreateInfoEXT :: PipelineDiscardRectangleStateCreateInfoEXT -> (VkPipelineDiscardRectangleStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineDiscardRectangleStateCreateInfoEXT marshalled cont = maybeWith (withVec withCStructRect2D) (discardRectangles (marshalled :: PipelineDiscardRectangleStateCreateInfoEXT)) (\pPDiscardRectangles -> maybeWith withSomeVkStruct (next (marshalled :: PipelineDiscardRectangleStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineDiscardRectangleStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT pPNext (flags (marshalled :: PipelineDiscardRectangleStateCreateInfoEXT)) (discardRectangleMode (marshalled :: PipelineDiscardRectangleStateCreateInfoEXT)) (maybe 0 (fromIntegral . Data.Vector.length) (discardRectangles (marshalled :: PipelineDiscardRectangleStateCreateInfoEXT))) pPDiscardRectangles)))

-- | A function to read a 'VkPipelineDiscardRectangleStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineDiscardRectangleStateCreateInfoEXT'.
fromCStructPipelineDiscardRectangleStateCreateInfoEXT :: VkPipelineDiscardRectangleStateCreateInfoEXT -> IO PipelineDiscardRectangleStateCreateInfoEXT
fromCStructPipelineDiscardRectangleStateCreateInfoEXT c = PipelineDiscardRectangleStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDiscardRectangleStateCreateInfoEXT)))
                                                                                                     <*> pure (vkFlags (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                                                                                                     <*> pure (vkDiscardRectangleMode (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))
                                                                                                     -- Optional length valued member elided
                                                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDiscardRectangleCount (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPDiscardRectangles (c :: VkPipelineDiscardRectangleStateCreateInfoEXT))

instance Zero PipelineDiscardRectangleStateCreateInfoEXT where
  zero = PipelineDiscardRectangleStateCreateInfoEXT Nothing
                                                    zero
                                                    zero
                                                    Nothing



-- | vkCmdSetDiscardRectangleEXT - Set discard rectangles dynamically
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstDiscardRectangle@ is the index of the first discard rectangle
--     whose state is updated by the command.
--
-- -   @discardRectangleCount@ is the number of discard rectangles whose
--     state are updated by the command.
--
-- -   @pDiscardRectangles@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures specifying
--     discard rectangles.
--
-- = Description
--
-- The discard rectangle taken from element i of @pDiscardRectangles@
-- replace the current state for the discard rectangle index
-- @firstDiscardRectangle@ + i, for i in [0, @discardRectangleCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     dynamic state enabled
--
-- -   The sum of @firstDiscardRectangle@ and @discardRectangleCount@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT'::@maxDiscardRectangles@
--
-- -   The @x@ and @y@ member of @offset@ in each
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) in each
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- -   Evaluation of (@offset.y@ + @extent.height@) in each
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' element of
--     @pDiscardRectangles@ /must/ not cause a signed integer addition
--     overflow
--
-- Unresolved directive in vkCmdSetDiscardRectangleEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetDiscardRectangleEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdSetDiscardRectangleEXT :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetDiscardRectangleEXT = \(CommandBuffer commandBuffer' commandTable) -> \firstDiscardRectangle' -> \discardRectangles' -> withVec withCStructRect2D discardRectangles' (\pDiscardRectangles' -> vkCmdSetDiscardRectangleEXT commandTable commandBuffer' firstDiscardRectangle' (fromIntegral $ Data.Vector.length discardRectangles') pDiscardRectangles' *> (pure ()))
