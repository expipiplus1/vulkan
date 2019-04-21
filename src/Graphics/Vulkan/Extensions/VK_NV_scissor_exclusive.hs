{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( withCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , fromCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , withCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  , fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , cmdSetExclusiveScissorNV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  , pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , vkCmdSetExclusiveScissorNV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
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
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  )



-- | VkPhysicalDeviceExclusiveScissorFeaturesNV - Structure describing
-- exclusive scissor features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.VkPhysicalDeviceExclusiveScissorFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-exclusive-scissor Exclusive Scissor Test>
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.VkPhysicalDeviceExclusiveScissorFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.VkPhysicalDeviceExclusiveScissorFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- feature.
--
-- Unresolved directive in VkPhysicalDeviceExclusiveScissorFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceExclusiveScissorFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceExclusiveScissorFeaturesNV" "exclusiveScissor"
  exclusiveScissor :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceExclusiveScissorFeaturesNV' and
-- marshal a 'PhysicalDeviceExclusiveScissorFeaturesNV' into it. The 'VkPhysicalDeviceExclusiveScissorFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceExclusiveScissorFeaturesNV :: PhysicalDeviceExclusiveScissorFeaturesNV -> (VkPhysicalDeviceExclusiveScissorFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceExclusiveScissorFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceExclusiveScissorFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceExclusiveScissorFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV pPNext (boolToBool32 (exclusiveScissor (marshalled :: PhysicalDeviceExclusiveScissorFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceExclusiveScissorFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceExclusiveScissorFeaturesNV'.
fromCStructPhysicalDeviceExclusiveScissorFeaturesNV :: VkPhysicalDeviceExclusiveScissorFeaturesNV -> IO PhysicalDeviceExclusiveScissorFeaturesNV
fromCStructPhysicalDeviceExclusiveScissorFeaturesNV c = PhysicalDeviceExclusiveScissorFeaturesNV <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceExclusiveScissorFeaturesNV)))
                                                                                                 <*> pure (bool32ToBool (vkExclusiveScissor (c :: VkPhysicalDeviceExclusiveScissorFeaturesNV)))

instance Zero PhysicalDeviceExclusiveScissorFeaturesNV where
  zero = PhysicalDeviceExclusiveScissorFeaturesNV Nothing
                                                  False



-- | VkPipelineViewportExclusiveScissorStateCreateInfoNV - Structure
-- specifying parameters controlling exclusive scissor testing
--
-- = Description
--
-- If this structure is not present, @exclusiveScissorCount@ is considered
-- to be @0@ and the exclusive scissor test is disabled.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @0@ or @1@
--
-- -   @exclusiveScissorCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   @exclusiveScissorCount@ /must/ be @0@ or identical to the
--     @viewportCount@ member of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--     and @exclusiveScissorCount@ is not @0@, @pExclusiveScissors@ /must/
--     be a valid pointer to an array of @exclusiveScissorCount@
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures
--
-- Unresolved directive in
-- VkPipelineViewportExclusiveScissorStateCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkPipelineViewportExclusiveScissorStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportExclusiveScissorStateCreateInfoNV" "pExclusiveScissors"
  exclusiveScissors :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportExclusiveScissorStateCreateInfoNV' and
-- marshal a 'PipelineViewportExclusiveScissorStateCreateInfoNV' into it. The 'VkPipelineViewportExclusiveScissorStateCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportExclusiveScissorStateCreateInfoNV :: PipelineViewportExclusiveScissorStateCreateInfoNV -> (VkPipelineViewportExclusiveScissorStateCreateInfoNV -> IO a) -> IO a
withCStructPipelineViewportExclusiveScissorStateCreateInfoNV marshalled cont = maybeWith (withVec withCStructRect2D) (exclusiveScissors (marshalled :: PipelineViewportExclusiveScissorStateCreateInfoNV)) (\pPExclusiveScissors -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportExclusiveScissorStateCreateInfoNV)) (\pPNext -> cont (VkPipelineViewportExclusiveScissorStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV pPNext (maybe 0 (fromIntegral . Data.Vector.length) (exclusiveScissors (marshalled :: PipelineViewportExclusiveScissorStateCreateInfoNV))) pPExclusiveScissors)))

-- | A function to read a 'VkPipelineViewportExclusiveScissorStateCreateInfoNV' and all additional
-- structures in the pointer chain into a 'PipelineViewportExclusiveScissorStateCreateInfoNV'.
fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV :: VkPipelineViewportExclusiveScissorStateCreateInfoNV -> IO PipelineViewportExclusiveScissorStateCreateInfoNV
fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV c = PipelineViewportExclusiveScissorStateCreateInfoNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV)))
                                                                                                                   -- Optional length valued member elided
                                                                                                                   <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkExclusiveScissorCount (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPExclusiveScissors (c :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))

instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV Nothing
                                                           Nothing



-- | vkCmdSetExclusiveScissorNV - Set the dynamic exclusive scissor
-- rectangles on a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstExclusiveScissor@ is the index of the first exclusive scissor
--     rectangle whose state is updated by the command.
--
-- -   @exclusiveScissorCount@ is the number of exclusive scissor
--     rectangles updated by the command.
--
-- -   @pExclusiveScissors@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures defining
--     exclusive scissor rectangles.
--
-- = Description
--
-- The scissor rectangles taken from element i of @pExclusiveScissors@
-- replace the current state for the scissor index @firstExclusiveScissor@
-- + i, for i in [0, @exclusiveScissorCount@).
--
-- Each scissor rectangle is described by a
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structure, with the
-- @offset.x@ and @offset.y@ values determining the upper left corner of
-- the scissor rectangle, and the @extent.width@ and @extent.height@ values
-- determining the size in pixels.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-exclusiveScissor exclusive scissor>
--     feature /must/ be enabled.
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--     dynamic state enabled
--
-- -   @firstExclusiveScissor@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstExclusiveScissor@ and @exclusiveScissorCount@
--     /must/ be between @1@ and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstExclusiveScissor@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @exclusiveScissorCount@ /must/ be @1@
--
-- -   The @x@ and @y@ members of @offset@ in each member of
--     @pExclusiveScissors@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- -   Evaluation of (@offset.y@ + @extent.height@) for each member of
--     @pExclusiveScissors@ /must/ not cause a signed integer addition
--     overflow
--
-- Unresolved directive in vkCmdSetExclusiveScissorNV.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetExclusiveScissorNV.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdSetExclusiveScissorNV :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetExclusiveScissorNV = \(CommandBuffer commandBuffer' commandTable) -> \firstExclusiveScissor' -> \exclusiveScissors' -> withVec withCStructRect2D exclusiveScissors' (\pExclusiveScissors' -> vkCmdSetExclusiveScissorNV commandTable commandBuffer' firstExclusiveScissor' (fromIntegral $ Data.Vector.length exclusiveScissors') pExclusiveScissors' *> (pure ()))
