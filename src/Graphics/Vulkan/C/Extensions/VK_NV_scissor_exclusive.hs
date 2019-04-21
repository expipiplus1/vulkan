{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , FN_vkCmdSetExclusiveScissorNV
  , PFN_vkCmdSetExclusiveScissorNV
  , vkCmdSetExclusiveScissorNV
  , pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkDynamicState(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPhysicalDeviceExclusiveScissorFeaturesNV - Structure describing
-- exclusive scissor features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceExclusiveScissorFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-exclusive-scissor Exclusive Scissor Test>
-- for more information.
--
-- If the 'VkPhysicalDeviceExclusiveScissorFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'VkPhysicalDeviceExclusiveScissorFeaturesNV' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable the feature.
--
-- Unresolved directive in VkPhysicalDeviceExclusiveScissorFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceExclusiveScissorFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceExclusiveScissorFeaturesNV = VkPhysicalDeviceExclusiveScissorFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @exclusiveScissor@ indicates that the implementation supports the
  -- exclusive scissor test.
  vkExclusiveScissor :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceExclusiveScissorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceExclusiveScissorFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkExclusiveScissor (poked :: VkPhysicalDeviceExclusiveScissorFeaturesNV))

instance Zero VkPhysicalDeviceExclusiveScissorFeaturesNV where
  zero = VkPhysicalDeviceExclusiveScissorFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
                                                    zero
                                                    zero

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
--     'VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV' and @exclusiveScissorCount@
--     is not @0@, @pExclusiveScissors@ /must/ be a valid pointer to an
--     array of @exclusiveScissorCount@
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures
--
-- Unresolved directive in
-- VkPipelineViewportExclusiveScissorStateCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkPipelineViewportExclusiveScissorStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPipelineViewportExclusiveScissorStateCreateInfoNV = VkPipelineViewportExclusiveScissorStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @exclusiveScissorCount@ is the number of exclusive scissor rectangles
  -- used by the pipeline.
  vkExclusiveScissorCount :: Word32
  , -- | @pExclusiveScissors@ is a pointer to an array of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures defining
  -- exclusive scissor rectangles. If the exclusive scissor state is dynamic,
  -- this member is ignored.
  vkPExclusiveScissors :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportExclusiveScissorStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
                                                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkExclusiveScissorCount (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPExclusiveScissors (poked :: VkPipelineViewportExclusiveScissorStateCreateInfoNV))

instance Zero VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = VkPipelineViewportExclusiveScissorStateCreateInfoNV VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
                                                             zero
                                                             zero
                                                             zero

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
--     'VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV' dynamic state enabled
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetExclusiveScissorNV" vkCmdSetExclusiveScissorNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()
#else
vkCmdSetExclusiveScissorNV :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()
vkCmdSetExclusiveScissorNV deviceCmds = mkVkCmdSetExclusiveScissorNV (pVkCmdSetExclusiveScissorNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExclusiveScissorNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ())
#endif

type FN_vkCmdSetExclusiveScissorNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetExclusiveScissorNV = FunPtr FN_vkCmdSetExclusiveScissorNV

-- | 'VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV' specifies that the
-- @pExclusiveScissors@ state in
-- 'VkPipelineViewportExclusiveScissorStateCreateInfoNV' will be ignored
-- and /must/ be set dynamically with 'vkCmdSetExclusiveScissorNV' before
-- any draw commands. The number of exclusive scissor rectangles used by a
-- pipeline is still specified by the @exclusiveScissorCount@ member of
-- 'VkPipelineViewportExclusiveScissorStateCreateInfoNV'.
pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV = VkDynamicState 1000205001

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV = VkStructureType 1000205002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV = VkStructureType 1000205000
