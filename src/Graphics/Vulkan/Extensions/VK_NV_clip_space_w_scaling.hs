{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , vkCmdSetViewportWScalingNV
  , VkViewportWScalingNV(..)
  , VkPipelineViewportWScalingStateCreateInfoNV(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkDynamicState(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VkDynamicState 1000087000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = VkStructureType 1000087000
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: Integral a => a
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"
-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling on a command
-- buffer
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose parameters
--     are updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose parameters are
--     updated by the command.
--
-- -   @pViewportWScalings@ is a pointer to an array of
--     'VkViewportWScalingNV' structures specifying viewport parameters.
--
-- = Description
-- #_description#
--
-- The viewport parameters taken from element i of @pViewportWScalings@
-- replace the current state for the viewport index @firstViewport@ + i,
-- for i in [0, @viewportCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV@ dynamic state enabled
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pViewportWScalings@ /must/ be a valid pointer to an array of
--     @viewportCount@ @VkViewportWScalingNV@ structures
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkViewportWScalingNV'
foreign import ccall "vkCmdSetViewportWScalingNV" vkCmdSetViewportWScalingNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()
-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineViewportWScalingStateCreateInfoNV',
-- 'vkCmdSetViewportWScalingNV'
data VkViewportWScalingNV = VkViewportWScalingNV
  { -- No documentation found for Nested "VkViewportWScalingNV" "vkXcoeff"
  vkXcoeff :: CFloat
  , -- No documentation found for Nested "VkViewportWScalingNV" "vkYcoeff"
  vkYcoeff :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkViewportWScalingNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkViewportWScalingNV <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkXcoeff (poked :: VkViewportWScalingNV))
                *> poke (ptr `plusPtr` 4) (vkYcoeff (poked :: VkViewportWScalingNV))
-- | VkPipelineViewportWScalingStateCreateInfoNV - Structure specifying
-- parameters of a newly created pipeline viewport W scaling state
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV@
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkViewportWScalingNV'
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "vkViewportWScalingEnable"
  vkViewportWScalingEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "vkViewportCount"
  vkViewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportWScalingStateCreateInfoNV" "vkPViewportWScalings"
  vkPViewportWScalings :: Ptr VkViewportWScalingNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportWScalingStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 20)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkViewportWScalingEnable (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportWScalings (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
