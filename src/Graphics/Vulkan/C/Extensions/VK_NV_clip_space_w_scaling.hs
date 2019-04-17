{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdSetViewportWScalingNV
#endif
  , FN_vkCmdSetViewportWScalingNV
  , PFN_vkCmdSetViewportWScalingNV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
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
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPipelineViewportWScalingStateCreateInfoNV - Structure specifying
-- parameters of a newly created pipeline viewport W scaling state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @viewportWScalingEnable@ controls whether viewport __W__ scaling is
  -- enabled.
  vkViewportWScalingEnable :: VkBool32
  , -- | @viewportCount@ /must/ be greater than @0@
  vkViewportCount :: Word32
  , -- | @pViewportWScalings@ is a pointer to an array of @VkViewportWScalingNV@
  -- structures, which define the __W__ scaling parameters for the
  -- corresponding viewport. If the viewport __W__ scaling state is dynamic,
  -- this member is ignored.
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

instance Zero VkPipelineViewportWScalingStateCreateInfoNV where
  zero = VkPipelineViewportWScalingStateCreateInfoNV zero
                                                     zero
                                                     zero
                                                     zero
                                                     zero
-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = See Also
--
-- No cross-references are available
data VkViewportWScalingNV = VkViewportWScalingNV
  { -- | @xcoeff@ and @ycoeff@ are the viewport’s W scaling factor for x and y
  -- respectively.
  vkXcoeff :: CFloat
  , -- No documentation found for Nested "VkViewportWScalingNV" "ycoeff"
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

instance Zero VkViewportWScalingNV where
  zero = VkViewportWScalingNV zero
                              zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling on a command
-- buffer
--
-- = Parameters
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
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
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
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
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
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewportWScalingNV" vkCmdSetViewportWScalingNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()

#endif
type FN_vkCmdSetViewportWScalingNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()
type PFN_vkCmdSetViewportWScalingNV = FunPtr FN_vkCmdSetViewportWScalingNV
-- | @VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV@ specifies that the
-- @pViewportScalings@ state in
-- 'VkPipelineViewportWScalingStateCreateInfoNV' will be ignored and /must/
-- be set dynamically with 'vkCmdSetViewportWScalingNV' before any draws
-- are performed with a pipeline state with
-- 'VkPipelineViewportWScalingStateCreateInfoNV' member
-- @viewportScalingEnable@ set to @VK_TRUE@
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VkDynamicState 1000087000
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: Integral a => a
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = VkStructureType 1000087000
