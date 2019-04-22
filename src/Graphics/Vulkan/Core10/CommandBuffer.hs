{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.CommandBuffer
  ( withCStructCommandBufferAllocateInfo
  , fromCStructCommandBufferAllocateInfo
  , CommandBufferAllocateInfo(..)
  , withCStructCommandBufferBeginInfo
  , fromCStructCommandBufferBeginInfo
  , CommandBufferBeginInfo(..)
  , withCStructCommandBufferInheritanceInfo
  , fromCStructCommandBufferInheritanceInfo
  , CommandBufferInheritanceInfo(..)
  , CommandBufferLevel
  , pattern COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern COMMAND_BUFFER_LEVEL_SECONDARY
  , CommandBufferResetFlagBits
  , pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , CommandBufferResetFlags
  , CommandBufferUsageFlagBits
  , pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , CommandBufferUsageFlags
  , QueryControlFlagBits
  , pattern QUERY_CONTROL_PRECISE_BIT
  , QueryControlFlags
  , allocateCommandBuffers
  , beginCommandBuffer
  , endCommandBuffer
  , freeCommandBuffers
  , resetCommandBuffer
  , useCommandBuffer
  , withCommandBuffers
  ) where

import Control.Exception
  ( bracket
  , bracket_
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
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
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  , VkCommandBufferLevel(..)
  , VkCommandBufferResetFlagBits(..)
  , VkCommandBufferUsageFlagBits(..)
  , VkQueryControlFlagBits(..)
  , vkAllocateCommandBuffers
  , vkBeginCommandBuffer
  , vkEndCommandBuffer
  , vkFreeCommandBuffers
  , vkResetCommandBuffer
  , pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY
  , pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
  , pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT
  , pattern VK_QUERY_CONTROL_PRECISE_BIT
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( Framebuffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkCommandBufferAllocateInfo - Structure specifying the allocation
-- parameters for command buffer object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferLevel',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers'
data CommandBufferAllocateInfo = CommandBufferAllocateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "CommandBufferAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandPool"
  commandPool :: CommandPool
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "level"
  level :: CommandBufferLevel
  , -- No documentation found for Nested "CommandBufferAllocateInfo" "commandBufferCount"
  commandBufferCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCommandBufferAllocateInfo' and
-- marshal a 'CommandBufferAllocateInfo' into it. The 'VkCommandBufferAllocateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCommandBufferAllocateInfo :: CommandBufferAllocateInfo -> (VkCommandBufferAllocateInfo -> IO a) -> IO a
withCStructCommandBufferAllocateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CommandBufferAllocateInfo)) (\pPNext -> cont (VkCommandBufferAllocateInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO pPNext (commandPool (marshalled :: CommandBufferAllocateInfo)) (level (marshalled :: CommandBufferAllocateInfo)) (commandBufferCount (marshalled :: CommandBufferAllocateInfo))))

-- | A function to read a 'VkCommandBufferAllocateInfo' and all additional
-- structures in the pointer chain into a 'CommandBufferAllocateInfo'.
fromCStructCommandBufferAllocateInfo :: VkCommandBufferAllocateInfo -> IO CommandBufferAllocateInfo
fromCStructCommandBufferAllocateInfo c = CommandBufferAllocateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferAllocateInfo)))
                                                                   <*> pure (vkCommandPool (c :: VkCommandBufferAllocateInfo))
                                                                   <*> pure (vkLevel (c :: VkCommandBufferAllocateInfo))
                                                                   <*> pure (vkCommandBufferCount (c :: VkCommandBufferAllocateInfo))

instance Zero CommandBufferAllocateInfo where
  zero = CommandBufferAllocateInfo Nothing
                                   zero
                                   zero
                                   zero



-- | VkCommandBufferBeginInfo - Structure specifying a command buffer begin
-- operation
--
-- == Valid Usage
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the @renderPass@ member of @pInheritanceInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the @subpass@ member of @pInheritanceInfo@ /must/ be a valid subpass
--     index within the @renderPass@ member of @pInheritanceInfo@
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT',
--     the @framebuffer@ member of @pInheritanceInfo@ /must/ be either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', or a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' that is compatible
--     with the @renderPass@ member of @pInheritanceInfo@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupCommandBufferBeginInfo'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer'
data CommandBufferBeginInfo = CommandBufferBeginInfo
  { -- Univalued member elided
  -- No documentation found for Nested "CommandBufferBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferBeginInfo" "flags"
  flags :: CommandBufferUsageFlags
  , -- No documentation found for Nested "CommandBufferBeginInfo" "pInheritanceInfo"
  inheritanceInfo :: Maybe CommandBufferInheritanceInfo
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCommandBufferBeginInfo' and
-- marshal a 'CommandBufferBeginInfo' into it. The 'VkCommandBufferBeginInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCommandBufferBeginInfo :: CommandBufferBeginInfo -> (VkCommandBufferBeginInfo -> IO a) -> IO a
withCStructCommandBufferBeginInfo marshalled cont = maybeWith (\a -> withCStructCommandBufferInheritanceInfo a . flip with) (inheritanceInfo (marshalled :: CommandBufferBeginInfo)) (\pPInheritanceInfo -> maybeWith withSomeVkStruct (next (marshalled :: CommandBufferBeginInfo)) (\pPNext -> cont (VkCommandBufferBeginInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO pPNext (flags (marshalled :: CommandBufferBeginInfo)) pPInheritanceInfo)))

-- | A function to read a 'VkCommandBufferBeginInfo' and all additional
-- structures in the pointer chain into a 'CommandBufferBeginInfo'.
fromCStructCommandBufferBeginInfo :: VkCommandBufferBeginInfo -> IO CommandBufferBeginInfo
fromCStructCommandBufferBeginInfo c = CommandBufferBeginInfo <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferBeginInfo)))
                                                             <*> pure (vkFlags (c :: VkCommandBufferBeginInfo))
                                                             <*> maybePeek (fromCStructCommandBufferInheritanceInfo <=< peek) (vkPInheritanceInfo (c :: VkCommandBufferBeginInfo))

instance Zero CommandBufferBeginInfo where
  zero = CommandBufferBeginInfo Nothing
                                zero
                                Nothing



-- | VkCommandBufferInheritanceInfo - Structure specifying command buffer
-- inheritance info
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @occlusionQueryEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is enabled, @queryFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     values
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @pipelineStatistics@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkCommandBufferInheritanceConditionalRenderingInfoEXT'
--
-- -   Both of @framebuffer@, and @renderPass@ that are valid handles
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data CommandBufferInheritanceInfo = CommandBufferInheritanceInfo
  { -- Univalued member elided
  -- No documentation found for Nested "CommandBufferInheritanceInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "renderPass"
  renderPass :: RenderPass
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "subpass"
  subpass :: Word32
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "framebuffer"
  framebuffer :: Framebuffer
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "occlusionQueryEnable"
  occlusionQueryEnable :: Bool
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "queryFlags"
  queryFlags :: QueryControlFlags
  , -- No documentation found for Nested "CommandBufferInheritanceInfo" "pipelineStatistics"
  pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCommandBufferInheritanceInfo' and
-- marshal a 'CommandBufferInheritanceInfo' into it. The 'VkCommandBufferInheritanceInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCommandBufferInheritanceInfo :: CommandBufferInheritanceInfo -> (VkCommandBufferInheritanceInfo -> IO a) -> IO a
withCStructCommandBufferInheritanceInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CommandBufferInheritanceInfo)) (\pPNext -> cont (VkCommandBufferInheritanceInfo VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO pPNext (renderPass (marshalled :: CommandBufferInheritanceInfo)) (subpass (marshalled :: CommandBufferInheritanceInfo)) (framebuffer (marshalled :: CommandBufferInheritanceInfo)) (boolToBool32 (occlusionQueryEnable (marshalled :: CommandBufferInheritanceInfo))) (queryFlags (marshalled :: CommandBufferInheritanceInfo)) (pipelineStatistics (marshalled :: CommandBufferInheritanceInfo))))

-- | A function to read a 'VkCommandBufferInheritanceInfo' and all additional
-- structures in the pointer chain into a 'CommandBufferInheritanceInfo'.
fromCStructCommandBufferInheritanceInfo :: VkCommandBufferInheritanceInfo -> IO CommandBufferInheritanceInfo
fromCStructCommandBufferInheritanceInfo c = CommandBufferInheritanceInfo <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandBufferInheritanceInfo)))
                                                                         <*> pure (vkRenderPass (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkSubpass (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkFramebuffer (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (bool32ToBool (vkOcclusionQueryEnable (c :: VkCommandBufferInheritanceInfo)))
                                                                         <*> pure (vkQueryFlags (c :: VkCommandBufferInheritanceInfo))
                                                                         <*> pure (vkPipelineStatistics (c :: VkCommandBufferInheritanceInfo))

instance Zero CommandBufferInheritanceInfo where
  zero = CommandBufferInheritanceInfo Nothing
                                      zero
                                      zero
                                      zero
                                      False
                                      zero
                                      zero


-- | VkCommandBufferLevel - Enumerant specifying a command buffer level
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo'
type CommandBufferLevel = VkCommandBufferLevel


{-# complete COMMAND_BUFFER_LEVEL_PRIMARY, COMMAND_BUFFER_LEVEL_SECONDARY :: CommandBufferLevel #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_LEVEL_PRIMARY'
-- specifies a primary command buffer.
pattern COMMAND_BUFFER_LEVEL_PRIMARY :: (a ~ CommandBufferLevel) => a
pattern COMMAND_BUFFER_LEVEL_PRIMARY = VK_COMMAND_BUFFER_LEVEL_PRIMARY


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_LEVEL_SECONDARY'
-- specifies a secondary command buffer.
pattern COMMAND_BUFFER_LEVEL_SECONDARY :: (a ~ CommandBufferLevel) => a
pattern COMMAND_BUFFER_LEVEL_SECONDARY = VK_COMMAND_BUFFER_LEVEL_SECONDARY

-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlags'
type CommandBufferResetFlagBits = VkCommandBufferResetFlagBits


{-# complete COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: CommandBufferResetFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT'
-- specifies that most or all memory resources currently owned by the
-- command buffer /should/ be returned to the parent command pool. If this
-- flag is not set, then the command buffer /may/ hold onto memory
-- resources and reuse them when recording commands. @commandBuffer@ is
-- moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT :: (a ~ CommandBufferResetFlagBits) => a
pattern COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT

-- | VkCommandBufferResetFlags - Bitmask of VkCommandBufferResetFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer'
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- | VkCommandBufferUsageFlagBits - Bitmask specifying usage behavior for
-- command buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlags'
type CommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits


{-# complete COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: CommandBufferUsageFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
-- specifies that each recording of the command buffer will only be
-- submitted once, and the command buffer will be reset and recorded again
-- between each submission.
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- specifies that a secondary command buffer is considered to be entirely
-- inside a render pass. If this is a primary command buffer, then this bit
-- is ignored.
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
-- specifies that a command buffer /can/ be resubmitted to a queue while it
-- is in the /pending state/, and recorded into multiple primary command
-- buffers.
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT :: (a ~ CommandBufferUsageFlagBits) => a
pattern COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

-- | VkCommandBufferUsageFlags - Bitmask of VkCommandBufferUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlagBits'
type CommandBufferUsageFlags = CommandBufferUsageFlagBits

-- | VkQueryControlFlagBits - Bitmask specifying constraints on a query
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags'
type QueryControlFlagBits = VkQueryControlFlagBits


{-# complete QUERY_CONTROL_PRECISE_BIT :: QueryControlFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT'
-- specifies the precision of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-occlusion occlusion queries>.
pattern QUERY_CONTROL_PRECISE_BIT :: (a ~ QueryControlFlagBits) => a
pattern QUERY_CONTROL_PRECISE_BIT = VK_QUERY_CONTROL_PRECISE_BIT

-- | VkQueryControlFlags - Bitmask of VkQueryControlFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginQueryIndexedEXT'
type QueryControlFlags = QueryControlFlagBits


-- | vkAllocateCommandBuffers - Allocate command buffers from an existing
-- command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo'
--     structure describing parameters of the allocation.
--
-- -   @pCommandBuffers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles in which
--     the resulting command buffer objects are returned. The array /must/
--     be at least the length specified by the @commandBufferCount@ member
--     of @pAllocateInfo@. Each allocated command buffer begins in the
--     initial state.
--
-- = Description
--
-- When command buffers are first allocated, they are in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo'
--     structure
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @pAllocateInfo@::commandBufferCount
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles
--
-- == Host Synchronization
--
-- -   Host access to @pAllocateInfo@::commandPool /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
allocateCommandBuffers :: Device ->  CommandBufferAllocateInfo ->  IO (Vector CommandBuffer)
allocateCommandBuffers = \(Device device' commandTable) -> \allocateInfo' -> allocaArray (fromIntegral (commandBufferCount (allocateInfo' :: CommandBufferAllocateInfo))) (\pCommandBuffers' -> (\marshalled -> withCStructCommandBufferAllocateInfo marshalled . flip with) allocateInfo' (\pAllocateInfo' -> vkAllocateCommandBuffers commandTable device' pAllocateInfo' pCommandBuffers' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM (fromIntegral (commandBufferCount (allocateInfo' :: CommandBufferAllocateInfo))) ((\p i -> CommandBuffer <$> peekElemOff p i <*> pure commandTable) pCommandBuffers'))))))


-- | vkBeginCommandBuffer - Start recording a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the handle of the command buffer which is to be
--     put in the recording state.
--
-- -   @pBeginInfo@ is an instance of the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo'
--     structure, which defines additional information about how the
--     command buffer begins recording.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or pending state>.
--
-- -   If @commandBuffer@ was allocated from a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' which did not
--     have the
--     'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
--     flag set, @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- -   If @commandBuffer@ is a secondary command buffer, the
--     @pInheritanceInfo@ member of @pBeginInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'
--     structure
--
-- -   If @commandBuffer@ is a secondary command buffer and either the
--     @occlusionQueryEnable@ member of the @pInheritanceInfo@ member of
--     @pBeginInfo@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', or the
--     precise occlusion queries feature is not enabled, the @queryFlags@
--     member of the @pInheritanceInfo@ member @pBeginInfo@ /must/ not
--     contain
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pBeginInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo'
beginCommandBuffer :: CommandBuffer ->  CommandBufferBeginInfo ->  IO ()
beginCommandBuffer = \(CommandBuffer commandBuffer' commandTable) -> \beginInfo' -> (\marshalled -> withCStructCommandBufferBeginInfo marshalled . flip with) beginInfo' (\pBeginInfo' -> vkBeginCommandBuffer commandTable commandBuffer' pBeginInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkEndCommandBuffer - Finish recording a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer to complete recording.
--
-- = Description
--
-- If there was an error during recording, the application will be notified
-- by an unsuccessful return code returned by
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkEndCommandBuffer'. If the
-- application wishes to further use the command buffer, the command buffer
-- /must/ be reset. The command buffer /must/ have been in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>,
-- and is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>.
--
-- -   If @commandBuffer@ is a primary command buffer, there /must/ not be
--     an active render pass instance
--
-- -   All queries made
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     during the recording of @commandBuffer@ /must/ have been made
--     inactive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
endCommandBuffer :: CommandBuffer ->  IO ()
endCommandBuffer = \(CommandBuffer commandBuffer' commandTable) -> vkEndCommandBuffer commandTable commandBuffer' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkFreeCommandBuffers - Free command buffers
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool from which the command buffers
--     were allocated.
--
-- -   @commandBufferCount@ is the length of the @pCommandBuffers@ array.
--
-- -   @pCommandBuffers@ is an array of handles of command buffers to free.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has any element of @pCommandBuffers@ recorded into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All elements of @pCommandBuffers@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles, each
--     element of which /must/ either be a valid handle or @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @commandPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' handle
--
-- -   @commandBufferCount@ /must/ be greater than @0@
--
-- -   @commandPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   Each element of @pCommandBuffers@ that is a valid handle /must/ have
--     been created, allocated, or retrieved from @commandPool@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- -   Host access to each member of @pCommandBuffers@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
freeCommandBuffers :: Device ->  CommandPool ->  Vector CommandBuffer ->  IO ()
freeCommandBuffers = \(Device device' commandTable) -> \commandPool' -> \commandBuffers' -> withVec ((&) . commandBufferHandle) commandBuffers' (\pCommandBuffers' -> vkFreeCommandBuffers commandTable device' commandPool' (fromIntegral $ Data.Vector.length commandBuffers') pCommandBuffers' *> (pure ()))


-- | vkResetCommandBuffer - Reset a command buffer to the initial state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer to reset. The command buffer
--     /can/ be in any state other than
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending>,
--     and is moved into the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits'
--     controlling the reset operation.
--
-- = Description
--
-- Any primary command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has @commandBuffer@ recorded into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   @commandBuffer@ /must/ have been allocated from a pool that was
--     created with the
--     'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits'
--     values
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlags'
resetCommandBuffer :: CommandBuffer ->  CommandBufferResetFlags ->  IO ()
resetCommandBuffer = \(CommandBuffer commandBuffer' commandTable) -> \flags' -> vkResetCommandBuffer commandTable commandBuffer' flags' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))

-- | A safe wrapper for 'beginCommandBuffer' and 'endCommandBuffer' using 'bracket_'
--
-- The allocated value must not be returned from the provided computation
useCommandBuffer
  :: CommandBuffer -> CommandBufferBeginInfo -> IO a -> IO a
useCommandBuffer commandBuffer commandBufferBeginInfo = bracket_
  (beginCommandBuffer commandBuffer commandBufferBeginInfo)
  ( endCommandBuffer commandBuffer)

-- | A safe wrapper for 'allocateCommandBuffers' and 'freeCommandBuffers' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withCommandBuffers
  :: Device -> CommandBufferAllocateInfo -> (Vector (CommandBuffer) -> IO a) -> IO a
withCommandBuffers device commandBufferAllocateInfo = bracket
  (allocateCommandBuffers device commandBufferAllocateInfo)
  (\o -> freeCommandBuffers device (commandPool (commandBufferAllocateInfo :: CommandBufferAllocateInfo))  o)
