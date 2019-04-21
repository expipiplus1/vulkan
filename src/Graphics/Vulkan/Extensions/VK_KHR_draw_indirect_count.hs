{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count
  ( cmdDrawIndexedIndirectCountKHR
  , cmdDrawIndirectCountKHR
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  ) where

import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountKHR
  , vkCmdDrawIndirectCountKHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  )



-- | vkCmdDrawIndexedIndirectCountKHR - Perform an indexed indirect draw with
-- the draw count sourced from a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndexedIndirectCountKHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_vertex_binding.txt[]
-- Unresolved directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_indirect_common.txt[]
-- Unresolved directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_indirect_count_common.txt[] *
-- @stride@ /must/ be a multiple of @4@ and /must/ be greater than or equal
-- to
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand')
-- * If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
-- (@maxDrawCount@ - 1) + @offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@ * If count stored
-- in @countBuffer@ is equal to @1@, (@offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@ * If count stored
-- in @countBuffer@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
-- @offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@
--
-- Unresolved directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawIndexedIndirectCountKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawIndexedIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirectCountKHR = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \countBuffer' -> \countBufferOffset' -> \maxDrawCount' -> \stride' -> vkCmdDrawIndexedIndirectCountKHR commandTable commandBuffer' buffer' offset' countBuffer' countBufferOffset' maxDrawCount' stride' *> (pure ())


-- | vkCmdDrawIndirectCountKHR - Perform an indirect draw with the draw count
-- sourced from a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndirectCountKHR'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- Unresolved directive in vkCmdDrawIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_common.txt[] Unresolved
-- directive in vkCmdDrawIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_vertex_binding.txt[]
-- Unresolved directive in vkCmdDrawIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_indirect_common.txt[]
-- Unresolved directive in vkCmdDrawIndirectCountKHR.txt -
-- include::{chapters}\/commonvalidity\/draw_indirect_count_common.txt[] *
-- @stride@ /must/ be a multiple of @4@ and /must/ be greater than or equal
-- to
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand')
-- * If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
-- (@maxDrawCount@ - 1) + @offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@ * If the count
-- stored in @countBuffer@ is equal to @1@, (@offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@ * If the count
-- stored in @countBuffer@ is greater than @1@, (@stride@ × (@drawCount@ -
-- 1) + @offset@ +
-- sizeof('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
-- /must/ be less than or equal to the size of @buffer@
--
-- Unresolved directive in vkCmdDrawIndirectCountKHR.txt -
-- include::{generated}\/validity\/protos\/vkCmdDrawIndirectCountKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdDrawIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectCountKHR = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \countBuffer' -> \countBufferOffset' -> \maxDrawCount' -> \stride' -> vkCmdDrawIndirectCountKHR commandTable commandBuffer' buffer' offset' countBuffer' countBufferOffset' maxDrawCount' stride' *> (pure ())
