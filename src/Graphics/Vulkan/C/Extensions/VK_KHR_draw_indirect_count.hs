{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( FN_vkCmdDrawIndexedIndirectCountKHR
  , PFN_vkCmdDrawIndexedIndirectCountKHR
  , vkCmdDrawIndexedIndirectCountKHR
  , FN_vkCmdDrawIndirectCountKHR
  , PFN_vkCmdDrawIndirectCountKHR
  , vkCmdDrawIndirectCountKHR
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
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
-- 'vkCmdDrawIndexedIndirectCountKHR' behaves similarly to
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexedIndirectCountKHR" vkCmdDrawIndexedIndirectCountKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
#else
vkCmdDrawIndexedIndirectCountKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndexedIndirectCountKHR deviceCmds = mkVkCmdDrawIndexedIndirectCountKHR (pVkCmdDrawIndexedIndirectCountKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirectCountKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
#endif

type FN_vkCmdDrawIndexedIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirectCountKHR = FunPtr FN_vkCmdDrawIndexedIndirectCountKHR

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
-- 'vkCmdDrawIndirectCountKHR' behaves similarly to
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirectCountKHR" vkCmdDrawIndirectCountKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
#else
vkCmdDrawIndirectCountKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndirectCountKHR deviceCmds = mkVkCmdDrawIndirectCountKHR (pVkCmdDrawIndirectCountKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectCountKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
#endif

type FN_vkCmdDrawIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectCountKHR = FunPtr FN_vkCmdDrawIndirectCountKHR

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
