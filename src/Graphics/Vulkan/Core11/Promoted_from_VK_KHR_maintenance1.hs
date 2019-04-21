{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , CommandPoolTrimFlagsKHR
  , trimCommandPool
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where




import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , vkTrimCommandPool
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )


-- | VkCommandPoolTrimFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VkCommandPoolTrimFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool'
type CommandPoolTrimFlags = VkCommandPoolTrimFlags

-- No documentation found for TopLevel "CommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags


-- | vkTrimCommandPool - Trim a command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool to trim.
--
-- -   @flags@ is reserved for future use.
--
-- = Description
--
-- Trimming a command pool recycles unused memory from the command pool
-- back to the system. Command buffers allocated from the pool are not
-- affected by the command.
--
-- __Note__
--
-- This command provides applications with some control over the internal
-- memory allocations used by command pools.
--
-- Unused memory normally arises from command buffers that have been
-- recorded and later reset, such that they are no longer using the memory.
-- On reset, a command buffer can return memory to its command pool, but
-- the only way to release memory from a command pool to the system
-- requires calling
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool', which cannot
-- be executed while any command buffers from that pool are still in use.
-- Subsequent recording operations into command buffers will re-use this
-- memory but since total memory requirements fluctuate over time, unused
-- memory can accumulate.
--
-- In this situation, trimming a command pool /may/ be useful to return
-- unused memory back to the system, returning the total outstanding memory
-- allocated by the pool back to a more “average” value.
--
-- Implementations utilize many internal allocation strategies that make it
-- impossible to guarantee that all unused memory is released back to the
-- system. For instance, an implementation of a command pool /may/ involve
-- allocating memory in bulk from the system and sub-allocating from that
-- memory. In such an implementation any live command buffer that holds a
-- reference to a bulk allocation would prevent that allocation from being
-- freed, even if only a small proportion of the bulk allocation is in use.
--
-- In most cases trimming will result in a reduction in allocated but
-- unused memory, but it does not guarantee the “ideal” behavior.
--
-- Trimming /may/ be an expensive operation, and /should/ not be called
-- frequently. Trimming /should/ be treated as a way to relieve memory
-- pressure after application-known points when there exists enough unused
-- memory that the cost of trimming is “worth” it.
--
-- Unresolved directive in vkTrimCommandPool.txt -
-- include::{generated}\/validity\/protos\/vkTrimCommandPool.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VkCommandPoolTrimFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
trimCommandPool :: Device ->  CommandPool ->  CommandPoolTrimFlags ->  IO ()
trimCommandPool = \(Device device' commandTable) -> \commandPool' -> \flags' -> vkTrimCommandPool commandTable device' commandPool' flags' *> (pure ())
