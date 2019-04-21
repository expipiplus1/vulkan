{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlagBits
  , SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlags
  , SemaphoreImportFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlagBits
  )


-- | VkSemaphoreImportFlagBits - Bitmask specifying additional parameters of
-- semaphore payload import
--
-- = Description
--
-- These bits have the following meanings:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags'
type SemaphoreImportFlagBits = VkSemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits

-- | VkSemaphoreImportFlags - Bitmask of VkSemaphoreImportFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags
