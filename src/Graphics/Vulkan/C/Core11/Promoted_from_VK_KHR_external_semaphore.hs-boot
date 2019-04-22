{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo
  , VkSemaphoreImportFlagBits
  , VkSemaphoreImportFlags
  ) where







data VkExportSemaphoreCreateInfo

data VkSemaphoreImportFlagBits

-- | VkSemaphoreImportFlags - Bitmask of VkSemaphoreImportFlagBits
--
-- = Description
--
-- 'VkSemaphoreImportFlags' is a bitmask type for setting a mask of zero or
-- more 'VkSemaphoreImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'VkSemaphoreImportFlagBits'
type VkSemaphoreImportFlags = VkSemaphoreImportFlagBits
