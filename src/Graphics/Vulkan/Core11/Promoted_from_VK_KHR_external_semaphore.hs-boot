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


-- No documentation found for TopLevel "SemaphoreImportFlagBits"
type SemaphoreImportFlagBits = VkSemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlags"
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags
