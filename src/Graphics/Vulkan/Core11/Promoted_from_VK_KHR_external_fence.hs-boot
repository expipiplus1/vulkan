{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlagBits
  , FenceImportFlagBitsKHR
  , FenceImportFlags
  , FenceImportFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlagBits
  )


-- | VkFenceImportFlagBits - Bitmask specifying additional parameters of
-- fence payload import
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags'
type FenceImportFlagBits = VkFenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits

-- | VkFenceImportFlags - Bitmask of VkFenceImportFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'
type FenceImportFlags = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags
