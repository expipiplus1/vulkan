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


-- No documentation found for TopLevel "FenceImportFlagBits"
type FenceImportFlagBits = VkFenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlags"
type FenceImportFlags = FenceImportFlagBits

-- No documentation found for TopLevel "FenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags
