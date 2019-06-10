{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryFeatureFlagBitsNV
  , ExternalMemoryFeatureFlagsNV
  , ExternalMemoryHandleTypeFlagBitsNV
  , ExternalMemoryHandleTypeFlagsNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryFeatureFlagBitsNV
  , VkExternalMemoryHandleTypeFlagBitsNV
  )


-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsNV"
type ExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsNV"
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsNV"
type ExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsNV"
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV
