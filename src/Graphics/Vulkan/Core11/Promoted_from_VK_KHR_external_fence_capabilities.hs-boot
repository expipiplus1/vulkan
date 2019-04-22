{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceFeatureFlagBits
  , ExternalFenceFeatureFlagBitsKHR
  , ExternalFenceFeatureFlags
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceHandleTypeFlagBits
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceHandleTypeFlags
  , ExternalFenceHandleTypeFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits
  , VkExternalFenceHandleTypeFlagBits
  )


-- | VkExternalFenceFeatureFlagBits - Bitfield describing features of an
-- external fence handle type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlags'
type ExternalFenceFeatureFlagBits = VkExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "ExternalFenceFeatureFlagBitsKHR"
type ExternalFenceFeatureFlagBitsKHR = ExternalFenceFeatureFlagBits

-- | VkExternalFenceFeatureFlags - Bitmask of VkExternalFenceFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'
type ExternalFenceFeatureFlags = ExternalFenceFeatureFlagBits

-- No documentation found for TopLevel "ExternalFenceFeatureFlagsKHR"
type ExternalFenceFeatureFlagsKHR = ExternalFenceFeatureFlags

-- | VkExternalFenceHandleTypeFlagBits - Bitmask of valid external fence
-- handle types
--
-- = Description
--
-- Some external fence handle types can only be shared within the same
-- underlying physical device and\/or the same driver version, as defined
-- in the following table:
--
-- > +----------------------+----------------------+-----------------------+
-- > | Handle type          | 'Graphics.Vulkan.C.C | 'Graphics.Vulkan.C.Co |
-- > |                      | ore11.Promoted_from_ | re11.Promoted_from_VK |
-- > |                      | VK_KHR_external_memo | _KHR_external_memory_ |
-- > |                      | ry_capabilities.VkPh | capabilities.VkPhysic |
-- > |                      | ysicalDeviceIDProper | alDeviceIDProperties' |
-- > |                      | ties'::@driverUUID@  | ::@deviceUUID@        |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_FD_BIT'  |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_WIN32_BI |                      |                       |
-- > | T'                   |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_OPAQUE_WIN32_KM |                      |                       |
-- > | T_BIT'               |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | No restriction       | No restriction        |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_fenc |                      |                       |
-- > | e_capabilities.VK_EX |                      |                       |
-- > | TERNAL_FENCE_HANDLE_ |                      |                       |
-- > | TYPE_SYNC_FD_BIT'    |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External fence handle types compatibility
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo'
type ExternalFenceHandleTypeFlagBits = VkExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagBitsKHR"
type ExternalFenceHandleTypeFlagBitsKHR = ExternalFenceHandleTypeFlagBits

-- | VkExternalFenceHandleTypeFlags - Bitmask of
-- VkExternalFenceHandleTypeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'
type ExternalFenceHandleTypeFlags = ExternalFenceHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalFenceHandleTypeFlagsKHR"
type ExternalFenceHandleTypeFlagsKHR = ExternalFenceHandleTypeFlags
