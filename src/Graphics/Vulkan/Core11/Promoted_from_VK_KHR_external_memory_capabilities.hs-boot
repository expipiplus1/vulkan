{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryFeatureFlagBits
  , ExternalMemoryFeatureFlagBitsKHR
  , ExternalMemoryFeatureFlags
  , ExternalMemoryFeatureFlagsKHR
  , ExternalMemoryHandleTypeFlagBits
  , ExternalMemoryHandleTypeFlagBitsKHR
  , ExternalMemoryHandleTypeFlags
  , ExternalMemoryHandleTypeFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryFeatureFlagBits
  , VkExternalMemoryHandleTypeFlagBits
  )


-- | VkExternalMemoryFeatureFlagBits - Bitmask specifying features of an
-- external memory handle type
--
-- = Description
--
-- Because their semantics in external APIs roughly align with that of an
-- image or buffer with a dedicated allocation in Vulkan, implementations
-- are /required/ to report
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- for the following external handle types:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlags'
type ExternalMemoryFeatureFlagBits = VkExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagBitsKHR"
type ExternalMemoryFeatureFlagBitsKHR = ExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlags - Bitmask of
-- VkExternalMemoryFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties'
type ExternalMemoryFeatureFlags = ExternalMemoryFeatureFlagBits

-- No documentation found for TopLevel "ExternalMemoryFeatureFlagsKHR"
type ExternalMemoryFeatureFlagsKHR = ExternalMemoryFeatureFlags

-- | VkExternalMemoryHandleTypeFlagBits - Bit specifying external memory
-- handle types
--
-- = Description
--
-- Some external memory handle types can only be shared within the same
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
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_FD_BIT |                      |                       |
-- > | '                    |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_WIN32_ |                      |                       |
-- > | BIT'                 |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_OPAQUE_WIN32_ |                      |                       |
-- > | KMT_BIT'             |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D11_TEXTURE |                      |                       |
-- > | _BIT'                |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D11_TEXTURE |                      |                       |
-- > | _KMT_BIT'            |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D12_HEAP_BI |                      |                       |
-- > | T'                   |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.C | Must match           | Must match            |
-- > | ore11.Promoted_from_ |                      |                       |
-- > | VK_KHR_external_memo |                      |                       |
-- > | ry_capabilities.VK_E |                      |                       |
-- > | XTERNAL_MEMORY_HANDL |                      |                       |
-- > | E_TYPE_D3D12_RESOURC |                      |                       |
-- > | E_BIT'               |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External memory handle types compatibility
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'
type ExternalMemoryHandleTypeFlagBits = VkExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagBitsKHR"
type ExternalMemoryHandleTypeFlagBitsKHR = ExternalMemoryHandleTypeFlagBits

-- | VkExternalMemoryHandleTypeFlags - Bitmask of
-- VkExternalMemoryHandleTypeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryProperties'
type ExternalMemoryHandleTypeFlags = ExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "ExternalMemoryHandleTypeFlagsKHR"
type ExternalMemoryHandleTypeFlagsKHR = ExternalMemoryHandleTypeFlags
