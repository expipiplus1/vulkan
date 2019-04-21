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
-- Implementations /must/ not report
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT'
-- for buffers with external handle type
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'.
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
-- > | 'Graphics.Vulkan.C.E | No restriction       | No restriction        |
-- > | xtensions.VK_EXT_ext |                      |                       |
-- > | ernal_memory_host.VK |                      |                       |
-- > | _EXTERNAL_MEMORY_HAN |                      |                       |
-- > | DLE_TYPE_HOST_ALLOCA |                      |                       |
-- > | TION_BIT_EXT'        |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.E | No restriction       | No restriction        |
-- > | xtensions.VK_EXT_ext |                      |                       |
-- > | ernal_memory_host.VK |                      |                       |
-- > | _EXTERNAL_MEMORY_HAN |                      |                       |
-- > | DLE_TYPE_HOST_MAPPED |                      |                       |
-- > | _FOREIGN_MEMORY_BIT_ |                      |                       |
-- > | EXT'                 |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.E | No restriction       | No restriction        |
-- > | xtensions.VK_EXT_ext |                      |                       |
-- > | ernal_memory_dma_buf |                      |                       |
-- > | .VK_EXTERNAL_MEMORY_ |                      |                       |
-- > | HANDLE_TYPE_DMA_BUF_ |                      |                       |
-- > | BIT_EXT'             |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.E | No restriction       | No restriction        |
-- > | xtensions.VK_ANDROID |                      |                       |
-- > | _external_memory_and |                      |                       |
-- > | roid_hardware_buffer |                      |                       |
-- > | .VK_EXTERNAL_MEMORY_ |                      |                       |
-- > | HANDLE_TYPE_ANDROID_ |                      |                       |
-- > | HARDWARE_BUFFER_BIT_ |                      |                       |
-- > | ANDROID'             |                      |                       |
-- > +----------------------+----------------------+-----------------------+
-- >
-- > External memory handle types compatibility
--
-- __Note__
--
-- The above table does not restrict the drivers and devices with which
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VK_EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
-- /may/ be shared, as these handle types inherently mean memory that does
-- not come from the same device, as they import memory from the host or a
-- foreign device, respectively.
--
-- __Note__
--
-- Even though the above table does not restrict the drivers and devices
-- with which
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_dma_buf.VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT'
-- /may/ be shared, query mechanisms exist in the Vulkan API that prevent
-- the import of incompatible dma-bufs (such as
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR')
-- and that prevent incompatible usage of dma-bufs (such as
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfoKHR'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfoKHR').
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo'
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
