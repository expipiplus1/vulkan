{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  , FormatFeatureFlagBits
  , FormatFeatureFlags
  , ImageCreateFlagBits
  , ImageCreateFlags
  , ImageTiling
  , ImageType
  , ImageUsageFlagBits
  , ImageUsageFlags
  , InstanceCreateFlags
  , MemoryHeapFlagBits
  , MemoryHeapFlags
  , MemoryPropertyFlagBits
  , MemoryPropertyFlags
  , PhysicalDeviceType
  , QueueFlagBits
  , QueueFlags
  , SampleCountFlagBits
  , SampleCountFlags
  ) where




import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits
  , VkImageCreateFlagBits
  , VkImageTiling
  , VkImageType
  , VkImageUsageFlagBits
  , VkInstanceCreateFlags
  , VkMemoryHeapFlagBits
  , VkMemoryPropertyFlagBits
  , VkPhysicalDeviceType
  , VkQueueFlagBits
  , VkSampleCountFlagBits
  )


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'
type DeviceSize = VkDeviceSize
  

-- | VkFormatFeatureFlagBits - Bitmask specifying features supported by a
-- buffer
--
-- = Description
--
-- The following bits /may/ be set in @linearTilingFeatures@,
-- @optimalTilingFeatures@, and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDrmFormatModifierPropertiesEXT drmFormatModifierTilingFeatures>,
-- specifying that the features are supported by
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImage images>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageView image views>
-- created with the queried
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'::@format@:
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     specifies that an image view /can/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT'
--     specifies that an image view /can/ be used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage images>.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--     specifies that an image view /can/ be used as storage image that
--     supports atomic operations.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     specifies that an image view /can/ be used as a framebuffer color
--     attachment and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
--     specifies that an image view /can/ be used as a framebuffer color
--     attachment that supports blending and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     specifies that an image view /can/ be used as a framebuffer
--     depth\/stencil attachment and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
--     specifies that an image /can/ be used as @srcImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_DST_BIT'
--     specifies that an image /can/ be used as @dstImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--     specifies that if
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     is also set, an image view /can/ be used with a sampler that has
--     either of @magFilter@ or @minFilter@ set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR', or @mipmapMode@
--     set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'. If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
--     is also set, an image can be used as the @srcImage@ to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' with
--     a @filter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR'.
--     This bit /must/ only be exposed for formats that also support the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
--
--     If the format being queried is a depth\/stencil format, this bit
--     only specifies that the depth aspect (not the stencil aspect) of an
--     image of this format supports linear filtering, and that linear
--     filtering of the depth aspect is supported whether depth compare is
--     enabled in the sampler or not. If this bit is not present, linear
--     filtering with depth compare disabled is unsupported and linear
--     filtering with depth compare enabled is supported, but /may/ compute
--     the filtered value in an implementation-dependent manner which
--     differs from the normal rules of linear filtering. The resulting
--     value /must/ be in the range [0,1] and /should/ be proportional to,
--     or a weighted average of, the number of comparison passes or
--     failures.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_FORMAT_FEATURE_TRANSFER_SRC_BIT'
--     specifies that an image /can/ be used as a source image for
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy commands>.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_FORMAT_FEATURE_TRANSFER_DST_BIT'
--     specifies that an image /can/ be used as a destination image for
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy commands>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears clear commands>.
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT'
--     specifies 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' /can/
--     be used as a sampled image with a min or max
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeEXT'.
--     This bit /must/ only be exposed for formats that also support the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--     specifies that 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     /can/ be used with a sampler that has either of @magFilter@ or
--     @minFilter@ set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VK_FILTER_CUBIC_EXT',
--     or be the source image for a blit with @filter@ set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VK_FILTER_CUBIC_EXT'.
--     This bit /must/ only be exposed for formats that also support the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'.
--     If the format being queried is a depth\/stencil format, this only
--     specifies that the depth aspect is cubic filterable.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
--     specifies that an application /can/ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     using this format as a source, and that an image of this format
--     /can/ be used with a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_MIDPOINT'.
--     Otherwise both @xChromaOffset@ and @yChromaOffset@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_COSITED_EVEN'.
--     If a format does not incorporate chroma downsampling (it is not a
--     “422” or “420” format) but the implementation supports sampler
--     Y’CBCR conversion for this format, the implementation /must/ set
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--     specifies that an application /can/ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     using this format as a source, and that an image of this format
--     /can/ be used with a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_COSITED_EVEN'.
--     Otherwise both @xChromaOffset@ and @yChromaOffset@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_CHROMA_LOCATION_MIDPOINT'.
--     If neither
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT'
--     nor
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT'
--     is set, the application /must/ not define a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     using this format as a source.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT'
--     specifies that the format can do linear sampler filtering
--     (min\/magFilter) whilst sampler Y’CBCR conversion is enabled.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT'
--     specifies that the format can have different chroma, min, and mag
--     filters.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
--     specifies that reconstruction is explicit, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-chroma-reconstruction>.
--     If this bit is not present, reconstruction is implicit by default.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'
--     specifies that reconstruction /can/ be forcibly made explicit by
--     setting
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo'::@forceExplicitReconstruction@
--     to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
--
-- -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_DISJOINT_BIT'
--     specifies that a multi-planar image /can/ have the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     set during image creation. An implementation /must/ not set
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_FORMAT_FEATURE_DISJOINT_BIT'
--     for /single-plane formats/.
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--     specifies that an image view /can/ be used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>.
--
-- The following bits /may/ be set in @bufferFeatures@, specifying that the
-- features are supported by
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBuffer buffers>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferView buffer views>
-- created with the queried
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'::@format@:
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
--     specifies that the format /can/ be used to create a buffer view that
--     /can/ be bound to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     descriptor.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
--     specifies that the format /can/ be used to create a buffer view that
--     /can/ be bound to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--     specifies that atomic operations are supported on
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     with this format.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT'
--     specifies that the format /can/ be used as a vertex attribute format
--     ('Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'::@format@).
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags'
type FormatFeatureFlagBits = VkFormatFeatureFlagBits

-- | VkFormatFeatureFlags - Bitmask of VkFormatFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'
type FormatFeatureFlags = FormatFeatureFlagBits

-- | VkImageCreateFlagBits - Bitmask specifying additional parameters of an
-- image
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-physicalfeatures Sparse Physical Device Features>
-- for more details.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags'
type ImageCreateFlagBits = VkImageCreateFlagBits

-- | VkImageCreateFlags - Bitmask of VkImageCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
type ImageCreateFlags = ImageCreateFlagBits

-- | VkImageTiling - Specifies the tiling arrangement of data in an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageTiling = VkImageTiling

-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageType = VkImageType

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags'
type ImageUsageFlagBits = VkImageUsageFlagBits

-- | VkImageUsageFlags - Bitmask of VkImageUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageUsageFlags = ImageUsageFlagBits

-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
type InstanceCreateFlags = VkInstanceCreateFlags

-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlags'
type MemoryHeapFlagBits = VkMemoryHeapFlagBits

-- | VkMemoryHeapFlags - Bitmask of VkMemoryHeapFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlagBits'
type MemoryHeapFlags = MemoryHeapFlagBits

-- | VkMemoryPropertyFlagBits - Bitmask specifying properties for a memory
-- type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlags'
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits

-- | VkMemoryPropertyFlags - Bitmask of VkMemoryPropertyFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlagBits',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryType'
type MemoryPropertyFlags = MemoryPropertyFlagBits

-- | VkPhysicalDeviceType - Supported physical device types
--
-- = Description
--
-- The physical device type is advertised for informational purposes only,
-- and does not directly affect the operation of the system. However, the
-- device type /may/ correlate with other advertised properties or
-- capabilities of the system, such as how many memory heaps there are.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
type PhysicalDeviceType = VkPhysicalDeviceType

-- | VkQueueFlagBits - Bitmask specifying capabilities of queues in a queue
-- family
--
-- = Description
--
-- If an implementation exposes any queue family that supports graphics
-- operations, at least one queue family of at least one physical device
-- exposed by the implementation /must/ support both graphics and compute
-- operations.
--
-- __Note__
--
-- All commands that are allowed on a queue that supports transfer
-- operations are also allowed on a queue that supports either graphics or
-- compute operations. Thus, if the capabilities of a queue family include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_GRAPHICS_BIT' or
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_COMPUTE_BIT',
-- then reporting the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_TRANSFER_BIT'
-- capability separately for that queue family is /optional/.
--
-- For further details see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlags'
type QueueFlagBits = VkQueueFlagBits

-- | VkQueueFlags - Bitmask of VkQueueFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlagBits'
type QueueFlags = QueueFlagBits

-- | VkSampleCountFlagBits - Bitmask specifying sample counts supported for
-- an image used for storage operations
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type SampleCountFlagBits = VkSampleCountFlagBits

-- | VkSampleCountFlags - Bitmask of VkSampleCountFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
type SampleCountFlags = SampleCountFlagBits
