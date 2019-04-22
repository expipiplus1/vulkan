{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorPool
  , DescriptorPoolCreateFlagBits
  , DescriptorPoolCreateFlags
  , DescriptorPoolResetFlags
  , DescriptorSet
  , DescriptorSetLayoutCreateFlagBits
  , DescriptorSetLayoutCreateFlags
  , DescriptorType
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits
  , VkDescriptorPoolResetFlags
  , VkDescriptorSetLayoutCreateFlagBits
  , VkDescriptorType
  , VkDescriptorPool
  , VkDescriptorSet
  )


-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool'
type DescriptorPool = VkDescriptorPool

-- | VkDescriptorPoolCreateFlagBits - Bitmask specifying certain supported
-- operations on a descriptor pool
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlags'
type DescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits

-- | VkDescriptorPoolCreateFlags - Bitmask of VkDescriptorPoolCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo'
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits

-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolResetFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool'
type DescriptorPoolResetFlags = VkDescriptorPoolResetFlags

-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
type DescriptorSet = VkDescriptorSet

-- | VkDescriptorSetLayoutCreateFlagBits - Bitmask specifying descriptor set
-- layout properties
--
-- = Description
--
-- __Note__
--
-- All bits for this type are defined by extensions, and none of those
-- extensions are enabled in this build of the specification.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlags'
type DescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits

-- | VkDescriptorSetLayoutCreateFlags - Bitmask of
-- VkDescriptorSetLayoutCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits

-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampler sampler descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler combined image sampler descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inputattachment input attachment descriptor>.
--
-- When a descriptor set is updated via elements of
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet', members
-- of @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only accessed
-- by the implementation when they correspond to descriptor type being
-- defined - otherwise they are ignored. The members accessed are as
-- follows for each descriptor type:
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
--     only the @sampler@ member of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     is accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     only the @imageView@ and @imageLayout@ members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     all members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     all members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pBufferInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pTexelBufferView@
--     is accessed.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolSize',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateEntry',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.VkImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
type DescriptorType = VkDescriptorType
