{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorBindingFlagBitsEXT
  , DescriptorBindingFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT
  )


-- | VkDescriptorBindingFlagBitsEXT - Bitmask specifying descriptor set
-- layout binding properties
--
-- = Description
--
-- __Note__
--
-- Note that while
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- both involve updates to descriptor sets after they are bound,
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- is a weaker requirement since it is only about descriptors that are not
-- used, whereas
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- requires the implementation to observe updates to descriptors that are
-- used.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT'
type DescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT

-- | VkDescriptorBindingFlagsEXT - Bitmask of VkDescriptorBindingFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT'
type DescriptorBindingFlagsEXT = DescriptorBindingFlagBitsEXT
