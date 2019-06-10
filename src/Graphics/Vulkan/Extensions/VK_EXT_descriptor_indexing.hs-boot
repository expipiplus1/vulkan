{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorBindingFlagBitsEXT
  , DescriptorBindingFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT
  )


-- No documentation found for TopLevel "DescriptorBindingFlagBitsEXT"
type DescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT

-- No documentation found for TopLevel "DescriptorBindingFlagsEXT"
type DescriptorBindingFlagsEXT = DescriptorBindingFlagBitsEXT
