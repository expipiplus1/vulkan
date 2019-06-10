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
  ( VkDescriptorPool
  , VkDescriptorPoolCreateFlagBits
  , VkDescriptorPoolResetFlags
  , VkDescriptorSet
  , VkDescriptorSetLayoutCreateFlagBits
  , VkDescriptorType
  )


-- No documentation found for TopLevel "DescriptorPool"
type DescriptorPool = VkDescriptorPool

-- No documentation found for TopLevel "DescriptorPoolCreateFlagBits"
type DescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "DescriptorPoolCreateFlags"
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "DescriptorPoolResetFlags"
type DescriptorPoolResetFlags = VkDescriptorPoolResetFlags

-- No documentation found for TopLevel "DescriptorSet"
type DescriptorSet = VkDescriptorSet

-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlagBits"
type DescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlags"
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "DescriptorType"
type DescriptorType = VkDescriptorType
