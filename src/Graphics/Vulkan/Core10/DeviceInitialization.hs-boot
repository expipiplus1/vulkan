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


-- No documentation found for TopLevel "DeviceSize"
type DeviceSize = VkDeviceSize
  

-- No documentation found for TopLevel "FormatFeatureFlagBits"
type FormatFeatureFlagBits = VkFormatFeatureFlagBits

-- No documentation found for TopLevel "FormatFeatureFlags"
type FormatFeatureFlags = FormatFeatureFlagBits

-- No documentation found for TopLevel "ImageCreateFlagBits"
type ImageCreateFlagBits = VkImageCreateFlagBits

-- No documentation found for TopLevel "ImageCreateFlags"
type ImageCreateFlags = ImageCreateFlagBits

-- No documentation found for TopLevel "ImageTiling"
type ImageTiling = VkImageTiling

-- No documentation found for TopLevel "ImageType"
type ImageType = VkImageType

-- No documentation found for TopLevel "ImageUsageFlagBits"
type ImageUsageFlagBits = VkImageUsageFlagBits

-- No documentation found for TopLevel "ImageUsageFlags"
type ImageUsageFlags = ImageUsageFlagBits

-- No documentation found for TopLevel "InstanceCreateFlags"
type InstanceCreateFlags = VkInstanceCreateFlags

-- No documentation found for TopLevel "MemoryHeapFlagBits"
type MemoryHeapFlagBits = VkMemoryHeapFlagBits

-- No documentation found for TopLevel "MemoryHeapFlags"
type MemoryHeapFlags = MemoryHeapFlagBits

-- No documentation found for TopLevel "MemoryPropertyFlagBits"
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits

-- No documentation found for TopLevel "MemoryPropertyFlags"
type MemoryPropertyFlags = MemoryPropertyFlagBits

-- No documentation found for TopLevel "PhysicalDeviceType"
type PhysicalDeviceType = VkPhysicalDeviceType

-- No documentation found for TopLevel "QueueFlagBits"
type QueueFlagBits = VkQueueFlagBits

-- No documentation found for TopLevel "QueueFlags"
type QueueFlags = QueueFlagBits

-- No documentation found for TopLevel "SampleCountFlagBits"
type SampleCountFlagBits = VkSampleCountFlagBits

-- No documentation found for TopLevel "SampleCountFlags"
type SampleCountFlags = SampleCountFlagBits
