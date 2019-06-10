{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  , MemoryMapFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  , VkMemoryMapFlags
  )


-- No documentation found for TopLevel "DeviceMemory"
type DeviceMemory = VkDeviceMemory

-- No documentation found for TopLevel "MemoryMapFlags"
type MemoryMapFlags = VkMemoryMapFlags
