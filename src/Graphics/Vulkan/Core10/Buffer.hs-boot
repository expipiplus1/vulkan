{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlagBits
  , BufferCreateFlags
  , BufferUsageFlagBits
  , BufferUsageFlags
  , SharingMode
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits
  , VkBufferUsageFlagBits
  , VkSharingMode
  )


-- No documentation found for TopLevel "BufferCreateFlagBits"
type BufferCreateFlagBits = VkBufferCreateFlagBits

-- No documentation found for TopLevel "BufferCreateFlags"
type BufferCreateFlags = BufferCreateFlagBits

-- No documentation found for TopLevel "BufferUsageFlagBits"
type BufferUsageFlagBits = VkBufferUsageFlagBits

-- No documentation found for TopLevel "BufferUsageFlags"
type BufferUsageFlags = BufferUsageFlagBits

-- No documentation found for TopLevel "SharingMode"
type SharingMode = VkSharingMode
