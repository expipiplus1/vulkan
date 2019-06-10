{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for TopLevel "Buffer"
type Buffer = VkBuffer

-- No documentation found for TopLevel "Image"
type Image = VkImage
