{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlagBits
  , ImageAspectFlags
  , SparseImageFormatFlagBits
  , SparseImageFormatFlags
  , SparseMemoryBindFlagBits
  , SparseMemoryBindFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits
  , VkSparseImageFormatFlagBits
  , VkSparseMemoryBindFlagBits
  )


-- No documentation found for TopLevel "ImageAspectFlagBits"
type ImageAspectFlagBits = VkImageAspectFlagBits

-- No documentation found for TopLevel "ImageAspectFlags"
type ImageAspectFlags = ImageAspectFlagBits

-- No documentation found for TopLevel "SparseImageFormatFlagBits"
type SparseImageFormatFlagBits = VkSparseImageFormatFlagBits

-- No documentation found for TopLevel "SparseImageFormatFlags"
type SparseImageFormatFlags = SparseImageFormatFlagBits

-- No documentation found for TopLevel "SparseMemoryBindFlagBits"
type SparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits

-- No documentation found for TopLevel "SparseMemoryBindFlags"
type SparseMemoryBindFlags = SparseMemoryBindFlagBits
