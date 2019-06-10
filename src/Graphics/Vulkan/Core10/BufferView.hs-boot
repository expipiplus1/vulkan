{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.BufferView
  ( BufferView
  , BufferViewCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferView
  , VkBufferViewCreateFlags
  )


-- No documentation found for TopLevel "BufferView"
type BufferView = VkBufferView

-- No documentation found for TopLevel "BufferViewCreateFlags"
type BufferViewCreateFlags = VkBufferViewCreateFlags
