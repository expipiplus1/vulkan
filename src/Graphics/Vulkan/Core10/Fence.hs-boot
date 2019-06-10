{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Fence
  ( FenceCreateFlagBits
  , FenceCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits
  )


-- No documentation found for TopLevel "FenceCreateFlagBits"
type FenceCreateFlagBits = VkFenceCreateFlagBits

-- No documentation found for TopLevel "FenceCreateFlags"
type FenceCreateFlags = FenceCreateFlagBits
