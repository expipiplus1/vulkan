{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , CompositeAlphaFlagBitsKHR
  , CompositeAlphaFlagsKHR
  , PresentModeKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR
  , VkCompositeAlphaFlagBitsKHR
  , VkPresentModeKHR
  , VkSurfaceKHR
  , VkSurfaceTransformFlagBitsKHR
  )


-- No documentation found for TopLevel "ColorSpaceKHR"
type ColorSpaceKHR = VkColorSpaceKHR

-- No documentation found for TopLevel "CompositeAlphaFlagBitsKHR"
type CompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR

-- No documentation found for TopLevel "CompositeAlphaFlagsKHR"
type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- No documentation found for TopLevel "PresentModeKHR"
type PresentModeKHR = VkPresentModeKHR

-- No documentation found for TopLevel "SurfaceKHR"
type SurfaceKHR = VkSurfaceKHR

-- No documentation found for TopLevel "SurfaceTransformFlagBitsKHR"
type SurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR

-- No documentation found for TopLevel "SurfaceTransformFlagsKHR"
type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR
