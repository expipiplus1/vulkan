{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( PointClippingBehavior
  , PointClippingBehaviorKHR
  , TessellationDomainOrigin
  , TessellationDomainOriginKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkPointClippingBehavior
  , VkTessellationDomainOrigin
  )


-- | VkPointClippingBehavior - Enum specifying the point clipping behavior
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'
type PointClippingBehavior = VkPointClippingBehavior

-- No documentation found for TopLevel "PointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior

-- | VkTessellationDomainOrigin - Enum describing tessellation domain origin
--
-- = Description
--
-- This enum affects how the @VertexOrderCw@ and @VertexOrderCcw@
-- tessellation execution modes are interpreted, since the winding is
-- defined relative to the orientation of the domain.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo'
type TessellationDomainOrigin = VkTessellationDomainOrigin

-- No documentation found for TopLevel "TessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin
