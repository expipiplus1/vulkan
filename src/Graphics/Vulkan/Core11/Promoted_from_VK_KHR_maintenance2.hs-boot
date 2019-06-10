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


-- No documentation found for TopLevel "PointClippingBehavior"
type PointClippingBehavior = VkPointClippingBehavior

-- No documentation found for TopLevel "PointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior

-- No documentation found for TopLevel "TessellationDomainOrigin"
type TessellationDomainOrigin = VkTessellationDomainOrigin

-- No documentation found for TopLevel "TessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin
