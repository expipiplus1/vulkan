{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( ComponentTypeNV
  , ScopeNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV
  , VkScopeNV
  )


-- | VkComponentTypeNV - Specify SPIR-V cooperative matrix component type
--
-- = See Also
--
-- No cross-references are available
type ComponentTypeNV = VkComponentTypeNV

-- | VkScopeNV - Specify SPIR-V scope
--
-- = Description
--
-- All enum values match the corresponding SPIR-V value.
--
-- = See Also
--
-- No cross-references are available
type ScopeNV = VkScopeNV
