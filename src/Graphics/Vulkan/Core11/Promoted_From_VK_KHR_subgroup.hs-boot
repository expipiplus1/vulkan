{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( SubgroupFeatureFlagBits
  , SubgroupFeatureFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkSubgroupFeatureFlagBits
  )


-- | VkSubgroupFeatureFlagBits - Enum describing what subgroup operations are
-- supported
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlags'
type SubgroupFeatureFlagBits = VkSubgroupFeatureFlagBits

-- | VkSubgroupFeatureFlags - Bitmask of VkSubgroupFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlagBits'
type SubgroupFeatureFlags = SubgroupFeatureFlagBits
