{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( MemoryOverallocationBehaviorAMD
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkMemoryOverallocationBehaviorAMD
  )


-- | VkMemoryOverallocationBehaviorAMD - Specify memory overallocation
-- behavior
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior.VkDeviceMemoryOverallocationCreateInfoAMD'
type MemoryOverallocationBehaviorAMD = VkMemoryOverallocationBehaviorAMD
