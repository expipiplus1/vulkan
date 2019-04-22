{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( TimeDomainEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkTimeDomainEXT
  )


-- | VkTimeDomainEXT - Supported time domains
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VkCalibratedTimestampInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.vkGetPhysicalDeviceCalibrateableTimeDomainsEXT'
type TimeDomainEXT = VkTimeDomainEXT
