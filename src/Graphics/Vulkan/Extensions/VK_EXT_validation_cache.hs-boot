{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ValidationCacheCreateFlagsEXT
  , ValidationCacheEXT
  , ValidationCacheHeaderVersionEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateFlagsEXT
  , VkValidationCacheHeaderVersionEXT
  , VkValidationCacheEXT
  )


-- | VkValidationCacheCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateInfoEXT'
type ValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT

-- | VkValidationCacheEXT - Opaque handle to a validation cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkDestroyValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkMergeValidationCachesEXT'
type ValidationCacheEXT = VkValidationCacheEXT

-- | VkValidationCacheHeaderVersionEXT - Encode validation cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT'
type ValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT
