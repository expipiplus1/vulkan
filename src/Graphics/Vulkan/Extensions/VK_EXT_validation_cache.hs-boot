{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ValidationCacheCreateFlagsEXT
  , ValidationCacheEXT
  , ValidationCacheHeaderVersionEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateFlagsEXT
  , VkValidationCacheEXT
  , VkValidationCacheHeaderVersionEXT
  )


-- No documentation found for TopLevel "ValidationCacheCreateFlagsEXT"
type ValidationCacheCreateFlagsEXT = VkValidationCacheCreateFlagsEXT

-- No documentation found for TopLevel "ValidationCacheEXT"
type ValidationCacheEXT = VkValidationCacheEXT

-- No documentation found for TopLevel "ValidationCacheHeaderVersionEXT"
type ValidationCacheHeaderVersionEXT = VkValidationCacheHeaderVersionEXT
