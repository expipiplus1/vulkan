{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeatureDisableEXT
  , ValidationFeatureEnableEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT
  , VkValidationFeatureEnableEXT
  )


-- | VkValidationFeatureDisableEXT - Specify validation features to disable
--
-- = See Also
--
-- No cross-references are available
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT

-- | VkValidationFeatureEnableEXT - Specify validation features to enable
--
-- = See Also
--
-- No cross-references are available
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT
