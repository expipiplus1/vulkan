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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VkValidationFeaturesEXT'
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT

-- | VkValidationFeatureEnableEXT - Specify validation features to enable
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VkValidationFeaturesEXT'
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT
