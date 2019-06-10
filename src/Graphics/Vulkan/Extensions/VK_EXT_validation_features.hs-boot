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


-- No documentation found for TopLevel "ValidationFeatureDisableEXT"
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT

-- No documentation found for TopLevel "ValidationFeatureEnableEXT"
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT
