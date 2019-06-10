{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeatureDisableEXT
  , pattern VALIDATION_FEATURE_DISABLE_ALL_EXT
  , pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT
  , pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
  , pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
  , pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
  , pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
  , pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
  , ValidationFeatureEnableEXT
  , pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
  , pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , ValidationFeaturesEXT(..)
#endif
  , pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern EXT_VALIDATION_FEATURES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT(..)
  , VkValidationFeatureEnableEXT(..)
  , pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  , pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
  , pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
  , pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  )


-- No documentation found for TopLevel "ValidationFeatureDisableEXT"
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT


{-# complete VALIDATION_FEATURE_DISABLE_ALL_EXT, VALIDATION_FEATURE_DISABLE_SHADERS_EXT, VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT, VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT, VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT, VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT, VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: ValidationFeatureDisableEXT #-}


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_ALL_EXT"
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT = VK_VALIDATION_FEATURE_DISABLE_ALL_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_SHADERS_EXT"
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT = VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT"
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT"
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT"
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT"
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT


-- No documentation found for Nested "ValidationFeatureDisableEXT" "VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT"
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT

-- No documentation found for TopLevel "ValidationFeatureEnableEXT"
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT


{-# complete VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT, VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT :: ValidationFeatureEnableEXT #-}


-- No documentation found for Nested "ValidationFeatureEnableEXT" "VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT"
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT :: (a ~ ValidationFeatureEnableEXT) => a
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT


-- No documentation found for Nested "ValidationFeatureEnableEXT" "VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT"
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT :: (a ~ ValidationFeatureEnableEXT) => a
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkValidationFeaturesEXT"
data ValidationFeaturesEXT = ValidationFeaturesEXT
  { -- No documentation found for Nested "ValidationFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pEnabledValidationFeatures"
  enabledValidationFeatures :: Vector ValidationFeatureEnableEXT
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pDisabledValidationFeatures"
  disabledValidationFeatures :: Vector ValidationFeatureDisableEXT
  }
  deriving (Show, Eq)

instance Zero ValidationFeaturesEXT where
  zero = ValidationFeaturesEXT Nothing
                               mempty
                               mempty

#endif

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME"
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME = VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_SPEC_VERSION"
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION :: Integral a => a
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION = VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
