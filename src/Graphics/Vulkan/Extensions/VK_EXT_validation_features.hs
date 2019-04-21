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
  , withCStructValidationFeaturesEXT
  , fromCStructValidationFeaturesEXT
  , ValidationFeaturesEXT(..)
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT(..)
  , VkValidationFeatureEnableEXT(..)
  , VkValidationFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
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
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  )


-- | VkValidationFeatureDisableEXT - Specify validation features to disable
--
-- = See Also
--
-- No cross-references are available
type ValidationFeatureDisableEXT = VkValidationFeatureDisableEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_ALL_EXT'
-- specifies that all validation checks are disabled.
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT = VK_VALIDATION_FEATURE_DISABLE_ALL_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT'
-- specifies that shader validation is disabled. This feature is enabled by
-- default.
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT = VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT'
-- specifies that thread safety validation is disabled. This feature is
-- enabled by default.
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT'
-- specifies that stateless parameter validation is disabled. This feature
-- is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT'
-- specifies that object lifetime validation is disabled. This feature is
-- enabled by default.
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT'
-- specifies that core validation checks are disabled. This feature is
-- enabled by default. If this feature is disabled, the shader validation
-- and GPU-assisted validation features are also disabled.
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT'
-- specifies that protection against duplicate non-dispatchable object
-- handles is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: (a ~ ValidationFeatureDisableEXT) => a
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT

-- | VkValidationFeatureEnableEXT - Specify validation features to enable
--
-- = See Also
--
-- No cross-references are available
type ValidationFeatureEnableEXT = VkValidationFeatureEnableEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT'
-- specifies that GPU-assisted validation is enabled. Activating this
-- feature instruments shader programs to generate additional diagnostic
-- data. This feature is disabled by default.
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT :: (a ~ ValidationFeatureEnableEXT) => a
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT'
-- specifies that the validation layers reserve a descriptor set binding
-- slot for their own use. The layer reports a value for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxBoundDescriptorSets@
-- that is one less than the value reported by the device. If the device
-- supports the binding of only one descriptor set, the validation layer
-- does not perform GPU-assisted validation. This feature is disabled by
-- default. The GPU-assisted validation feature must be enabled in order to
-- use this feature.
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT :: (a ~ ValidationFeatureEnableEXT) => a
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT


-- | VkValidationFeaturesEXT - Specify validation features to enable or
-- disable for a Vulkan instance
--
-- = Description
--
-- Unresolved directive in VkValidationFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkValidationFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ValidationFeaturesEXT = ValidationFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ValidationFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pEnabledValidationFeatures"
  enabledValidationFeatures :: Vector ValidationFeatureEnableEXT
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFeaturesEXT" "pDisabledValidationFeatures"
  disabledValidationFeatures :: Vector ValidationFeatureDisableEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkValidationFeaturesEXT' and
-- marshal a 'ValidationFeaturesEXT' into it. The 'VkValidationFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructValidationFeaturesEXT :: ValidationFeaturesEXT -> (VkValidationFeaturesEXT -> IO a) -> IO a
withCStructValidationFeaturesEXT marshalled cont = withVec (&) (disabledValidationFeatures (marshalled :: ValidationFeaturesEXT)) (\pPDisabledValidationFeatures -> withVec (&) (enabledValidationFeatures (marshalled :: ValidationFeaturesEXT)) (\pPEnabledValidationFeatures -> maybeWith withSomeVkStruct (next (marshalled :: ValidationFeaturesEXT)) (\pPNext -> cont (VkValidationFeaturesEXT VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT pPNext (fromIntegral (Data.Vector.length (enabledValidationFeatures (marshalled :: ValidationFeaturesEXT)))) pPEnabledValidationFeatures (fromIntegral (Data.Vector.length (disabledValidationFeatures (marshalled :: ValidationFeaturesEXT)))) pPDisabledValidationFeatures))))

-- | A function to read a 'VkValidationFeaturesEXT' and all additional
-- structures in the pointer chain into a 'ValidationFeaturesEXT'.
fromCStructValidationFeaturesEXT :: VkValidationFeaturesEXT -> IO ValidationFeaturesEXT
fromCStructValidationFeaturesEXT c = ValidationFeaturesEXT <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationFeaturesEXT)))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkEnabledValidationFeatureCount (c :: VkValidationFeaturesEXT))) (peekElemOff (vkPEnabledValidationFeatures (c :: VkValidationFeaturesEXT))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkDisabledValidationFeatureCount (c :: VkValidationFeaturesEXT))) (peekElemOff (vkPDisabledValidationFeatures (c :: VkValidationFeaturesEXT))))

instance Zero ValidationFeaturesEXT where
  zero = ValidationFeaturesEXT Nothing
                               Data.Vector.empty
                               Data.Vector.empty

