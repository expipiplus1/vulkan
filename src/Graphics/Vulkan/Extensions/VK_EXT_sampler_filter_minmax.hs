{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , 
  SamplerReductionModeCreateInfoEXT(..)
#endif
  , SamplerReductionModeEXT
  , pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  , pattern SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkSamplerReductionModeEXT(..)
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  , pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
  , pattern VK_SAMPLER_REDUCTION_MODE_MAX_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_MIN_EXT
  , pattern VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT"
data PhysicalDeviceSamplerFilterMinmaxPropertiesEXT = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxSingleComponentFormats"
  filterMinmaxSingleComponentFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSamplerFilterMinmaxPropertiesEXT" "filterMinmaxImageComponentMapping"
  filterMinmaxImageComponentMapping :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  zero = PhysicalDeviceSamplerFilterMinmaxPropertiesEXT Nothing
                                                        False
                                                        False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSamplerReductionModeCreateInfoEXT"
data SamplerReductionModeCreateInfoEXT = SamplerReductionModeCreateInfoEXT
  { -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SamplerReductionModeCreateInfoEXT" "reductionMode"
  reductionMode :: SamplerReductionModeEXT
  }
  deriving (Show, Eq)

instance Zero SamplerReductionModeCreateInfoEXT where
  zero = SamplerReductionModeCreateInfoEXT Nothing
                                           zero

#endif

-- No documentation found for TopLevel "SamplerReductionModeEXT"
type SamplerReductionModeEXT = VkSamplerReductionModeEXT


{-# complete SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT, SAMPLER_REDUCTION_MODE_MIN_EXT, SAMPLER_REDUCTION_MODE_MAX_EXT :: SamplerReductionModeEXT #-}


-- No documentation found for Nested "SamplerReductionModeEXT" "SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT


-- No documentation found for Nested "SamplerReductionModeEXT" "SAMPLER_REDUCTION_MODE_MIN_EXT"
pattern SAMPLER_REDUCTION_MODE_MIN_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_MIN_EXT = VK_SAMPLER_REDUCTION_MODE_MIN_EXT


-- No documentation found for Nested "SamplerReductionModeEXT" "SAMPLER_REDUCTION_MODE_MAX_EXT"
pattern SAMPLER_REDUCTION_MODE_MAX_EXT :: (a ~ SamplerReductionModeEXT) => a
pattern SAMPLER_REDUCTION_MODE_MAX_EXT = VK_SAMPLER_REDUCTION_MODE_MAX_EXT

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME"
pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION"
pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION :: Integral a => a
pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
