{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceCornerSampledImageFeaturesNV(..)
  , 
#endif
  pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  , pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( pattern VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceCornerSampledImageFeaturesNV"
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "cornerSampledImage"
  cornerSampledImage :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceCornerSampledImageFeaturesNV where
  zero = PhysicalDeviceCornerSampledImageFeaturesNV Nothing
                                                    False

#endif

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME"
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION"
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION :: Integral a => a
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
