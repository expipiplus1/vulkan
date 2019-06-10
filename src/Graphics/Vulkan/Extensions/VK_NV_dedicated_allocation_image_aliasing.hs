{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , 
#endif
  pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
  , pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV"
data PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "dedicatedAllocationImageAliasing"
  dedicatedAllocationImageAliasing :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  zero = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV Nothing
                                                                  False

#endif

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME"
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION"
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION :: Integral a => a
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
