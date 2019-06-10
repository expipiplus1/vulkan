{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , 
#endif
  pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT"
data PhysicalDeviceYcbcrImageArraysFeaturesEXT = PhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "ycbcrImageArrays"
  ycbcrImageArrays :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  zero = PhysicalDeviceYcbcrImageArraysFeaturesEXT Nothing
                                                   False

#endif

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: Integral a => a
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
