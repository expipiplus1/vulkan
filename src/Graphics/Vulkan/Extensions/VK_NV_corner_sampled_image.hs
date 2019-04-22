{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( withCStructPhysicalDeviceCornerSampledImageFeaturesNV
  , fromCStructPhysicalDeviceCornerSampledImageFeaturesNV
  , PhysicalDeviceCornerSampledImageFeaturesNV(..)
  , pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  , pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( VkPhysicalDeviceCornerSampledImageFeaturesNV(..)
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  )



-- | VkPhysicalDeviceCornerSampledImageFeaturesNV - Structure describing
-- corner sampled image features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image.VkPhysicalDeviceCornerSampledImageFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image.VkPhysicalDeviceCornerSampledImageFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image.VkPhysicalDeviceCornerSampledImageFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "cornerSampledImage"
  cornerSampledImage :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceCornerSampledImageFeaturesNV' and
-- marshal a 'PhysicalDeviceCornerSampledImageFeaturesNV' into it. The 'VkPhysicalDeviceCornerSampledImageFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceCornerSampledImageFeaturesNV :: PhysicalDeviceCornerSampledImageFeaturesNV -> (VkPhysicalDeviceCornerSampledImageFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceCornerSampledImageFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceCornerSampledImageFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceCornerSampledImageFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV pPNext (boolToBool32 (cornerSampledImage (marshalled :: PhysicalDeviceCornerSampledImageFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceCornerSampledImageFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceCornerSampledImageFeaturesNV'.
fromCStructPhysicalDeviceCornerSampledImageFeaturesNV :: VkPhysicalDeviceCornerSampledImageFeaturesNV -> IO PhysicalDeviceCornerSampledImageFeaturesNV
fromCStructPhysicalDeviceCornerSampledImageFeaturesNV c = PhysicalDeviceCornerSampledImageFeaturesNV <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCornerSampledImageFeaturesNV)))
                                                                                                     <*> pure (bool32ToBool (vkCornerSampledImage (c :: VkPhysicalDeviceCornerSampledImageFeaturesNV)))

instance Zero PhysicalDeviceCornerSampledImageFeaturesNV where
  zero = PhysicalDeviceCornerSampledImageFeaturesNV Nothing
                                                    False


-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME"
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION"
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION :: Integral a => a
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
