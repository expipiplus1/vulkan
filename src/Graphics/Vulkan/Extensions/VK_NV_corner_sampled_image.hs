{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( withCStructPhysicalDeviceCornerSampledImageFeaturesNV
  , fromCStructPhysicalDeviceCornerSampledImageFeaturesNV
  , PhysicalDeviceCornerSampledImageFeaturesNV(..)
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  ) where

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
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( pattern VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
  , pattern VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceCornerSampledImageFeaturesNV"
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCornerSampledImageFeaturesNV" "cornerSampledImage"
  vkCornerSampledImage :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceCornerSampledImageFeaturesNV :: PhysicalDeviceCornerSampledImageFeaturesNV -> (VkPhysicalDeviceCornerSampledImageFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceCornerSampledImageFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceCornerSampledImageFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceCornerSampledImageFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV pPNext (boolToBool32 (vkCornerSampledImage (from :: PhysicalDeviceCornerSampledImageFeaturesNV)))))
fromCStructPhysicalDeviceCornerSampledImageFeaturesNV :: VkPhysicalDeviceCornerSampledImageFeaturesNV -> IO PhysicalDeviceCornerSampledImageFeaturesNV
fromCStructPhysicalDeviceCornerSampledImageFeaturesNV c = PhysicalDeviceCornerSampledImageFeaturesNV <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCornerSampledImageFeaturesNV)))
                                                                                                     <*> pure (bool32ToBool (vkCornerSampledImage (c :: VkPhysicalDeviceCornerSampledImageFeaturesNV)))
instance Zero PhysicalDeviceCornerSampledImageFeaturesNV where
  zero = PhysicalDeviceCornerSampledImageFeaturesNV Nothing
                                                    False
