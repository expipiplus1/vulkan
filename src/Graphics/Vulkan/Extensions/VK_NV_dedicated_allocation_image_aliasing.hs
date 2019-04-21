{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  , fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  , PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
  )



-- | VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV - Structure
-- describing dedicated allocation image aliasing features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing.VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing.VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing.VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "dedicatedAllocationImageAliasing"
  dedicatedAllocationImageAliasing :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' and
-- marshal a 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' into it. The 'VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> (VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV pPNext (boolToBool32 (dedicatedAllocationImageAliasing (marshalled :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'.
fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> IO PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV c = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV <$> -- Univalued Member elided
                                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))
                                                                                                                                 <*> pure (bool32ToBool (vkDedicatedAllocationImageAliasing (c :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))

instance Zero PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  zero = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV Nothing
                                                                  False

