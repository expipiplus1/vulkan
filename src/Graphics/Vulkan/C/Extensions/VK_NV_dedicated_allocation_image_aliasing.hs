{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV - Structure
-- describing dedicated allocation image aliasing features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- @VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV@ structure
-- describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV@
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV@ /can/ also
-- be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @dedicatedAllocationImageAliasing@ indicates that the implementation
  -- supports aliasing of compatible image objects on a dedicated allocation.
  vkDedicatedAllocationImageAliasing :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                                        <*> peek (ptr `plusPtr` 8)
                                                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkDedicatedAllocationImageAliasing (poked :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV))

instance Zero VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  zero = VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV zero
                                                                    zero
                                                                    zero
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME"
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = "VK_NV_dedicated_allocation_image_aliasing"
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION"
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV = VkStructureType 1000240000
