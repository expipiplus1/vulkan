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
  )


-- No documentation found for TopLevel "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV"
data VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "dedicatedAllocationImageAliasing"
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
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME"
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = "VK_NV_dedicated_allocation_image_aliasing"
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION"
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV = VkStructureType 1000240000
