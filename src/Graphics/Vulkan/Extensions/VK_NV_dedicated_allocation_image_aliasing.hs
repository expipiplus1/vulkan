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


-- No documentation found for TopLevel "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV"
data PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV" "dedicatedAllocationImageAliasing"
  vkDedicatedAllocationImageAliasing :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> (VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV pPNext (boolToBool32 (vkDedicatedAllocationImageAliasing (from :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))))
fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> IO PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV c = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV <$> -- Univalued Member elided
                                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))
                                                                                                                                 <*> pure (bool32ToBool (vkDedicatedAllocationImageAliasing (c :: VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)))
