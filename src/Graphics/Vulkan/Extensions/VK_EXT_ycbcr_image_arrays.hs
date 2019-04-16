{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  , fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  , PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceYcbcrImageArraysFeaturesEXT"
data PhysicalDeviceYcbcrImageArraysFeaturesEXT = PhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "ycbcrImageArrays"
  vkYcbcrImageArrays :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT :: PhysicalDeviceYcbcrImageArraysFeaturesEXT -> (VkPhysicalDeviceYcbcrImageArraysFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceYcbcrImageArraysFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT pPNext (boolToBool32 (vkYcbcrImageArrays (from :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)))))
fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT -> IO PhysicalDeviceYcbcrImageArraysFeaturesEXT
fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT c = PhysicalDeviceYcbcrImageArraysFeaturesEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT)))
                                                                                                   <*> pure (bool32ToBool (vkYcbcrImageArrays (c :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT)))
