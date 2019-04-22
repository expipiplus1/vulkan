{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  , fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  , PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  )



-- | VkPhysicalDeviceYcbcrImageArraysFeaturesEXT - Structure describing
-- extended Y
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays.VkPhysicalDeviceYcbcrImageArraysFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays.VkPhysicalDeviceYcbcrImageArraysFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays.VkPhysicalDeviceYcbcrImageArraysFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceYcbcrImageArraysFeaturesEXT = PhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceYcbcrImageArraysFeaturesEXT" "ycbcrImageArrays"
  ycbcrImageArrays :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' and
-- marshal a 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' into it. The 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT :: PhysicalDeviceYcbcrImageArraysFeaturesEXT -> (VkPhysicalDeviceYcbcrImageArraysFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceYcbcrImageArraysFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT pPNext (boolToBool32 (ycbcrImageArrays (marshalled :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceYcbcrImageArraysFeaturesEXT'.
fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT -> IO PhysicalDeviceYcbcrImageArraysFeaturesEXT
fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT c = PhysicalDeviceYcbcrImageArraysFeaturesEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT)))
                                                                                                   <*> pure (bool32ToBool (vkYcbcrImageArrays (c :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT)))

instance Zero PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  zero = PhysicalDeviceYcbcrImageArraysFeaturesEXT Nothing
                                                   False


-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: Integral a => a
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
