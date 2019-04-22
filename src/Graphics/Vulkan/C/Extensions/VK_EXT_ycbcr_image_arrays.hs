{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
  , pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
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


-- | VkPhysicalDeviceYcbcrImageArraysFeaturesEXT - Structure describing
-- extended Y
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'VkPhysicalDeviceYcbcrImageArraysFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceYcbcrImageArraysFeaturesEXT = VkPhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @ycbcrImageArrays@ indicates that the implementation supports creating
  -- images with a format that requires
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y’CBCR conversion>
  -- and has multiple array layers.
  vkYcbcrImageArrays :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceYcbcrImageArraysFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceYcbcrImageArraysFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkYcbcrImageArrays (poked :: VkPhysicalDeviceYcbcrImageArraysFeaturesEXT))

instance Zero VkPhysicalDeviceYcbcrImageArraysFeaturesEXT where
  zero = VkPhysicalDeviceYcbcrImageArraysFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
                                                     zero
                                                     zero

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT = VkStructureType 1000252000
