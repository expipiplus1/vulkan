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
  )


-- No documentation found for TopLevel "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT"
data VkPhysicalDeviceYcbcrImageArraysFeaturesEXT = VkPhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceYcbcrImageArraysFeaturesEXT" "ycbcrImageArrays"
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
-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"
-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT = VkStructureType 1000252000
