{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
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


-- No documentation found for TopLevel "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT"
data VkPhysicalDeviceScalarBlockLayoutFeaturesEXT = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT" "scalarBlockLayout"
  vkScalarBlockLayout :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkScalarBlockLayout (poked :: VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))

instance Zero VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  zero = VkPhysicalDeviceScalarBlockLayoutFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
                                                      zero
                                                      zero

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME"
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION"
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT = VkStructureType 1000221000
