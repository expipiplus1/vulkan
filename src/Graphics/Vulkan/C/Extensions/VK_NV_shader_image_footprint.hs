{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( VkPhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
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


-- No documentation found for TopLevel "VkPhysicalDeviceShaderImageFootprintFeaturesNV"
data VkPhysicalDeviceShaderImageFootprintFeaturesNV = VkPhysicalDeviceShaderImageFootprintFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShaderImageFootprintFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderImageFootprintFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceShaderImageFootprintFeaturesNV" "imageFootprint"
  vkImageFootprint :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShaderImageFootprintFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShaderImageFootprintFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShaderImageFootprintFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShaderImageFootprintFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkImageFootprint (poked :: VkPhysicalDeviceShaderImageFootprintFeaturesNV))

instance Zero VkPhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = VkPhysicalDeviceShaderImageFootprintFeaturesNV zero
                                                        zero
                                                        zero
-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME"
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"
-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION"
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION :: Integral a => a
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV = VkStructureType 1000204000
