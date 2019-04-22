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


-- | VkPhysicalDeviceShaderImageFootprintFeaturesNV - Structure describing
-- shader image footprint features that can be supported by an
-- implementation
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-footprint Texel Footprint Evaluation>
-- for more information.
--
-- If the 'VkPhysicalDeviceShaderImageFootprintFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceShaderImageFootprintFeaturesNV' /can/ also be used in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceShaderImageFootprintFeaturesNV = VkPhysicalDeviceShaderImageFootprintFeaturesNV
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderImageFootprintFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @imageFootprint@ specifies whether the implementation supports the
  -- @ImageFootprintNV@ SPIR-V capability.
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
  zero = VkPhysicalDeviceShaderImageFootprintFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
                                                        zero
                                                        zero

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME"
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION"
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION :: Integral a => a
pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV = VkStructureType 1000204000
