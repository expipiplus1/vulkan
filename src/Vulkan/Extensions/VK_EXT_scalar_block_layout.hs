{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_scalar_block_layout"
module Vulkan.Extensions.VK_EXT_scalar_block_layout  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
                                                     , PhysicalDeviceScalarBlockLayoutFeaturesEXT
                                                     , EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
                                                     , pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
                                                     , EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
                                                     , pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT"
type PhysicalDeviceScalarBlockLayoutFeaturesEXT = PhysicalDeviceScalarBlockLayoutFeatures


type EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION"
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1


type EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME"
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

