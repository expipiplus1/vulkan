{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_variable_pointers  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
                                                   , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
                                                   , PhysicalDeviceVariablePointersFeaturesKHR
                                                   , PhysicalDeviceVariablePointerFeaturesKHR
                                                   , KHR_VARIABLE_POINTERS_SPEC_VERSION
                                                   , pattern KHR_VARIABLE_POINTERS_SPEC_VERSION
                                                   , KHR_VARIABLE_POINTERS_EXTENSION_NAME
                                                   , pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME
                                                   ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeaturesKHR"
type PhysicalDeviceVariablePointersFeaturesKHR = PhysicalDeviceVariablePointersFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeaturesKHR"
type PhysicalDeviceVariablePointerFeaturesKHR = PhysicalDeviceVariablePointersFeatures


type KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_SPEC_VERSION"
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION = 1


type KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME"
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"

