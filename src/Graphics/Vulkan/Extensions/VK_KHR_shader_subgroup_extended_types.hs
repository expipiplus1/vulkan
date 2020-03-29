{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shader_subgroup_extended_types  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR
                                                                         , PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR
                                                                         , KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION
                                                                         , pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION
                                                                         , KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME
                                                                         , pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME
                                                                         ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR"
type PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR = PhysicalDeviceShaderSubgroupExtendedTypesFeatures


type KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION"
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION = 1


type KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME = "VK_KHR_shader_subgroup_extended_types"

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME"
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME = "VK_KHR_shader_subgroup_extended_types"

