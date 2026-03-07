{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_fragment_density_map_layered - device extension
--
-- = VK_VALVE_fragment_density_map_layered
--
-- [__Name String__]
--     @VK_VALVE_fragment_density_map_layered@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     612
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
--
-- [__Contact__]
--
--     -   Connor Abbott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_fragment_density_map_layered] @cwabbott0%0A*Here describe the issue or question you have about the VK_VALVE_fragment_density_map_layered extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-06
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1.
--
--     -   Interacts with Vulkan 1.3.
--
--     -   Interacts with Vulkan 1.4.
--
--     -   Interacts with @VK_EXT_fragment_density_map@.
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Connor Abbott, VALVE
--
--     -   Mike Blumenkrantz, VALVE
--
-- == Description
--
-- This extension enables the use of layered fragment density maps.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentDensityMapLayeredCreateInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_FRAGMENT_DENSITY_MAP_LAYERED_EXTENSION_NAME'
--
-- -   'VALVE_FRAGMENT_DENSITY_MAP_LAYERED_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RenderPassCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_FEATURES_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_LAYERED_PROPERTIES_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_DENSITY_MAP_LAYERED_CREATE_INFO_VALVE'
--
-- == Version History
--
-- -   Revision 1, 2025-06-06 (Mike Blumenkrantz)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_VALVE_fragment_density_map_layered Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_fragment_density_map_layered  ( PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE
                                                                , PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE
                                                                , PipelineFragmentDensityMapLayeredCreateInfoVALVE
                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE

instance ToCStruct PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE
instance Show PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE

instance FromCStruct PhysicalDeviceFragmentDensityMapLayeredFeaturesVALVE


data PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE

instance ToCStruct PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE
instance Show PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE

instance FromCStruct PhysicalDeviceFragmentDensityMapLayeredPropertiesVALVE


data PipelineFragmentDensityMapLayeredCreateInfoVALVE

instance ToCStruct PipelineFragmentDensityMapLayeredCreateInfoVALVE
instance Show PipelineFragmentDensityMapLayeredCreateInfoVALVE

instance FromCStruct PipelineFragmentDensityMapLayeredCreateInfoVALVE

