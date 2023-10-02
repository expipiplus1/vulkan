{-# language CPP #-}
-- | = Name
--
-- VK_EXT_graphics_pipeline_library - device extension
--
-- == VK_EXT_graphics_pipeline_library
--
-- [__Name String__]
--     @VK_EXT_graphics_pipeline_library@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     321
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_graphics_pipeline_library] @tobski%0A*Here describe the issue or question you have about the VK_EXT_graphics_pipeline_library extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_graphics_pipeline_library.adoc VK_EXT_graphics_pipeline_library>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-08-17
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Chris Glover, Google
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Piers Daniell, NVidia
--
--     -   Boris Zanin, Mobica
--
--     -   Krzysztof Niski, NVidia
--
--     -   Dan Ginsburg, Valve
--
--     -   Sebastian Aaltonen, Unity
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   Calle Lejdfors, Ubisoft
--
--     -   Tiago Rodrigues, Ubisoft
--
--     -   Francois Duranleau, Gameloft
--
-- == Description
--
-- This extension allows the separate compilation of four distinct parts of
-- graphics pipelines, with the intent of allowing faster pipeline loading
-- for applications reusing the same shaders or state in multiple
-- pipelines. Each part can be independently compiled into a graphics
-- pipeline library, with a final link step required to create an
-- executable pipeline that can be bound to a command buffer.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'GraphicsPipelineLibraryCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT'
--
-- == New Enums
--
-- -   'GraphicsPipelineLibraryFlagBitsEXT'
--
-- -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits'
--
-- == New Bitmasks
--
-- -   'GraphicsPipelineLibraryFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME'
--
-- -   'EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-08-17 (Tobias Hector)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'GraphicsPipelineLibraryCreateInfoEXT',
-- 'GraphicsPipelineLibraryFlagBitsEXT', 'GraphicsPipelineLibraryFlagsEXT',
-- 'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT',
-- 'PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT',
-- 'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_graphics_pipeline_library  ( GraphicsPipelineLibraryCreateInfoEXT
                                                           , PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
                                                           , PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data GraphicsPipelineLibraryCreateInfoEXT

instance ToCStruct GraphicsPipelineLibraryCreateInfoEXT
instance Show GraphicsPipelineLibraryCreateInfoEXT

instance FromCStruct GraphicsPipelineLibraryCreateInfoEXT


data PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT

instance ToCStruct PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
instance Show PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT

instance FromCStruct PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT


data PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT

instance ToCStruct PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
instance Show PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT

instance FromCStruct PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT

