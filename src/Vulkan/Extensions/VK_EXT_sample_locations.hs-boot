{-# language CPP #-}
-- | = Name
--
-- VK_EXT_sample_locations - device extension
--
-- == VK_EXT_sample_locations
--
-- [__Name String__]
--     @VK_EXT_sample_locations@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     144
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_sample_locations] @drakos-amd%0A*Here describe the issue or question you have about the VK_EXT_sample_locations extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-02
--
-- [__Contributors__]
--
--     -   Mais Alnasser, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Slawomir Grajewski, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Bill Licea-Kane, Qualcomm
--
-- == Description
--
-- This extension allows an application to modify the locations of samples
-- within a pixel used in rasterization. Additionally, it allows
-- applications to specify different sample locations for each pixel in a
-- group of adjacent pixels, which /can/ increase antialiasing quality
-- (particularly if a custom resolve shader is used that takes advantage of
-- these different locations).
--
-- It is common for implementations to optimize the storage of depth values
-- by storing values that /can/ be used to reconstruct depth at each sample
-- location, rather than storing separate depth values for each sample. For
-- example, the depth values from a single triangle /may/ be represented
-- using plane equations. When the depth value for a sample is needed, it
-- is automatically evaluated at the sample location. Modifying the sample
-- locations causes the reconstruction to no longer evaluate the same depth
-- values as when the samples were originally generated, thus the depth
-- aspect of a depth\/stencil attachment /must/ be cleared before rendering
-- to it using different sample locations.
--
-- Some implementations /may/ need to evaluate depth image values while
-- performing image layout transitions. To accommodate this, instances of
-- the 'SampleLocationsInfoEXT' structure /can/ be specified for each
-- situation where an explicit or automatic layout transition has to take
-- place. 'SampleLocationsInfoEXT' /can/ be chained from
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures to provide
-- sample locations for layout transitions performed by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier' calls, and
-- 'RenderPassSampleLocationsBeginInfoEXT' /can/ be chained from
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo' to provide
-- sample locations for layout transitions performed implicitly by a render
-- pass instance.
--
-- == New Commands
--
-- -   'cmdSetSampleLocationsEXT'
--
-- -   'getPhysicalDeviceMultisamplePropertiesEXT'
--
-- == New Structures
--
-- -   'AttachmentSampleLocationsEXT'
--
-- -   'MultisamplePropertiesEXT'
--
-- -   'SampleLocationEXT'
--
-- -   'SubpassSampleLocationsEXT'
--
-- -   Extending 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2':
--
--     -   'SampleLocationsInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSampleLocationsPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo':
--
--     -   'PipelineSampleLocationsStateCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'RenderPassSampleLocationsBeginInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SAMPLE_LOCATIONS_EXTENSION_NAME'
--
-- -   'EXT_SAMPLE_LOCATIONS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2017-08-02 (Daniel Rakos)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'AttachmentSampleLocationsEXT', 'MultisamplePropertiesEXT',
-- 'PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'PipelineSampleLocationsStateCreateInfoEXT',
-- 'RenderPassSampleLocationsBeginInfoEXT', 'SampleLocationEXT',
-- 'SampleLocationsInfoEXT', 'SubpassSampleLocationsEXT',
-- 'cmdSetSampleLocationsEXT', 'getPhysicalDeviceMultisamplePropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_sample_locations Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_sample_locations  ( AttachmentSampleLocationsEXT
                                                  , MultisamplePropertiesEXT
                                                  , PhysicalDeviceSampleLocationsPropertiesEXT
                                                  , PipelineSampleLocationsStateCreateInfoEXT
                                                  , RenderPassSampleLocationsBeginInfoEXT
                                                  , SampleLocationEXT
                                                  , SampleLocationsInfoEXT
                                                  , SubpassSampleLocationsEXT
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AttachmentSampleLocationsEXT

instance ToCStruct AttachmentSampleLocationsEXT
instance Show AttachmentSampleLocationsEXT

instance FromCStruct AttachmentSampleLocationsEXT


data MultisamplePropertiesEXT

instance ToCStruct MultisamplePropertiesEXT
instance Show MultisamplePropertiesEXT

instance FromCStruct MultisamplePropertiesEXT


data PhysicalDeviceSampleLocationsPropertiesEXT

instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT
instance Show PhysicalDeviceSampleLocationsPropertiesEXT

instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT


data PipelineSampleLocationsStateCreateInfoEXT

instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT
instance Show PipelineSampleLocationsStateCreateInfoEXT

instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT


data RenderPassSampleLocationsBeginInfoEXT

instance ToCStruct RenderPassSampleLocationsBeginInfoEXT
instance Show RenderPassSampleLocationsBeginInfoEXT

instance FromCStruct RenderPassSampleLocationsBeginInfoEXT


data SampleLocationEXT

instance ToCStruct SampleLocationEXT
instance Show SampleLocationEXT

instance FromCStruct SampleLocationEXT


data SampleLocationsInfoEXT

instance ToCStruct SampleLocationsInfoEXT
instance Show SampleLocationsInfoEXT

instance FromCStruct SampleLocationsInfoEXT


data SubpassSampleLocationsEXT

instance ToCStruct SubpassSampleLocationsEXT
instance Show SubpassSampleLocationsEXT

instance FromCStruct SubpassSampleLocationsEXT

