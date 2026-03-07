{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance6 - device extension
--
-- = VK_KHR_maintenance6
--
-- [__Name String__]
--     @VK_KHR_maintenance6@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     546
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_KHR_push_descriptor
--
-- [__Contact__]
--
--     -   Jon Leech
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance6] @oddhack%0A*Here describe the issue or question you have about the VK_KHR_maintenance6 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance6.adoc VK_KHR_maintenance6>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-03
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_EXT_robustness2@
--
-- [__Contributors__]
--
--     -   Jon Leech, Khronos
--
--     -   Stu Smith, AMD
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Ralph Potter, Samsung
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   'BindMemoryStatusKHR' may be included in the @pNext@ chain of
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
--     allowing applications to identify individual resources for which
--     memory binding failed during calls to
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindBufferMemory2'
--     and
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2'.
--
-- -   A new property @fragmentShadingRateClampCombinerInputs@ to indicate
--     if an implementation clamps the inputs to fragment shading rate
--     combiner operations.
--
-- -   'Vulkan.Core10.APIConstants.NULL_HANDLE' is allowed to be used when
--     binding an index buffer, instead of a valid
--     'Vulkan.Core10.Handles.Buffer' handle. When the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is enabled, every index fetched results in a value of zero.
--
-- -   A new property @maxCombinedImageSamplerDescriptorCount@ to indicate
--     the maximum number of descriptors needed for any of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
--     supported by the implementation.
--
-- -   A new property @blockTexelViewCompatibleMultipleLayers@ indicating
--     whether
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT'
--     is allowed to be used with @layerCount@ > 1
--
-- -   @pNext@ extensible *2 versions of all descriptor binding commands.
--
-- == New Commands
--
-- -   'cmdBindDescriptorSets2KHR'
--
-- -   'cmdPushConstants2KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'cmdBindDescriptorBufferEmbeddedSamplers2EXT'
--
-- -   'cmdSetDescriptorBufferOffsets2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'cmdPushDescriptorSet2KHR'
--
-- -   'cmdPushDescriptorSetWithTemplate2KHR'
--
-- == New Structures
--
-- -   'BindDescriptorSetsInfoKHR'
--
-- -   'PushConstantsInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'BindMemoryStatusKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance6FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance6PropertiesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'BindDescriptorBufferEmbeddedSamplersInfoEXT'
--
-- -   'SetDescriptorBufferOffsetsInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'PushDescriptorSetInfoKHR'
--
-- -   'PushDescriptorSetWithTemplateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_6_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_6_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_SETS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_MEMORY_STATUS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_6_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANTS_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DESCRIPTOR_BUFFER_EMBEDDED_SAMPLERS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_DESCRIPTOR_BUFFER_OFFSETS_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DESCRIPTOR_SET_WITH_TEMPLATE_INFO_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-08-01 (Jon Leech)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance6 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance6  ( BindDescriptorBufferEmbeddedSamplersInfoEXT
                                              , BindDescriptorSetsInfoKHR
                                              , BindMemoryStatusKHR
                                              , PhysicalDeviceMaintenance6FeaturesKHR
                                              , PhysicalDeviceMaintenance6PropertiesKHR
                                              , PushConstantsInfoKHR
                                              , PushDescriptorSetInfoKHR
                                              , PushDescriptorSetWithTemplateInfoKHR
                                              , SetDescriptorBufferOffsetsInfoEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role BindDescriptorBufferEmbeddedSamplersInfoEXT nominal
data BindDescriptorBufferEmbeddedSamplersInfoEXT (es :: [Type])

instance ( Extendss BindDescriptorBufferEmbeddedSamplersInfoEXT es
         , PokeChain es ) => ToCStruct (BindDescriptorBufferEmbeddedSamplersInfoEXT es)
instance Show (Chain es) => Show (BindDescriptorBufferEmbeddedSamplersInfoEXT es)

instance ( Extendss BindDescriptorBufferEmbeddedSamplersInfoEXT es
         , PeekChain es ) => FromCStruct (BindDescriptorBufferEmbeddedSamplersInfoEXT es)


type role BindDescriptorSetsInfoKHR nominal
data BindDescriptorSetsInfoKHR (es :: [Type])

instance ( Extendss BindDescriptorSetsInfoKHR es
         , PokeChain es ) => ToCStruct (BindDescriptorSetsInfoKHR es)
instance Show (Chain es) => Show (BindDescriptorSetsInfoKHR es)

instance ( Extendss BindDescriptorSetsInfoKHR es
         , PeekChain es ) => FromCStruct (BindDescriptorSetsInfoKHR es)


data BindMemoryStatusKHR

instance ToCStruct BindMemoryStatusKHR
instance Show BindMemoryStatusKHR

instance FromCStruct BindMemoryStatusKHR


data PhysicalDeviceMaintenance6FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance6FeaturesKHR
instance Show PhysicalDeviceMaintenance6FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance6FeaturesKHR


data PhysicalDeviceMaintenance6PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance6PropertiesKHR
instance Show PhysicalDeviceMaintenance6PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance6PropertiesKHR


type role PushConstantsInfoKHR nominal
data PushConstantsInfoKHR (es :: [Type])

instance ( Extendss PushConstantsInfoKHR es
         , PokeChain es ) => ToCStruct (PushConstantsInfoKHR es)
instance Show (Chain es) => Show (PushConstantsInfoKHR es)

instance ( Extendss PushConstantsInfoKHR es
         , PeekChain es ) => FromCStruct (PushConstantsInfoKHR es)


type role PushDescriptorSetInfoKHR nominal
data PushDescriptorSetInfoKHR (es :: [Type])

instance ( Extendss PushDescriptorSetInfoKHR es
         , PokeChain es ) => ToCStruct (PushDescriptorSetInfoKHR es)
instance Show (Chain es) => Show (PushDescriptorSetInfoKHR es)

instance ( Extendss PushDescriptorSetInfoKHR es
         , PeekChain es ) => FromCStruct (PushDescriptorSetInfoKHR es)


type role PushDescriptorSetWithTemplateInfoKHR nominal
data PushDescriptorSetWithTemplateInfoKHR (es :: [Type])

instance ( Extendss PushDescriptorSetWithTemplateInfoKHR es
         , PokeChain es ) => ToCStruct (PushDescriptorSetWithTemplateInfoKHR es)
instance Show (Chain es) => Show (PushDescriptorSetWithTemplateInfoKHR es)

instance ( Extendss PushDescriptorSetWithTemplateInfoKHR es
         , PeekChain es ) => FromCStruct (PushDescriptorSetWithTemplateInfoKHR es)


type role SetDescriptorBufferOffsetsInfoEXT nominal
data SetDescriptorBufferOffsetsInfoEXT (es :: [Type])

instance ( Extendss SetDescriptorBufferOffsetsInfoEXT es
         , PokeChain es ) => ToCStruct (SetDescriptorBufferOffsetsInfoEXT es)
instance Show (Chain es) => Show (SetDescriptorBufferOffsetsInfoEXT es)

instance ( Extendss SetDescriptorBufferOffsetsInfoEXT es
         , PeekChain es ) => FromCStruct (SetDescriptorBufferOffsetsInfoEXT es)

