{-# language CPP #-}
-- | = Name
--
-- VK_KHR_device_address_commands - device extension
--
-- = VK_KHR_device_address_commands
--
-- [__Name String__]
--     @VK_KHR_device_address_commands@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     319
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--                     
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--                      or
--                     
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--                  and
--                 
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state VK_EXT_extended_dynamic_state>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_2
--
--     -   Interacts with VK_AMD_buffer_marker
--
--     -   Interacts with VK_EXT_conditional_rendering
--
--     -   Interacts with VK_EXT_mesh_shader
--
--     -   Interacts with VK_EXT_transform_feedback
--
--     -   Interacts with VK_KHR_acceleration_structure
--
--     -   Interacts with VK_KHR_draw_indirect_count
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_device_address_commands] @tobski%0A*Here describe the issue or question you have about the VK_KHR_device_address_commands extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_device_address_commands.adoc VK_KHR_device_address_commands>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-10
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Allan MacKinnon, Google
--
--     -   Daniel Koch, NVIDIA
--
--     -   Autumn Ashton, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Stuart Smith, AMD
--
--     -   Caterina Shablia, Collabora
--
--     -   Piotr Byszewski, Cognizant
--
--     -   James Fitzpatrick, Imagination
--
--     -   Daniel Story, Nintendo
--
--     -   Baldur Karlsson, Valve
--
--     -   Jon Leech, Khronos
--
--     -   Samuel Pitoiset, Valve
--
--     -   Lionel Landwerlin, Intel
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Žiga Markuš, LunarG
--
--     -   Spencer Fricke, Lunarg
--
--     -   Per Inge Mathisen, Arm
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Ralph Potter, Samsung
--
--     -   Marijn Suijten, Traverse Research
--
--     -   Ricard Garcia, Igalia
--
--     -   Mengyang Liu, AMD
--
--     -   Artem Kharytoniuk, LunarG
--
--     -   Marty Johnson, Khronos
--
--     -   Alyssa Rosenzweig, Valve
--
-- == Description
--
-- This extension enables applications to use device addresses in place of
-- buffer objects for most functionality.
--
-- == New Commands
--
-- -   'cmdBindIndexBuffer3KHR'
--
-- -   'cmdBindVertexBuffers3KHR'
--
-- -   'cmdCopyImageToMemoryKHR'
--
-- -   'cmdCopyMemoryKHR'
--
-- -   'cmdCopyMemoryToImageKHR'
--
-- -   'cmdCopyQueryPoolResultsToMemoryKHR'
--
-- -   'cmdDispatchIndirect2KHR'
--
-- -   'cmdDrawIndexedIndirect2KHR'
--
-- -   'cmdDrawIndirect2KHR'
--
-- -   'cmdFillMemoryKHR'
--
-- -   'cmdUpdateMemoryKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_draw_indirect_count VK_KHR_draw_indirect_count>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- is supported:
--
-- -   'cmdDrawMeshTasksIndirectCount2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>
-- is supported:
--
-- -   'cmdWriteMarkerToMemoryAMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   'cmdBeginConditionalRendering2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- is supported:
--
-- -   'cmdDrawMeshTasksIndirect2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   'cmdBeginTransformFeedback2EXT'
--
-- -   'cmdBindTransformFeedbackBuffers2EXT'
--
-- -   'cmdDrawIndirectByteCount2EXT'
--
-- -   'cmdEndTransformFeedback2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   'createAccelerationStructure2KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_draw_indirect_count VK_KHR_draw_indirect_count>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
-- is supported:
--
-- -   'cmdDrawIndexedIndirectCount2KHR'
--
-- -   'cmdDrawIndirectCount2KHR'
--
-- == New Structures
--
-- -   'BindIndexBuffer3InfoKHR'
--
-- -   'BindVertexBuffer3InfoKHR'
--
-- -   'CopyDeviceMemoryImageInfoKHR'
--
-- -   'CopyDeviceMemoryInfoKHR'
--
-- -   'DeviceAddressRangeKHR'
--
-- -   'DeviceMemoryCopyKHR'
--
-- -   'DeviceMemoryImageCopyKHR'
--
-- -   'DispatchIndirect2InfoKHR'
--
-- -   'DrawIndirect2InfoKHR'
--
-- -   'DrawIndirectCount2InfoKHR'
--
-- -   'MemoryRangeBarrierKHR'
--
-- -   'StridedDeviceAddressRangeKHR'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.DependencyInfo':
--
--     -   'MemoryRangeBarriersInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceAddressCommandsFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>
-- is supported:
--
-- -   'MemoryMarkerInfoAMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   'ConditionalRenderingBeginInfo2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   'BindTransformFeedbackBuffer2InfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   'AccelerationStructureCreateInfo2KHR'
--
-- == New Enums
--
-- -   'AddressCommandFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'AddressCommandFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEVICE_ADDRESS_COMMANDS_EXTENSION_NAME'
--
-- -   'KHR_DEVICE_ADDRESS_COMMANDS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_INDEX_BUFFER_3_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_VERTEX_BUFFER_3_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_DEVICE_MEMORY_IMAGE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_DEVICE_MEMORY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_COPY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_IMAGE_COPY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPATCH_INDIRECT_2_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DRAW_INDIRECT_2_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DRAW_INDIRECT_COUNT_2_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_RANGE_BARRIERS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_RANGE_BARRIER_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_ADDRESS_COMMANDS_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MARKER_INFO_AMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_2_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   Extending 'AddressCommandFlagBitsKHR':
--
--     -   'ADDRESS_COMMAND_TRANSFORM_FEEDBACK_BUFFER_USAGE_BIT_KHR'
--
--     -   'ADDRESS_COMMAND_UNKNOWN_TRANSFORM_FEEDBACK_BUFFER_USAGE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_TRANSFORM_FEEDBACK_BUFFER_2_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_2_KHR'
--
-- == Version History
--
-- -   Revision 1, 2026-03-10 (Tobias Hector)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_device_address_commands Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_device_address_commands  ( AccelerationStructureCreateInfo2KHR
                                                         , BindIndexBuffer3InfoKHR
                                                         , BindTransformFeedbackBuffer2InfoEXT
                                                         , BindVertexBuffer3InfoKHR
                                                         , ConditionalRenderingBeginInfo2EXT
                                                         , CopyDeviceMemoryImageInfoKHR
                                                         , CopyDeviceMemoryInfoKHR
                                                         , DeviceAddressRangeKHR
                                                         , DeviceMemoryCopyKHR
                                                         , DeviceMemoryImageCopyKHR
                                                         , DispatchIndirect2InfoKHR
                                                         , DrawIndirect2InfoKHR
                                                         , DrawIndirectCount2InfoKHR
                                                         , MemoryMarkerInfoAMD
                                                         , MemoryRangeBarrierKHR
                                                         , MemoryRangeBarriersInfoKHR
                                                         , PhysicalDeviceDeviceAddressCommandsFeaturesKHR
                                                         , StridedDeviceAddressRangeKHR
                                                         , AddressCommandFlagsKHR
                                                         , AddressCommandFlagBitsKHR
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role AccelerationStructureCreateInfo2KHR nominal
data AccelerationStructureCreateInfo2KHR (es :: [Type])

instance ( Extendss AccelerationStructureCreateInfo2KHR es
         , PokeChain es ) => ToCStruct (AccelerationStructureCreateInfo2KHR es)
instance Show (Chain es) => Show (AccelerationStructureCreateInfo2KHR es)

instance ( Extendss AccelerationStructureCreateInfo2KHR es
         , PeekChain es ) => FromCStruct (AccelerationStructureCreateInfo2KHR es)


data BindIndexBuffer3InfoKHR

instance ToCStruct BindIndexBuffer3InfoKHR
instance Show BindIndexBuffer3InfoKHR

instance FromCStruct BindIndexBuffer3InfoKHR


data BindTransformFeedbackBuffer2InfoEXT

instance ToCStruct BindTransformFeedbackBuffer2InfoEXT
instance Show BindTransformFeedbackBuffer2InfoEXT

instance FromCStruct BindTransformFeedbackBuffer2InfoEXT


data BindVertexBuffer3InfoKHR

instance ToCStruct BindVertexBuffer3InfoKHR
instance Show BindVertexBuffer3InfoKHR

instance FromCStruct BindVertexBuffer3InfoKHR


data ConditionalRenderingBeginInfo2EXT

instance ToCStruct ConditionalRenderingBeginInfo2EXT
instance Show ConditionalRenderingBeginInfo2EXT

instance FromCStruct ConditionalRenderingBeginInfo2EXT


data CopyDeviceMemoryImageInfoKHR

instance ToCStruct CopyDeviceMemoryImageInfoKHR
instance Show CopyDeviceMemoryImageInfoKHR

instance FromCStruct CopyDeviceMemoryImageInfoKHR


data CopyDeviceMemoryInfoKHR

instance ToCStruct CopyDeviceMemoryInfoKHR
instance Show CopyDeviceMemoryInfoKHR

instance FromCStruct CopyDeviceMemoryInfoKHR


data DeviceAddressRangeKHR

instance ToCStruct DeviceAddressRangeKHR
instance Show DeviceAddressRangeKHR

instance FromCStruct DeviceAddressRangeKHR


data DeviceMemoryCopyKHR

instance ToCStruct DeviceMemoryCopyKHR
instance Show DeviceMemoryCopyKHR

instance FromCStruct DeviceMemoryCopyKHR


type role DeviceMemoryImageCopyKHR nominal
data DeviceMemoryImageCopyKHR (es :: [Type])

instance ( Extendss DeviceMemoryImageCopyKHR es
         , PokeChain es ) => ToCStruct (DeviceMemoryImageCopyKHR es)
instance Show (Chain es) => Show (DeviceMemoryImageCopyKHR es)

instance ( Extendss DeviceMemoryImageCopyKHR es
         , PeekChain es ) => FromCStruct (DeviceMemoryImageCopyKHR es)


data DispatchIndirect2InfoKHR

instance ToCStruct DispatchIndirect2InfoKHR
instance Show DispatchIndirect2InfoKHR

instance FromCStruct DispatchIndirect2InfoKHR


data DrawIndirect2InfoKHR

instance ToCStruct DrawIndirect2InfoKHR
instance Show DrawIndirect2InfoKHR

instance FromCStruct DrawIndirect2InfoKHR


data DrawIndirectCount2InfoKHR

instance ToCStruct DrawIndirectCount2InfoKHR
instance Show DrawIndirectCount2InfoKHR

instance FromCStruct DrawIndirectCount2InfoKHR


data MemoryMarkerInfoAMD

instance ToCStruct MemoryMarkerInfoAMD
instance Show MemoryMarkerInfoAMD

instance FromCStruct MemoryMarkerInfoAMD


data MemoryRangeBarrierKHR

instance ToCStruct MemoryRangeBarrierKHR
instance Show MemoryRangeBarrierKHR

instance FromCStruct MemoryRangeBarrierKHR


type role MemoryRangeBarriersInfoKHR nominal
data MemoryRangeBarriersInfoKHR (es :: [Type])

instance ( Extendss MemoryRangeBarriersInfoKHR es
         , PokeChain es ) => ToCStruct (MemoryRangeBarriersInfoKHR es)
instance Show (Chain es) => Show (MemoryRangeBarriersInfoKHR es)

instance ( Extendss MemoryRangeBarriersInfoKHR es
         , PeekChain es ) => FromCStruct (MemoryRangeBarriersInfoKHR es)


data PhysicalDeviceDeviceAddressCommandsFeaturesKHR

instance ToCStruct PhysicalDeviceDeviceAddressCommandsFeaturesKHR
instance Show PhysicalDeviceDeviceAddressCommandsFeaturesKHR

instance FromCStruct PhysicalDeviceDeviceAddressCommandsFeaturesKHR


data StridedDeviceAddressRangeKHR

instance ToCStruct StridedDeviceAddressRangeKHR
instance Show StridedDeviceAddressRangeKHR

instance FromCStruct StridedDeviceAddressRangeKHR


type AddressCommandFlagsKHR = AddressCommandFlagBitsKHR

data AddressCommandFlagBitsKHR

