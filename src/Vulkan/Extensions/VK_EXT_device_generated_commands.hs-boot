{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_generated_commands - device extension
--
-- = VK_EXT_device_generated_commands
--
-- [__Name String__]
--     @VK_EXT_device_generated_commands@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     573
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_shader_object
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_device_generated_commands] @zmike%0A*Here describe the issue or question you have about the VK_EXT_device_generated_commands extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_device_generated_commands.adoc VK_EXT_device_generated_commands>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-23
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires Vulkan 1.1
--
--     -   This extension requires @VK_EXT_buffer_device_address@ or
--         @VK_KHR_buffer_device_address@ or Vulkan 1.2 for the ability to
--         bind vertex and index buffers on the device.
--
--     -   This extension requires @VK_KHR_maintenance5@ for the ability to
--         use VkPipelineCreateFlags2KHR.
--
--     -   This extension interacts with @VK_NV_mesh_shader@. If the latter
--         extension is not supported, remove the command tokens to
--         initiate NV mesh tasks drawing in this extension.
--
--     -   This extension interacts with @VK_EXT_mesh_shader@. If the
--         latter extension is not supported, remove the command tokens to
--         initiate EXT mesh tasks drawing in this extension.
--
--     -   This extension interacts with @VK_KHR_ray_tracing_pipeline@. If
--         the latter extension is not supported, remove the command tokens
--         to initiate ray tracing in this extension.
--
--     -   This extension interacts with @VK_EXT_shader_object@. If the
--         latter extension is not supported, remove references to shader
--         objects in this extension.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, VALVE
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Spencer Fricke, LunarG
--
--     -   Ricardo Garcia, Igalia
--
--     -   Tobias Hector, AMD
--
--     -   Baldur Karlsson, VALVE
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Lionel Landwerlin, INTEL
--
--     -   Jon Leech, Khronos
--
--     -   Ting Wei, ARM
--
--     -   Ken Shanyi Zhang, AMD
--
--     -   Faith Ekstrand, Collabora
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Connor Abbott, VALVE
--
--     -   Samuel Pitoiset, VALVE
--
-- == Description
--
-- This extension allows the device to generate a number of commands for
-- command buffers. It provides a subset of functionality from both
-- @VK_NV_device_generated_commands@ and
-- @VK_NV_device_generated_commands_compute@ as well as some new features.
--
-- When rendering a large number of objects, the device can be leveraged to
-- implement a number of critical functions, like updating matrices, or
-- implementing occlusion culling, frustum culling, front to back sorting,
-- etc. Implementing those on the device does not require any special
-- extension, since an application is free to define its own data
-- structures, and just process them using shaders.
--
-- To render objects which have been processed on the device, Vulkan has
-- several ways to perform indirect rendering, from the most basic
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect' with one indirect
-- draw to
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount'
-- which supports multiple indirect draws batched together, with a way to
-- determine number of draws at device execution time.
--
-- However, if rendering state needs to change between the indirect draws,
-- then unextended Vulkan forces the application to speculatively record a
-- prohibitive number of redundant indirect commands covering all possible
-- state combinations - which could end up processing nothing after culling
-- - or read back the processed stream and issue graphics command from the
-- host. For very large scenes, the synchronization overhead and cost to
-- generate the command buffer can become the bottleneck. This extension
-- allows an application to generate a device side stream of state changes
-- and commands, and convert it efficiently into a command buffer without
-- having to read it back to the host.
--
-- Furthermore, it allows incremental changes to such command buffers by
-- manipulating only partial sections of a command stream — for example
-- pipeline and shader object bindings. Unextended Vulkan requires
-- re-creation of entire command buffers in such a scenario, or updates
-- synchronized on the host.
--
-- The intended usage for this extension is for the application to:
--
-- -   create 'Vulkan.Core10.Handles.Buffer' objects and retrieve physical
--     addresses from them via
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   create a 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' for the
--     ability to change shaders on the device.
--
-- -   create a 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT',
--     which lists the 'IndirectCommandsTokenTypeEXT' it wants to
--     dynamically execute as an atomic command sequence. This step likely
--     involves some internal device code compilation, since the intent is
--     for the GPU to generate the command buffer based on the layout.
--
-- -   fill the input stream buffers with the data for each of the inputs
--     it needs. Each input is an array that will be filled with
--     token-dependent data.
--
-- -   set up a preprocess 'Vulkan.Core10.Handles.Buffer' that uses memory
--     according to the information retrieved via
--     'getGeneratedCommandsMemoryRequirementsEXT'.
--
-- -   optionally preprocess the generated content using
--     'cmdPreprocessGeneratedCommandsEXT', for example on an asynchronous
--     compute queue, or for the purpose of reusing the data in multiple
--     executions.
--
-- -   call 'cmdExecuteGeneratedCommandsEXT' to create and execute the
--     actual device commands for all sequences based on the inputs
--     provided.
--
-- For each draw in a sequence, the following can be specified:
--
-- -   a number of vertex buffer bindings
--
-- -   a different index buffer, with an optional dynamic offset and index
--     type
--
-- -   a number of different push constants
--
-- -   updates to bound shader stages
--
-- For each dispatch in a sequence, the following can be specified:
--
-- -   a number of different push constants
--
-- -   updates to bound shader stages
--
-- For each trace rays in a sequence, the following can be specified:
--
-- -   a number of different push constants
--
-- -   updates to bound shader stages
--
-- While the GPU can be faster than a CPU to generate the commands, it will
-- not happen asynchronously to the device, therefore the primary use case
-- is generating “less” total work (occlusion culling, classification to
-- use specialized shaders, etc.).
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT'
--
-- -   'Vulkan.Extensions.Handles.IndirectExecutionSetEXT'
--
-- == New Commands
--
-- -   'cmdExecuteGeneratedCommandsEXT'
--
-- -   'cmdPreprocessGeneratedCommandsEXT'
--
-- -   'createIndirectCommandsLayoutEXT'
--
-- -   'createIndirectExecutionSetEXT'
--
-- -   'destroyIndirectCommandsLayoutEXT'
--
-- -   'destroyIndirectExecutionSetEXT'
--
-- -   'getGeneratedCommandsMemoryRequirementsEXT'
--
-- -   'updateIndirectExecutionSetPipelineEXT'
--
-- -   'updateIndirectExecutionSetShaderEXT'
--
-- == New Structures
--
-- -   'BindIndexBufferIndirectCommandEXT'
--
-- -   'BindVertexBufferIndirectCommandEXT'
--
-- -   'DrawIndirectCountIndirectCommandEXT'
--
-- -   'GeneratedCommandsInfoEXT'
--
-- -   'GeneratedCommandsMemoryRequirementsInfoEXT'
--
-- -   'IndirectCommandsExecutionSetTokenEXT'
--
-- -   'IndirectCommandsIndexBufferTokenEXT'
--
-- -   'IndirectCommandsLayoutCreateInfoEXT'
--
-- -   'IndirectCommandsLayoutTokenEXT'
--
-- -   'IndirectCommandsPushConstantTokenEXT'
--
-- -   'IndirectCommandsVertexBufferTokenEXT'
--
-- -   'IndirectExecutionSetCreateInfoEXT'
--
-- -   'IndirectExecutionSetPipelineInfoEXT'
--
-- -   'IndirectExecutionSetShaderInfoEXT'
--
-- -   'IndirectExecutionSetShaderLayoutInfoEXT'
--
-- -   'WriteIndirectExecutionSetPipelineEXT'
--
-- -   Extending 'GeneratedCommandsInfoEXT',
--     'GeneratedCommandsMemoryRequirementsInfoEXT':
--
--     -   'GeneratedCommandsPipelineInfoEXT'
--
--     -   'GeneratedCommandsShaderInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   'WriteIndirectExecutionSetShaderEXT'
--
-- == New Unions
--
-- -   'IndirectCommandsTokenDataEXT'
--
-- -   'IndirectExecutionSetInfoEXT'
--
-- == New Enums
--
-- -   'IndirectCommandsInputModeFlagBitsEXT'
--
-- -   'IndirectCommandsLayoutUsageFlagBitsEXT'
--
-- -   'IndirectCommandsTokenTypeEXT'
--
-- -   'IndirectExecutionSetInfoTypeEXT'
--
-- == New Bitmasks
--
-- -   'IndirectCommandsInputModeFlagsEXT'
--
-- -   'IndirectCommandsLayoutUsageFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME'
--
-- -   'EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_EXT'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_INDIRECT_EXECUTION_SET_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDIRECT_BINDABLE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT'
--
-- == Example Code
--
-- TODO
--
-- == Version History
--
-- -   Revision 1, 2024-02-23 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_device_generated_commands Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_generated_commands  ( BindIndexBufferIndirectCommandEXT
                                                           , BindVertexBufferIndirectCommandEXT
                                                           , DrawIndirectCountIndirectCommandEXT
                                                           , GeneratedCommandsInfoEXT
                                                           , GeneratedCommandsMemoryRequirementsInfoEXT
                                                           , GeneratedCommandsPipelineInfoEXT
                                                           , GeneratedCommandsShaderInfoEXT
                                                           , IndirectCommandsExecutionSetTokenEXT
                                                           , IndirectCommandsIndexBufferTokenEXT
                                                           , IndirectCommandsLayoutCreateInfoEXT
                                                           , IndirectCommandsLayoutTokenEXT
                                                           , IndirectCommandsPushConstantTokenEXT
                                                           , IndirectCommandsVertexBufferTokenEXT
                                                           , IndirectExecutionSetCreateInfoEXT
                                                           , IndirectExecutionSetPipelineInfoEXT
                                                           , IndirectExecutionSetShaderInfoEXT
                                                           , IndirectExecutionSetShaderLayoutInfoEXT
                                                           , PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT
                                                           , PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT
                                                           , WriteIndirectExecutionSetPipelineEXT
                                                           , WriteIndirectExecutionSetShaderEXT
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BindIndexBufferIndirectCommandEXT

instance ToCStruct BindIndexBufferIndirectCommandEXT
instance Show BindIndexBufferIndirectCommandEXT

instance FromCStruct BindIndexBufferIndirectCommandEXT


data BindVertexBufferIndirectCommandEXT

instance ToCStruct BindVertexBufferIndirectCommandEXT
instance Show BindVertexBufferIndirectCommandEXT

instance FromCStruct BindVertexBufferIndirectCommandEXT


data DrawIndirectCountIndirectCommandEXT

instance ToCStruct DrawIndirectCountIndirectCommandEXT
instance Show DrawIndirectCountIndirectCommandEXT

instance FromCStruct DrawIndirectCountIndirectCommandEXT


type role GeneratedCommandsInfoEXT nominal
data GeneratedCommandsInfoEXT (es :: [Type])

instance ( Extendss GeneratedCommandsInfoEXT es
         , PokeChain es ) => ToCStruct (GeneratedCommandsInfoEXT es)
instance Show (Chain es) => Show (GeneratedCommandsInfoEXT es)

instance ( Extendss GeneratedCommandsInfoEXT es
         , PeekChain es ) => FromCStruct (GeneratedCommandsInfoEXT es)


type role GeneratedCommandsMemoryRequirementsInfoEXT nominal
data GeneratedCommandsMemoryRequirementsInfoEXT (es :: [Type])

instance ( Extendss GeneratedCommandsMemoryRequirementsInfoEXT es
         , PokeChain es ) => ToCStruct (GeneratedCommandsMemoryRequirementsInfoEXT es)
instance Show (Chain es) => Show (GeneratedCommandsMemoryRequirementsInfoEXT es)

instance ( Extendss GeneratedCommandsMemoryRequirementsInfoEXT es
         , PeekChain es ) => FromCStruct (GeneratedCommandsMemoryRequirementsInfoEXT es)


data GeneratedCommandsPipelineInfoEXT

instance ToCStruct GeneratedCommandsPipelineInfoEXT
instance Show GeneratedCommandsPipelineInfoEXT

instance FromCStruct GeneratedCommandsPipelineInfoEXT


data GeneratedCommandsShaderInfoEXT

instance ToCStruct GeneratedCommandsShaderInfoEXT
instance Show GeneratedCommandsShaderInfoEXT

instance FromCStruct GeneratedCommandsShaderInfoEXT


data IndirectCommandsExecutionSetTokenEXT

instance ToCStruct IndirectCommandsExecutionSetTokenEXT
instance Show IndirectCommandsExecutionSetTokenEXT

instance FromCStruct IndirectCommandsExecutionSetTokenEXT


data IndirectCommandsIndexBufferTokenEXT

instance ToCStruct IndirectCommandsIndexBufferTokenEXT
instance Show IndirectCommandsIndexBufferTokenEXT

instance FromCStruct IndirectCommandsIndexBufferTokenEXT


type role IndirectCommandsLayoutCreateInfoEXT nominal
data IndirectCommandsLayoutCreateInfoEXT (es :: [Type])

instance ( Extendss IndirectCommandsLayoutCreateInfoEXT es
         , PokeChain es ) => ToCStruct (IndirectCommandsLayoutCreateInfoEXT es)
instance Show (Chain es) => Show (IndirectCommandsLayoutCreateInfoEXT es)


type role IndirectCommandsLayoutTokenEXT nominal
data IndirectCommandsLayoutTokenEXT (es :: [Type])

instance ( Extendss IndirectCommandsLayoutTokenEXT es
         , PokeChain es ) => ToCStruct (IndirectCommandsLayoutTokenEXT es)
instance Show (Chain es) => Show (IndirectCommandsLayoutTokenEXT es)


data IndirectCommandsPushConstantTokenEXT

instance ToCStruct IndirectCommandsPushConstantTokenEXT
instance Show IndirectCommandsPushConstantTokenEXT

instance FromCStruct IndirectCommandsPushConstantTokenEXT


data IndirectCommandsVertexBufferTokenEXT

instance ToCStruct IndirectCommandsVertexBufferTokenEXT
instance Show IndirectCommandsVertexBufferTokenEXT

instance FromCStruct IndirectCommandsVertexBufferTokenEXT


data IndirectExecutionSetCreateInfoEXT

instance ToCStruct IndirectExecutionSetCreateInfoEXT
instance Show IndirectExecutionSetCreateInfoEXT


data IndirectExecutionSetPipelineInfoEXT

instance ToCStruct IndirectExecutionSetPipelineInfoEXT
instance Show IndirectExecutionSetPipelineInfoEXT

instance FromCStruct IndirectExecutionSetPipelineInfoEXT


data IndirectExecutionSetShaderInfoEXT

instance ToCStruct IndirectExecutionSetShaderInfoEXT
instance Show IndirectExecutionSetShaderInfoEXT

instance FromCStruct IndirectExecutionSetShaderInfoEXT


data IndirectExecutionSetShaderLayoutInfoEXT

instance ToCStruct IndirectExecutionSetShaderLayoutInfoEXT
instance Show IndirectExecutionSetShaderLayoutInfoEXT

instance FromCStruct IndirectExecutionSetShaderLayoutInfoEXT


data PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT
instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT


data PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT
instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT


data WriteIndirectExecutionSetPipelineEXT

instance ToCStruct WriteIndirectExecutionSetPipelineEXT
instance Show WriteIndirectExecutionSetPipelineEXT

instance FromCStruct WriteIndirectExecutionSetPipelineEXT


data WriteIndirectExecutionSetShaderEXT

instance ToCStruct WriteIndirectExecutionSetShaderEXT
instance Show WriteIndirectExecutionSetShaderEXT

instance FromCStruct WriteIndirectExecutionSetShaderEXT

