{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_generated_commands - device extension
--
-- == VK_NV_device_generated_commands
--
-- [__Name String__]
--     @VK_NV_device_generated_commands@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     278
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_device_generated_commands:%20&body=@pixeljetstream%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-02-20
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires Vulkan 1.1
--
--     -   This extension requires @VK_EXT_buffer_device_address@ or
--         @VK_KHR_buffer_device_address@ or Vulkan 1.2 for the ability to
--         bind vertex and index buffers on the device.
--
--     -   This extension interacts with @VK_NV_mesh_shader@. If the latter
--         extension is not supported, remove the command token to initiate
--         mesh tasks drawing in this extension.
--
-- [__Contributors__]
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Pierre Boudier, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Baldur Karlsson, Valve
--
--     -   Mathias Schott, NVIDIA
--
--     -   Tyson Smith, NVIDIA
--
--     -   Ingo Esser, NVIDIA
--
-- == Description
--
-- This extension allows the device to generate a number of critical
-- graphics commands for command buffers.
--
-- When rendering a large number of objects, the device can be leveraged to
-- implement a number of critical functions, like updating matrices, or
-- implementing occlusion culling, frustum culling, front to back sorting,
-- etc. Implementing those on the device does not require any special
-- extension, since an application is free to define its own data
-- structures, and just process them using shaders.
--
-- However, if the application desires to quickly kick off the rendering of
-- the final stream of objects, then unextended Vulkan forces the
-- application to read back the processed stream and issue graphics command
-- from the host. For very large scenes, the synchronization overhead and
-- cost to generate the command buffer can become the bottleneck. This
-- extension allows an application to generate a device side stream of
-- state changes and commands, and convert it efficiently into a command
-- buffer without having to read it back to the host.
--
-- Furthermore, it allows incremental changes to such command buffers by
-- manipulating only partial sections of a command stream — for example
-- pipeline bindings. Unextended Vulkan requires re-creation of entire
-- command buffers in such a scenario, or updates synchronized on the host.
--
-- The intended usage for this extension is for the application to:
--
-- -   create 'Vulkan.Core10.Handles.Buffer' objects and retrieve physical
--     addresses from them via
--     'Vulkan.Extensions.VK_EXT_buffer_device_address.getBufferDeviceAddressEXT'
--
-- -   create a graphics pipeline using
--     'GraphicsPipelineShaderGroupsCreateInfoNV' for the ability to change
--     shaders on the device.
--
-- -   create a 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV', which
--     lists the 'IndirectCommandsTokenTypeNV' it wants to dynamically
--     execute as an atomic command sequence. This step likely involves
--     some internal device code compilation, since the intent is for the
--     GPU to generate the command buffer in the pipeline.
--
-- -   fill the input stream buffers with the data for each of the inputs
--     it needs. Each input is an array that will be filled with
--     token-dependent data.
--
-- -   set up a preprocess 'Vulkan.Core10.Handles.Buffer' that uses memory
--     according to the information retrieved via
--     'getGeneratedCommandsMemoryRequirementsNV'.
--
-- -   optionally preprocess the generated content using
--     'cmdPreprocessGeneratedCommandsNV', for example on an asynchronous
--     compute queue, or for the purpose of re-using the data in multiple
--     executions.
--
-- -   call 'cmdExecuteGeneratedCommandsNV' to create and execute the
--     actual device commands for all sequences based on the inputs
--     provided.
--
-- For each draw in a sequence, the following can be specified:
--
-- -   a different shader group
--
-- -   a number of vertex buffer bindings
--
-- -   a different index buffer, with an optional dynamic offset and index
--     type
--
-- -   a number of different push constants
--
-- -   a flag that encodes the primitive winding
--
-- While the GPU can be faster than a CPU to generate the commands, it will
-- not happen asynchronously to the device, therefore the primary use-case
-- is generating “less” total work (occlusion culling, classification to
-- use specialized shaders, etc.).
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
--
-- == New Commands
--
-- -   'cmdBindPipelineShaderGroupNV'
--
-- -   'cmdExecuteGeneratedCommandsNV'
--
-- -   'cmdPreprocessGeneratedCommandsNV'
--
-- -   'createIndirectCommandsLayoutNV'
--
-- -   'destroyIndirectCommandsLayoutNV'
--
-- -   'getGeneratedCommandsMemoryRequirementsNV'
--
-- == New Structures
--
-- -   'BindIndexBufferIndirectCommandNV'
--
-- -   'BindShaderGroupIndirectCommandNV'
--
-- -   'BindVertexBufferIndirectCommandNV'
--
-- -   'GeneratedCommandsInfoNV'
--
-- -   'GeneratedCommandsMemoryRequirementsInfoNV'
--
-- -   'GraphicsShaderGroupCreateInfoNV'
--
-- -   'IndirectCommandsLayoutCreateInfoNV'
--
-- -   'IndirectCommandsLayoutTokenNV'
--
-- -   'IndirectCommandsStreamNV'
--
-- -   'SetStateFlagsIndirectCommandNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'GraphicsPipelineShaderGroupsCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'
--
-- == New Enums
--
-- -   'IndirectCommandsLayoutUsageFlagBitsNV'
--
-- -   'IndirectCommandsTokenTypeNV'
--
-- -   'IndirectStateFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'IndirectCommandsLayoutUsageFlagsNV'
--
-- -   'IndirectStateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME'
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV'
--
-- == Issues
--
-- 1) How to name this extension ?
--
-- @VK_NV_device_generated_commands@
--
-- As usual, one of the hardest issues ;)
--
-- Alternatives: @VK_gpu_commands@, @VK_execute_commands@,
-- @VK_device_commands@, @VK_device_execute_commands@, @VK_device_execute@,
-- @VK_device_created_commands@, @VK_device_recorded_commands@,
-- @VK_device_generated_commands@ @VK_indirect_generated_commands@
--
-- 2) Should we use a serial stateful token stream or stateless sequence
-- descriptions?
--
-- Similarly to 'Vulkan.Core10.Handles.Pipeline', fixed layouts have the
-- most likelihood to be cross-vendor adoptable. They also benefit from
-- being processable in parallel. This is a different design choice
-- compared to the serial command stream generated through
-- @GL_NV_command_list@.
--
-- 3) How to name a sequence description?
--
-- @VkIndirectCommandsLayout@ as in the NVX extension predecessor.
--
-- Alternative: @VkGeneratedCommandsLayout@
--
-- 4) Do we want to provide @indirectCommands@ inputs with layout or at
-- @indirectCommands@ time?
--
-- Separate layout from data as Vulkan does. Provide full flexibility for
-- @indirectCommands@.
--
-- 5) Should the input be provided as SoA or AoS?
--
-- Both ways are desireable. AoS can provide portability to other APIs and
-- easier to setup, while SoA allows to update individual inputs in a
-- cache-efficient manner, when others remain static.
--
-- 6) How do we make developers aware of the memory requirements of
-- implementation-dependent data used for the generated commands?
--
-- Make the API explicit and introduce a @preprocess@
-- 'Vulkan.Core10.Handles.Buffer'. Developers have to allocate it using
-- 'getGeneratedCommandsMemoryRequirementsNV'.
--
-- In the NVX version the requirements were hidden implicitly as part of
-- the command buffer reservation process, however as the memory
-- requirements can be substantial, we want to give developers the ability
-- to budget the memory themselves. By lowering the @maxSequencesCount@ the
-- memory consumption can be reduced. Furthermore re-use of the memory is
-- possible, for example for doing explicit preprocessing and execution in
-- a ping-pong fashion.
--
-- The actual buffer size is implementation dependent and may be zero, i.e.
-- not always required.
--
-- When making use of Graphics Shader Groups, the programs should behave
-- similar with regards to vertex inputs, clipping and culling outputs of
-- the geometry stage, as well as sample shading behavior in fragment
-- shaders, to reduce the amount of the worst-case memory approximation.
--
-- 7) Should we allow additional per-sequence dynamic state changes?
--
-- Yes
--
-- Introduced a lightweight indirect state flag 'IndirectStateFlagBitsNV'.
-- So far only switching front face winding state is exposed. Especially in
-- CAD\/DCC mirrored transforms that require such changes are common, and
-- similar flexibility is given in the ray tracing instance description.
--
-- The flag could be extended further, for example to switch between
-- primitive-lists or -strips, or make other state modifications.
--
-- Furthermore, as new tokens can be added easily, future extension could
-- add the ability to change any
-- 'Vulkan.Core10.Enums.DynamicState.DynamicState'.
--
-- 8) How do we allow re-using already “generated” @indirectCommands@?
--
-- Expose a @preprocessBuffer@ to re-use implementation-dependencyFlags
-- data. Set the @isPreprocessed@ to true in
-- 'cmdExecuteGeneratedCommandsNV'.
--
-- 9) Under which conditions is 'cmdExecuteGeneratedCommandsNV' legal?
--
-- It behaves like a regular draw call command.
--
-- 10) Is 'cmdPreprocessGeneratedCommandsNV' copying the input data or
-- referencing it?
--
-- There are multiple implementations possible:
--
-- -   one could have some emulation code that parses the inputs, and
--     generates an output command buffer, therefore copying the inputs.
--
-- -   one could just reference the inputs, and have the processing done in
--     pipe at execution time.
--
-- If the data is mandated to be copied, then it puts a penalty on
-- implementation that could process the inputs directly in pipe. If the
-- data is “referenced”, then it allows both types of implementation.
--
-- The inputs are “referenced”, and /must/ not be modified after the call
-- to 'cmdExecuteGeneratedCommandsNV' has completed.
--
-- 11) Which buffer usage flags are required for the buffers referenced by
-- 'GeneratedCommandsInfoNV' ?
--
-- Reuse existing
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--
-- -   'GeneratedCommandsInfoNV'::@preprocessBuffer@
--
-- -   'GeneratedCommandsInfoNV'::@sequencesCountBuffer@
--
-- -   'GeneratedCommandsInfoNV'::@sequencesIndexBuffer@
--
-- -   'IndirectCommandsStreamNV'::@buffer@
--
-- 12) In which pipeline stage does the device generated command expansion
-- happen?
--
-- 'cmdPreprocessGeneratedCommandsNV' is treated as if it occurs in a
-- separate logical pipeline from either graphics or compute, and that
-- pipeline only includes
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TOP_OF_PIPE_BIT',
-- a new stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV',
-- and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'.
-- This new stage has two corresponding new access types,
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
-- and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV',
-- used to synchronize reading the buffer inputs and writing the preprocess
-- memory output.
--
-- The generated output written in the preprocess buffer memory by
-- 'cmdExecuteGeneratedCommandsNV' is considered to be consumed by the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'
-- pipeline stage.
--
-- Thus, to synchronize from writing the input buffers to preprocessing via
-- 'cmdPreprocessGeneratedCommandsNV', use:
--
-- -   @dstStageMask@ =
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--
-- -   @dstAccessMask@ =
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
--
-- To synchronize from 'cmdPreprocessGeneratedCommandsNV' to executing the
-- generated commands by 'cmdExecuteGeneratedCommandsNV', use:
--
-- -   @srcStageMask@ =
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--
-- -   @srcAccessMask@ =
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV'
--
-- -   @dstStageMask@ =
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'
--
-- -   @dstAccessMask@ =
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_INDIRECT_COMMAND_READ_BIT'
--
-- When 'cmdExecuteGeneratedCommandsNV' is used with a @isPreprocessed@ of
-- 'Vulkan.Core10.FundamentalTypes.FALSE', the generated commands are
-- implicitly preprocessed, therefore one only needs to synchronize the
-- inputs via:
--
-- -   @dstStageMask@ =
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'
--
-- -   @dstAccessMask@ =
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_INDIRECT_COMMAND_READ_BIT'
--
-- 13) What if most token data is “static”, but we frequently want to
-- render a subsection?
--
-- Added “sequencesIndexBuffer”. This allows to easier sort and filter what
-- should actually be executed.
--
-- 14) What are the changes compared to the previous NVX extension?
--
-- -   Compute dispatch support was removed (was never implemented in
--     drivers). There are different approaches how dispatching from the
--     device should work, hence we defer this to a future extension.
--
-- -   The @ObjectTableNVX@ was replaced by using physical buffer addresses
--     and introducing Shader Groups for the graphics pipeline.
--
-- -   Less state changes are possible overall, but the important
--     operations are still there (reduces complexity of implementation).
--
-- -   The API was redesigned so all inputs must be passed at both
--     preprocessing and execution time (this was implicit in NVX, now it
--     is explicit)
--
-- -   The reservation of intermediate command space is now mandatory and
--     explicit through a preprocess buffer.
--
-- -   The 'IndirectStateFlagBitsNV' were introduced
--
-- 15) When porting from other APIs, their indirect buffers may use
-- different enums, for example for index buffer types. How to solve this?
--
-- Added “pIndexTypeValues” to map custom @uint32_t@ values to
-- corresponding 'Vulkan.Core10.Enums.IndexType.IndexType'.
--
-- 16) Do we need more shader group state overrides?
--
-- The NVX version allowed all PSO states to be different, however as the
-- goal is not to replace all state setup, but focus on highly-frequent
-- state changes for drawing lots of objects, we reduced the amount of
-- state overrides. Especially VkPipelineLayout as well as VkRenderPass
-- configuration should be left static, the rest is still open for
-- discussion.
--
-- The current focus is just to allow VertexInput changes as well as
-- shaders, while all shader groups use the same shader stages.
--
-- Too much flexibility will increase the test coverage requirement as
-- well. However, further extensions could allow more dynamic state as
-- well.
--
-- 17) Do we need more detailed physical device feature queries\/enables?
--
-- An EXT version would need detailed implementor feedback to come up with
-- a good set of features. Please contact us if you are interested, we are
-- happy to make more features optional, or add further restrictions to
-- reduce the minimum feature set of an EXT.
--
-- 18) Is there an interaction with VK_KHR_pipeline_library planned?
--
-- Yes, a future version of this extension will detail the interaction,
-- once VK_KHR_pipeline_library is no longer provisional.
--
-- == Example Code
--
-- Open-Source samples illustrating the usage of the extension can be found
-- at the following location (may not yet exist at time of writing):
--
-- <https://github.com/nvpro-samples/vk_device_generated_cmds>
--
-- == Version History
--
-- -   Revision 1, 2020-02-20 (Christoph Kubisch)
--
--     -   Initial version
--
-- = See Also
--
-- 'BindIndexBufferIndirectCommandNV', 'BindShaderGroupIndirectCommandNV',
-- 'BindVertexBufferIndirectCommandNV', 'GeneratedCommandsInfoNV',
-- 'GeneratedCommandsMemoryRequirementsInfoNV',
-- 'GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'GraphicsShaderGroupCreateInfoNV', 'IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV',
-- 'IndirectCommandsLayoutTokenNV',
-- 'IndirectCommandsLayoutUsageFlagBitsNV',
-- 'IndirectCommandsLayoutUsageFlagsNV', 'IndirectCommandsStreamNV',
-- 'IndirectCommandsTokenTypeNV', 'IndirectStateFlagBitsNV',
-- 'IndirectStateFlagsNV',
-- 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
-- 'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV',
-- 'SetStateFlagsIndirectCommandNV', 'cmdBindPipelineShaderGroupNV',
-- 'cmdExecuteGeneratedCommandsNV', 'cmdPreprocessGeneratedCommandsNV',
-- 'createIndirectCommandsLayoutNV', 'destroyIndirectCommandsLayoutNV',
-- 'getGeneratedCommandsMemoryRequirementsNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_generated_commands  ( BindIndexBufferIndirectCommandNV
                                                          , BindShaderGroupIndirectCommandNV
                                                          , BindVertexBufferIndirectCommandNV
                                                          , GeneratedCommandsInfoNV
                                                          , GeneratedCommandsMemoryRequirementsInfoNV
                                                          , GraphicsPipelineShaderGroupsCreateInfoNV
                                                          , GraphicsShaderGroupCreateInfoNV
                                                          , IndirectCommandsLayoutCreateInfoNV
                                                          , IndirectCommandsLayoutTokenNV
                                                          , IndirectCommandsStreamNV
                                                          , PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
                                                          , PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
                                                          , SetStateFlagsIndirectCommandNV
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BindIndexBufferIndirectCommandNV

instance ToCStruct BindIndexBufferIndirectCommandNV
instance Show BindIndexBufferIndirectCommandNV

instance FromCStruct BindIndexBufferIndirectCommandNV


data BindShaderGroupIndirectCommandNV

instance ToCStruct BindShaderGroupIndirectCommandNV
instance Show BindShaderGroupIndirectCommandNV

instance FromCStruct BindShaderGroupIndirectCommandNV


data BindVertexBufferIndirectCommandNV

instance ToCStruct BindVertexBufferIndirectCommandNV
instance Show BindVertexBufferIndirectCommandNV

instance FromCStruct BindVertexBufferIndirectCommandNV


data GeneratedCommandsInfoNV

instance ToCStruct GeneratedCommandsInfoNV
instance Show GeneratedCommandsInfoNV

instance FromCStruct GeneratedCommandsInfoNV


data GeneratedCommandsMemoryRequirementsInfoNV

instance ToCStruct GeneratedCommandsMemoryRequirementsInfoNV
instance Show GeneratedCommandsMemoryRequirementsInfoNV

instance FromCStruct GeneratedCommandsMemoryRequirementsInfoNV


data GraphicsPipelineShaderGroupsCreateInfoNV

instance ToCStruct GraphicsPipelineShaderGroupsCreateInfoNV
instance Show GraphicsPipelineShaderGroupsCreateInfoNV

instance FromCStruct GraphicsPipelineShaderGroupsCreateInfoNV


data GraphicsShaderGroupCreateInfoNV

instance ToCStruct GraphicsShaderGroupCreateInfoNV
instance Show GraphicsShaderGroupCreateInfoNV

instance FromCStruct GraphicsShaderGroupCreateInfoNV


data IndirectCommandsLayoutCreateInfoNV

instance ToCStruct IndirectCommandsLayoutCreateInfoNV
instance Show IndirectCommandsLayoutCreateInfoNV

instance FromCStruct IndirectCommandsLayoutCreateInfoNV


data IndirectCommandsLayoutTokenNV

instance ToCStruct IndirectCommandsLayoutTokenNV
instance Show IndirectCommandsLayoutTokenNV

instance FromCStruct IndirectCommandsLayoutTokenNV


data IndirectCommandsStreamNV

instance ToCStruct IndirectCommandsStreamNV
instance Show IndirectCommandsStreamNV

instance FromCStruct IndirectCommandsStreamNV


data PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV


data PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV


data SetStateFlagsIndirectCommandNV

instance ToCStruct SetStateFlagsIndirectCommandNV
instance Show SetStateFlagsIndirectCommandNV

instance FromCStruct SetStateFlagsIndirectCommandNV

