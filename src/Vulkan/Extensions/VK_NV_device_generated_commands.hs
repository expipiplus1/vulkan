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
module Vulkan.Extensions.VK_NV_device_generated_commands  ( cmdExecuteGeneratedCommandsNV
                                                          , cmdPreprocessGeneratedCommandsNV
                                                          , cmdBindPipelineShaderGroupNV
                                                          , getGeneratedCommandsMemoryRequirementsNV
                                                          , createIndirectCommandsLayoutNV
                                                          , withIndirectCommandsLayoutNV
                                                          , destroyIndirectCommandsLayoutNV
                                                          , PhysicalDeviceDeviceGeneratedCommandsFeaturesNV(..)
                                                          , PhysicalDeviceDeviceGeneratedCommandsPropertiesNV(..)
                                                          , GraphicsShaderGroupCreateInfoNV(..)
                                                          , GraphicsPipelineShaderGroupsCreateInfoNV(..)
                                                          , BindShaderGroupIndirectCommandNV(..)
                                                          , BindIndexBufferIndirectCommandNV(..)
                                                          , BindVertexBufferIndirectCommandNV(..)
                                                          , SetStateFlagsIndirectCommandNV(..)
                                                          , IndirectCommandsStreamNV(..)
                                                          , IndirectCommandsLayoutTokenNV(..)
                                                          , IndirectCommandsLayoutCreateInfoNV(..)
                                                          , GeneratedCommandsInfoNV(..)
                                                          , GeneratedCommandsMemoryRequirementsInfoNV(..)
                                                          , IndirectCommandsLayoutUsageFlagsNV
                                                          , IndirectCommandsLayoutUsageFlagBitsNV( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
                                                                                                 , ..
                                                                                                 )
                                                          , IndirectStateFlagsNV
                                                          , IndirectStateFlagBitsNV( INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV
                                                                                   , ..
                                                                                   )
                                                          , IndirectCommandsTokenTypeNV( INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
                                                                                       , ..
                                                                                       )
                                                          , NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , IndirectCommandsLayoutNV(..)
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipelineShaderGroupNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPreprocessGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetGeneratedCommandsMemoryRequirementsNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.Core10.Pipeline (PipelineTessellationStateCreateInfo)
import Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()

-- | vkCmdExecuteGeneratedCommandsNV - Performs the generation and execution
-- of commands on the device
--
-- == Valid Usage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02697# For each set /n/
--     that is statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command, a descriptor
--     set /must/ have been bound to /n/ at the same pipeline bind point,
--     with a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for
--     set /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02698# For each push
--     constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02699# Descriptors in
--     each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02700# A valid pipeline
--     /must/ be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02859# There /must/ not
--     have been any calls to dynamic state setting commands for any state
--     not specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command, since
--     that pipeline was bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-renderPass-02684# The current
--     render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-subpass-02685# The subpass
--     index of the current render pass /must/ be equal to the @subpass@
--     member of the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Vulkan.Core10.Handles.Pipeline' bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02686# Every input
--     attachment used by the current subpass /must/ be bound to the
--     pipeline via a descriptor set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04584# Image subresources
--     used as attachments in the current render pass /must/ not be
--     accessed in any way other than as an attachment by this command,
--     except for cases involving read-only access to depth\/stencil
--     attachments as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-02689#
--     If the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-03417# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-scissorCount-03418# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-03419# If the
--     bound graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04137# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04138# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04139# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04140# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveTopology-03420# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04007# All vertex input
--     bindings accessed via vertex input variables declared in the vertex
--     shader entry point’s interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02721# For a given vertex
--     buffer binding, any attribute data fetched /must/ be entirely
--     contained within the corresponding vertex buffer binding, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-02970#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-isPreprocessed-02908# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then
--     'cmdPreprocessGeneratedCommandsNV' /must/ have already been executed
--     on the device, using the same @pGeneratedCommandsInfo@ content as
--     well as the content of the input buffers it references (all except
--     'GeneratedCommandsInfoNV'::@preprocessBuffer@). Furthermore
--     @pGeneratedCommandsInfo@\`s @indirectCommandsLayout@ /must/ have
--     been created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pipeline-02909#
--     'GeneratedCommandsInfoNV'::@pipeline@ /must/ match the current bound
--     pipeline at 'GeneratedCommandsInfoNV'::@pipelineBindPoint@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02910# Transform feedback
--     /must/ not be active
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-deviceGeneratedCommands-02911#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pGeneratedCommandsInfo-parameter#
--     @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoNV' structure
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-renderpass# This command
--     /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoNV'
cmdExecuteGeneratedCommandsNV :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command is
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @isPreprocessed@ represents whether the input data has already been
                                 -- preprocessed on the device. If it is
                                 -- 'Vulkan.Core10.FundamentalTypes.FALSE' this command will implicitly
                                 -- trigger the preprocessing step, otherwise not.
                                 ("isPreprocessed" ::: Bool)
                              -> -- | @pGeneratedCommandsInfo@ is a pointer to an instance of the
                                 -- 'GeneratedCommandsInfoNV' structure containing parameters affecting the
                                 -- generation of commands.
                                 GeneratedCommandsInfoNV
                              -> io ()
cmdExecuteGeneratedCommandsNV commandBuffer isPreprocessed generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdExecuteGeneratedCommandsNVPtr = pVkCmdExecuteGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdExecuteGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdExecuteGeneratedCommandsNV' = mkVkCmdExecuteGeneratedCommandsNV vkCmdExecuteGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdExecuteGeneratedCommandsNV" (vkCmdExecuteGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) (boolToBool32 (isPreprocessed)) pGeneratedCommandsInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPreprocessGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()

-- | vkCmdPreprocessGeneratedCommandsNV - Performs preprocessing for
-- generated commands
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-commandBuffer-02974#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-pGeneratedCommandsInfo-02927#
--     @pGeneratedCommandsInfo@\`s @indirectCommandsLayout@ /must/ have
--     been created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' bit set
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-deviceGeneratedCommands-02928#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-pGeneratedCommandsInfo-parameter#
--     @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoNV' structure
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoNV'
cmdPreprocessGeneratedCommandsNV :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer which does the preprocessing.
                                    CommandBuffer
                                 -> -- | @pGeneratedCommandsInfo@ is a pointer to an instance of the
                                    -- 'GeneratedCommandsInfoNV' structure containing parameters affecting the
                                    -- preprocessing step.
                                    GeneratedCommandsInfoNV
                                 -> io ()
cmdPreprocessGeneratedCommandsNV commandBuffer generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdPreprocessGeneratedCommandsNVPtr = pVkCmdPreprocessGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPreprocessGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPreprocessGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdPreprocessGeneratedCommandsNV' = mkVkCmdPreprocessGeneratedCommandsNV vkCmdPreprocessGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdPreprocessGeneratedCommandsNV" (vkCmdPreprocessGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) pGeneratedCommandsInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipelineShaderGroupNV
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()

-- | vkCmdBindPipelineShaderGroupNV - Bind a pipeline object
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-groupIndex-02893# @groupIndex@
--     /must/ be @0@ or less than the effective
--     'GraphicsPipelineShaderGroupsCreateInfoNV'::@groupCount@ including
--     the referenced pipelines
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-pipelineBindPoint-02894# The
--     @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-groupIndex-02895# The same
--     restrictions as
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline' apply as if
--     the bound pipeline was created only with the Shader Group from the
--     @groupIndex@ information
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-deviceGeneratedCommands-02896#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-pipeline-parameter# @pipeline@
--     /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-commonparent# Both of
--     @commandBuffer@, and @pipeline@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
cmdBindPipelineShaderGroupNV :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer that the pipeline will be bound
                                -- to.
                                CommandBuffer
                             -> -- | @pipelineBindPoint@ is a
                                -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
                                -- specifying the bind point to which the pipeline will be bound.
                                PipelineBindPoint
                             -> -- | @pipeline@ is the pipeline to be bound.
                                Pipeline
                             -> -- | @groupIndex@ is the shader group to be bound.
                                ("groupIndex" ::: Word32)
                             -> io ()
cmdBindPipelineShaderGroupNV commandBuffer pipelineBindPoint pipeline groupIndex = liftIO $ do
  let vkCmdBindPipelineShaderGroupNVPtr = pVkCmdBindPipelineShaderGroupNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindPipelineShaderGroupNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipelineShaderGroupNV is null" Nothing Nothing
  let vkCmdBindPipelineShaderGroupNV' = mkVkCmdBindPipelineShaderGroupNV vkCmdBindPipelineShaderGroupNVPtr
  traceAroundEvent "vkCmdBindPipelineShaderGroupNV" (vkCmdBindPipelineShaderGroupNV' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline) (groupIndex))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGeneratedCommandsMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetGeneratedCommandsMemoryRequirementsNV - Retrieve the buffer
-- allocation requirements for generated commands
--
-- == Valid Usage
--
-- -   #VUID-vkGetGeneratedCommandsMemoryRequirementsNV-deviceGeneratedCommands-02906#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetGeneratedCommandsMemoryRequirementsNV-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetGeneratedCommandsMemoryRequirementsNV-pInfo-parameter#
--     @pInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsMemoryRequirementsInfoNV' structure
--
-- -   #VUID-vkGetGeneratedCommandsMemoryRequirementsNV-pMemoryRequirements-parameter#
--     @pMemoryRequirements@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getGeneratedCommandsMemoryRequirementsNV :: forall a io
                                          . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                         => -- | @device@ is the logical device that owns the buffer.
                                            Device
                                         -> -- | @pInfo@ is a pointer to an instance of the
                                            -- 'GeneratedCommandsMemoryRequirementsInfoNV' structure containing
                                            -- parameters required for the memory requirements query.
                                            GeneratedCommandsMemoryRequirementsInfoNV
                                         -> io (MemoryRequirements2 a)
getGeneratedCommandsMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetGeneratedCommandsMemoryRequirementsNVPtr = pVkGetGeneratedCommandsMemoryRequirementsNV (deviceCmds (device :: Device))
  lift $ unless (vkGetGeneratedCommandsMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGeneratedCommandsMemoryRequirementsNV is null" Nothing Nothing
  let vkGetGeneratedCommandsMemoryRequirementsNV' = mkVkGetGeneratedCommandsMemoryRequirementsNV vkGetGeneratedCommandsMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetGeneratedCommandsMemoryRequirementsNV" (vkGetGeneratedCommandsMemoryRequirementsNV' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result) -> Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result

-- | vkCreateIndirectCommandsLayoutNV - Create an indirect command layout
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-deviceGeneratedCommands-02929#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'IndirectCommandsLayoutCreateInfoNV' structure
--
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-pIndirectCommandsLayout-parameter#
--     @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
createIndirectCommandsLayoutNV :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the indirect command layout.
                                  Device
                               -> -- | @pCreateInfo@ is a pointer to an instance of the
                                  -- 'IndirectCommandsLayoutCreateInfoNV' structure containing parameters
                                  -- affecting creation of the indirect command layout.
                                  IndirectCommandsLayoutCreateInfoNV
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (IndirectCommandsLayoutNV)
createIndirectCommandsLayoutNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutNVPtr = pVkCreateIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkCreateIndirectCommandsLayoutNV' = mkVkCreateIndirectCommandsLayoutNV vkCreateIndirectCommandsLayoutNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutNV 8) free
  r <- lift $ traceAroundEvent "vkCreateIndirectCommandsLayoutNV" (vkCreateIndirectCommandsLayoutNV' (deviceHandle (device)) pCreateInfo pAllocator (pPIndirectCommandsLayout))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectCommandsLayout <- lift $ peek @IndirectCommandsLayoutNV pPIndirectCommandsLayout
  pure $ (pIndirectCommandsLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createIndirectCommandsLayoutNV' and 'destroyIndirectCommandsLayoutNV'
--
-- To ensure that 'destroyIndirectCommandsLayoutNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withIndirectCommandsLayoutNV :: forall io r . MonadIO io => Device -> IndirectCommandsLayoutCreateInfoNV -> Maybe AllocationCallbacks -> (io IndirectCommandsLayoutNV -> (IndirectCommandsLayoutNV -> io ()) -> r) -> r
withIndirectCommandsLayoutNV device pCreateInfo pAllocator b =
  b (createIndirectCommandsLayoutNV device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectCommandsLayoutNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyIndirectCommandsLayoutNV - Destroy an indirect commands layout
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-indirectCommandsLayout-02938#
--     All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-indirectCommandsLayout-02939#
--     If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-indirectCommandsLayout-02940#
--     If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-deviceGeneratedCommands-02941#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-indirectCommandsLayout-parameter#
--     If @indirectCommandsLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectCommandsLayout@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutNV-indirectCommandsLayout-parent#
--     If @indirectCommandsLayout@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @indirectCommandsLayout@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
destroyIndirectCommandsLayoutNV :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the logical device that destroys the layout.
                                   Device
                                -> -- | @indirectCommandsLayout@ is the layout to destroy.
                                   IndirectCommandsLayoutNV
                                -> -- | @pAllocator@ controls host memory allocation as described in the
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyIndirectCommandsLayoutNV device indirectCommandsLayout allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutNVPtr = pVkDestroyIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkDestroyIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkDestroyIndirectCommandsLayoutNV' = mkVkDestroyIndirectCommandsLayoutNV vkDestroyIndirectCommandsLayoutNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyIndirectCommandsLayoutNV" (vkDestroyIndirectCommandsLayoutNV' (deviceHandle (device)) (indirectCommandsLayout) pAllocator)
  pure $ ()


-- | VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV - Structure describing
-- the device-generated commands features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- the features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
  { -- | #features-deviceGeneratedCommands# @deviceGeneratedCommands@ indicates
    -- whether the implementation supports functionality to generate commands
    -- on the device. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedCommands :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommands))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  peekCStruct p = do
    deviceGeneratedCommands <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
             (bool32ToBool deviceGeneratedCommands)

instance Storable PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
           zero


-- | VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV - Structure
-- describing push descriptor limits that can be supported by an
-- implementation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  { -- | @maxGraphicsShaderGroupCount@ is the maximum number of shader groups in
    -- 'GraphicsPipelineShaderGroupsCreateInfoNV'.
    maxGraphicsShaderGroupCount :: Word32
  , -- | @maxIndirectSequenceCount@ is the maximum number of sequences in
    -- 'GeneratedCommandsInfoNV' and in
    -- 'GeneratedCommandsMemoryRequirementsInfoNV'.
    maxIndirectSequenceCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsTokenCount"
    maxIndirectCommandsTokenCount :: Word32
  , -- | @maxIndirectCommandsStreamCount@ is the maximum number of streams in
    -- 'IndirectCommandsLayoutCreateInfoNV'.
    maxIndirectCommandsStreamCount :: Word32
  , -- | @maxIndirectCommandsTokenOffset@ is the maximum offset in
    -- 'IndirectCommandsLayoutTokenNV'.
    maxIndirectCommandsTokenOffset :: Word32
  , -- | @maxIndirectCommandsStreamStride@ is the maximum stream stride in
    -- 'IndirectCommandsLayoutCreateInfoNV'.
    maxIndirectCommandsStreamStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesCountBufferOffsetAlignment"
    minSequencesCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesIndexBufferOffsetAlignment"
    minSequencesIndexBufferOffsetAlignment :: Word32
  , -- | @minIndirectCommandsBufferOffsetAlignment@ is the minimum alignment for
    -- memory addresses used in 'IndirectCommandsStreamNV' and as preprocess
    -- buffer in 'GeneratedCommandsInfoNV'.
    minIndirectCommandsBufferOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxGraphicsShaderGroupCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxIndirectSequenceCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxIndirectCommandsTokenCount)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxIndirectCommandsStreamCount)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxIndirectCommandsTokenOffset)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxIndirectCommandsStreamStride)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (minSequencesCountBufferOffsetAlignment)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (minSequencesIndexBufferOffsetAlignment)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (minIndirectCommandsBufferOffsetAlignment)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  peekCStruct p = do
    maxGraphicsShaderGroupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxIndirectSequenceCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxIndirectCommandsTokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxIndirectCommandsStreamCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxIndirectCommandsTokenOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxIndirectCommandsStreamStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    minSequencesCountBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    minSequencesIndexBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    minIndirectCommandsBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
             maxGraphicsShaderGroupCount maxIndirectSequenceCount maxIndirectCommandsTokenCount maxIndirectCommandsStreamCount maxIndirectCommandsTokenOffset maxIndirectCommandsStreamStride minSequencesCountBufferOffsetAlignment minSequencesIndexBufferOffsetAlignment minIndirectCommandsBufferOffsetAlignment

instance Storable PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGraphicsShaderGroupCreateInfoNV - Structure specifying override
-- parameters for each shader group
--
-- == Valid Usage
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-stageCount-02888# For
--     @stageCount@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@stageCount@
--     apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pStages-02889# For
--     @pStages@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pVertexInputState-02890# For
--     @pVertexInputState@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pVertexInputState@
--     apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pTessellationState-02891#
--     For @pTessellationState@, the same restrictions as in
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pTessellationState@
--     apply
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pStages-parameter# @pStages@
--     /must/ be a valid pointer to an array of @stageCount@ valid
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-stageCount-arraylength#
--     @stageCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsShaderGroupCreateInfoNV = GraphicsShaderGroupCreateInfoNV
  { -- | @pStages@ is an array of size @stageCount@ structures of type
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' describing the
    -- set of the shader stages to be included in this shader group.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pVertexInputState@ is a pointer to an instance of the
    -- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo' structure.
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- | @pTessellationState@ is a pointer to an instance of the
    -- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo' structure,
    -- and is ignored if the shader group does not include a tessellation
    -- control shader stage and tessellation evaluation shader stage.
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsShaderGroupCreateInfoNV)
#endif
deriving instance Show GraphicsShaderGroupCreateInfoNV

instance ToCStruct GraphicsShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsShaderGroupCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pVertexInputState'' <- case (vertexInputState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineVertexInputStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineVertexInputStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _)))) pVertexInputState''
    pTessellationState'' <- case (tessellationState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineTessellationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineTessellationStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _)))) pTessellationState''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct GraphicsShaderGroupCreateInfoNV where
  peekCStruct p = do
    stageCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pVertexInputState <- peek @(Ptr (PipelineVertexInputStateCreateInfo _)) ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _))))
    pVertexInputState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pVertexInputState
    pTessellationState <- peek @(Ptr (PipelineTessellationStateCreateInfo _)) ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _))))
    pTessellationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pTessellationState
    pure $ GraphicsShaderGroupCreateInfoNV
             pStages' pVertexInputState' pTessellationState'

instance Zero GraphicsShaderGroupCreateInfoNV where
  zero = GraphicsShaderGroupCreateInfoNV
           mempty
           Nothing
           Nothing


-- | VkGraphicsPipelineShaderGroupsCreateInfoNV - Structure specifying
-- parameters of a newly created multi shader group pipeline
--
-- = Description
--
-- When referencing shader groups by index, groups defined in the
-- referenced pipelines are treated as if they were defined as additional
-- entries in @pGroups@. They are appended in the order they appear in the
-- @pPipelines@ array and in the @pGroups@ array when those pipelines were
-- defined.
--
-- The application /must/ maintain the lifetime of all such referenced
-- pipelines based on the pipelines that make use of them.
--
-- == Valid Usage
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-groupCount-02879#
--     @groupCount@ /must/ be at least @1@ and as maximum
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxGraphicsShaderGroupCount@
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-groupCount-02880#
--     The sum of @groupCount@ including those groups added from referenced
--     @pPipelines@ /must/ also be as maximum
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxGraphicsShaderGroupCount@
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-02881# The
--     state of the first element of @pGroups@ /must/ match its equivalent
--     within the parent’s
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-02882# Each
--     element of @pGroups@ /must/ in combination with the rest of the
--     pipeline state yield a valid state configuration
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-02884# All
--     elements of @pGroups@ /must/ use the same shader stage combinations
--     unless any mesh shader stage is used, then either combination of
--     task and mesh or just mesh shader is valid
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-02885# Mesh
--     and regular primitive shading stages cannot be mixed across
--     @pGroups@
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pPipelines-02886#
--     Each element of @pPipelines@ /must/ have been created with identical
--     state to the pipeline currently created except the state that can be
--     overridden by 'GraphicsShaderGroupCreateInfoNV'
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-deviceGeneratedCommands-02887#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV'
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-parameter#
--     @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid 'GraphicsShaderGroupCreateInfoNV' structures
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pPipelines-parameter#
--     If @pipelineCount@ is not @0@, @pPipelines@ /must/ be a valid
--     pointer to an array of @pipelineCount@ valid
--     'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-groupCount-arraylength#
--     @groupCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'GraphicsShaderGroupCreateInfoNV', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsPipelineShaderGroupsCreateInfoNV = GraphicsPipelineShaderGroupsCreateInfoNV
  { -- | @pGroups@ is an array of 'GraphicsShaderGroupCreateInfoNV' values
    -- specifying which state of the original
    -- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' each shader group
    -- overrides.
    groups :: Vector GraphicsShaderGroupCreateInfoNV
  , -- | @pPipelines@ is an array of graphics 'Vulkan.Core10.Handles.Pipeline',
    -- which are referenced within the created pipeline, including all their
    -- shader groups.
    pipelines :: Vector Pipeline
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineShaderGroupsCreateInfoNV)
#endif
deriving instance Show GraphicsPipelineShaderGroupsCreateInfoNV

instance ToCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineShaderGroupsCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelines)) :: Word32))
    pPPipelines' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (pipelines)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelines' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (pipelines)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Pipeline))) (pPPipelines')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  peekCStruct p = do
    groupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pGroups <- peek @(Ptr GraphicsShaderGroupCreateInfoNV) ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @GraphicsShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV)))
    pipelineCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPipelines <- peek @(Ptr Pipeline) ((p `plusPtr` 40 :: Ptr (Ptr Pipeline)))
    pPipelines' <- generateM (fromIntegral pipelineCount) (\i -> peek @Pipeline ((pPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
    pure $ GraphicsPipelineShaderGroupsCreateInfoNV
             pGroups' pPipelines'

instance Zero GraphicsPipelineShaderGroupsCreateInfoNV where
  zero = GraphicsPipelineShaderGroupsCreateInfoNV
           mempty
           mempty


-- | VkBindShaderGroupIndirectCommandNV - Structure specifying input data for
-- a single shader group command token
--
-- == Valid Usage
--
-- -   #VUID-VkBindShaderGroupIndirectCommandNV-None-02944# The current
--     bound graphics pipeline, as well as the pipelines it may reference,
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkBindShaderGroupIndirectCommandNV-index-02945# The @index@
--     /must/ be within range of the accessible shader groups of the
--     current bound graphics pipeline. See 'cmdBindPipelineShaderGroupNV'
--     for further details
--
-- = See Also
--
-- No cross-references are available
data BindShaderGroupIndirectCommandNV = BindShaderGroupIndirectCommandNV
  { -- No documentation found for Nested "VkBindShaderGroupIndirectCommandNV" "groupIndex"
    groupIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindShaderGroupIndirectCommandNV)
#endif
deriving instance Show BindShaderGroupIndirectCommandNV

instance ToCStruct BindShaderGroupIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindShaderGroupIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (groupIndex)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindShaderGroupIndirectCommandNV where
  peekCStruct p = do
    groupIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ BindShaderGroupIndirectCommandNV
             groupIndex

instance Storable BindShaderGroupIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindShaderGroupIndirectCommandNV where
  zero = BindShaderGroupIndirectCommandNV
           zero


-- | VkBindIndexBufferIndirectCommandNV - Structure specifying input data for
-- a single index buffer command token
--
-- == Valid Usage
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-None-02946# The buffer’s
--     usage flag from which the address was acquired /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-bufferAddress-02947# The
--     @bufferAddress@ /must/ be aligned to the @indexType@ used
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-None-02948# Each element of
--     the buffer from which the address was acquired and that is
--     non-sparse /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
data BindIndexBufferIndirectCommandNV = BindIndexBufferIndirectCommandNV
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as index buffer.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
    -- specifying how indices are treated. Instead of the Vulkan enum values, a
    -- custom @uint32_t@ value /can/ be mapped to an
    -- 'Vulkan.Core10.Enums.IndexType.IndexType' by specifying the
    -- 'IndirectCommandsLayoutTokenNV'::@pIndexTypes@ and
    -- 'IndirectCommandsLayoutTokenNV'::@pIndexTypeValues@ arrays.
    indexType :: IndexType
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindIndexBufferIndirectCommandNV)
#endif
deriving instance Show BindIndexBufferIndirectCommandNV

instance ToCStruct BindIndexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindIndexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (indexType)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (zero)
    f

instance FromCStruct BindIndexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 12 :: Ptr IndexType))
    pure $ BindIndexBufferIndirectCommandNV
             bufferAddress size indexType

instance Storable BindIndexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindIndexBufferIndirectCommandNV where
  zero = BindIndexBufferIndirectCommandNV
           zero
           zero
           zero


-- | VkBindVertexBufferIndirectCommandNV - Structure specifying input data
-- for a single vertex buffer command token
--
-- == Valid Usage
--
-- -   #VUID-VkBindVertexBufferIndirectCommandNV-None-02949# The buffer’s
--     usage flag from which the address was acquired /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkBindVertexBufferIndirectCommandNV-None-02950# Each element
--     of the buffer from which the address was acquired and that is
--     non-sparse /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
data BindVertexBufferIndirectCommandNV = BindVertexBufferIndirectCommandNV
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as vertex input binding.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @stride@ is the byte size stride for this vertex input binding as in
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@. It is
    -- only used if 'IndirectCommandsLayoutTokenNV'::@vertexDynamicStride@ was
    -- set, otherwise the stride is inherited from the current bound graphics
    -- pipeline.
    stride :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindVertexBufferIndirectCommandNV)
#endif
deriving instance Show BindVertexBufferIndirectCommandNV

instance ToCStruct BindVertexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindVertexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (stride)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindVertexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ BindVertexBufferIndirectCommandNV
             bufferAddress size stride

instance Storable BindVertexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindVertexBufferIndirectCommandNV where
  zero = BindVertexBufferIndirectCommandNV
           zero
           zero
           zero


-- | VkSetStateFlagsIndirectCommandNV - Structure specifying input data for a
-- single state flag command token
--
-- = See Also
--
-- No cross-references are available
data SetStateFlagsIndirectCommandNV = SetStateFlagsIndirectCommandNV
  { -- | @data@ encodes packed state that this command alters.
    --
    -- -   Bit @0@: If set represents
    --     'Vulkan.Core10.Enums.FrontFace.FRONT_FACE_CLOCKWISE', otherwise
    --     'Vulkan.Core10.Enums.FrontFace.FRONT_FACE_COUNTER_CLOCKWISE'
    data' :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SetStateFlagsIndirectCommandNV)
#endif
deriving instance Show SetStateFlagsIndirectCommandNV

instance ToCStruct SetStateFlagsIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetStateFlagsIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (data')
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct SetStateFlagsIndirectCommandNV where
  peekCStruct p = do
    data' <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ SetStateFlagsIndirectCommandNV
             data'

instance Storable SetStateFlagsIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SetStateFlagsIndirectCommandNV where
  zero = SetStateFlagsIndirectCommandNV
           zero


-- | VkIndirectCommandsStreamNV - Structure specifying input streams for
-- generated command tokens
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsStreamNV-buffer-02942# The @buffer@’s usage
--     flag /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkIndirectCommandsStreamNV-offset-02943# The @offset@ /must/
--     be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minIndirectCommandsBufferOffsetAlignment@
--
-- -   #VUID-VkIndirectCommandsStreamNV-buffer-02975# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsStreamNV-buffer-parameter# @buffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'GeneratedCommandsInfoNV'
data IndirectCommandsStreamNV = IndirectCommandsStreamNV
  { -- | @buffer@ specifies the 'Vulkan.Core10.Handles.Buffer' storing the
    -- functional arguments for each sequence. These arguments /can/ be written
    -- by the device.
    buffer :: Buffer
  , -- | @offset@ specified an offset into @buffer@ where the arguments start.
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsStreamNV)
#endif
deriving instance Show IndirectCommandsStreamNV

instance ToCStruct IndirectCommandsStreamNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsStreamNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct IndirectCommandsStreamNV where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    pure $ IndirectCommandsStreamNV
             buffer offset

instance Storable IndirectCommandsStreamNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsStreamNV where
  zero = IndirectCommandsStreamNV
           zero
           zero


-- | VkIndirectCommandsLayoutTokenNV - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-stream-02951# @stream@ /must/
--     be smaller than 'IndirectCommandsLayoutCreateInfoNV'::@streamCount@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-offset-02952# @offset@ /must/
--     be less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsTokenOffset@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02976# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV',
--     @vertexBindingUnit@ /must/ stay within device supported limits for
--     the appropriate commands
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02977# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantPipelineLayout@ /must/ be valid
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02978# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantOffset@ /must/ be a multiple of @4@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02979# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantSize@ /must/ be a multiple of @4@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02980# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantOffset@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02981# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV',
--     @pushconstantSize@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @pushconstantOffset@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02982# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV', for
--     each byte in the range specified by @pushconstantOffset@ and
--     @pushconstantSize@ and for each shader stage in
--     @pushconstantShaderStageFlags@, there /must/ be a push constant
--     range in @pushconstantPipelineLayout@ that includes that byte and
--     that stage
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02983# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV', for
--     each byte in the range specified by @pushconstantOffset@ and
--     @pushconstantSize@ and for each push constant range that overlaps
--     that byte, @pushconstantShaderStageFlags@ /must/ include all stages
--     in that push constant range’s
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@pushconstantShaderStageFlags@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02984# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV',
--     @indirectStateFlags@ /must/ not be ´0´
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-parameter#
--     @tokenType@ /must/ be a valid 'IndirectCommandsTokenTypeNV' value
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pushconstantPipelineLayout-parameter#
--     If @pushconstantPipelineLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pushconstantPipelineLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pushconstantShaderStageFlags-parameter#
--     @pushconstantShaderStageFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-indirectStateFlags-parameter#
--     @indirectStateFlags@ /must/ be a valid combination of
--     'IndirectStateFlagBitsNV' values
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pIndexTypes-parameter# If
--     @indexTypeCount@ is not @0@, @pIndexTypes@ /must/ be a valid pointer
--     to an array of @indexTypeCount@ valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' values
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pIndexTypeValues-parameter# If
--     @indexTypeCount@ is not @0@, @pIndexTypeValues@ /must/ be a valid
--     pointer to an array of @indexTypeCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'IndirectCommandsLayoutCreateInfoNV', 'IndirectCommandsTokenTypeNV',
-- 'IndirectStateFlagsNV', 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectCommandsLayoutTokenNV = IndirectCommandsLayoutTokenNV
  { -- | @tokenType@ specifies the token command type.
    tokenType :: IndirectCommandsTokenTypeNV
  , -- | @stream@ is the index of the input stream that contains the token
    -- argument data.
    stream :: Word32
  , -- | @offset@ is a relative starting offset within the input stream memory
    -- for the token argument data.
    offset :: Word32
  , -- | @vertexBindingUnit@ is used for the vertex buffer binding command.
    vertexBindingUnit :: Word32
  , -- | @vertexDynamicStride@ sets if the vertex buffer stride is provided by
    -- the binding command rather than the current bound graphics pipeline
    -- state.
    vertexDynamicStride :: Bool
  , -- | @pushconstantPipelineLayout@ is the
    -- 'Vulkan.Core10.Handles.PipelineLayout' used for the push constant
    -- command.
    pushconstantPipelineLayout :: PipelineLayout
  , -- | @pushconstantShaderStageFlags@ are the shader stage flags used for the
    -- push constant command.
    pushconstantShaderStageFlags :: ShaderStageFlags
  , -- | @pushconstantOffset@ is the offset used for the push constant command.
    pushconstantOffset :: Word32
  , -- | @pushconstantSize@ is the size used for the push constant command.
    pushconstantSize :: Word32
  , -- | @indirectStateFlags@ are the active states for the state flag command.
    indirectStateFlags :: IndirectStateFlagsNV
  , -- | @pIndexTypes@ is the used 'Vulkan.Core10.Enums.IndexType.IndexType' for
    -- the corresponding @uint32_t@ value entry in @pIndexTypeValues@.
    indexTypes :: Vector IndexType
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pIndexTypeValues"
    indexTypeValues :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutTokenNV)
#endif
deriving instance Show IndirectCommandsLayoutTokenNV

instance ToCStruct IndirectCommandsLayoutTokenNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (tokenType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (stream)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (offset)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (vertexBindingUnit)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (vertexDynamicStride))
    lift $ poke ((p `plusPtr` 40 :: Ptr PipelineLayout)) (pushconstantPipelineLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr ShaderStageFlags)) (pushconstantShaderStageFlags)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (pushconstantOffset)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (pushconstantSize)
    lift $ poke ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV)) (indirectStateFlags)
    let pIndexTypesLength = Data.Vector.length $ (indexTypes)
    lift $ unless ((Data.Vector.length $ (indexTypeValues)) == pIndexTypesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pIndexTypeValues and pIndexTypes must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral pIndexTypesLength :: Word32))
    pPIndexTypes' <- ContT $ allocaBytesAligned @IndexType ((Data.Vector.length (indexTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (indexTypes)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (indexTypeValues)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indexTypeValues)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectCommandsLayoutTokenNV where
  peekCStruct p = do
    tokenType <- peek @IndirectCommandsTokenTypeNV ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV))
    stream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    offset <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    vertexBindingUnit <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    vertexDynamicStride <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pushconstantPipelineLayout <- peek @PipelineLayout ((p `plusPtr` 40 :: Ptr PipelineLayout))
    pushconstantShaderStageFlags <- peek @ShaderStageFlags ((p `plusPtr` 48 :: Ptr ShaderStageFlags))
    pushconstantOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pushconstantSize <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    indirectStateFlags <- peek @IndirectStateFlagsNV ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV))
    indexTypeCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pIndexTypes <- peek @(Ptr IndexType) ((p `plusPtr` 72 :: Ptr (Ptr IndexType)))
    pIndexTypes' <- generateM (fromIntegral indexTypeCount) (\i -> peek @IndexType ((pIndexTypes `advancePtrBytes` (4 * (i)) :: Ptr IndexType)))
    pIndexTypeValues <- peek @(Ptr Word32) ((p `plusPtr` 80 :: Ptr (Ptr Word32)))
    pIndexTypeValues' <- generateM (fromIntegral indexTypeCount) (\i -> peek @Word32 ((pIndexTypeValues `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutTokenNV
             tokenType stream offset vertexBindingUnit (bool32ToBool vertexDynamicStride) pushconstantPipelineLayout pushconstantShaderStageFlags pushconstantOffset pushconstantSize indirectStateFlags pIndexTypes' pIndexTypeValues'

instance Zero IndirectCommandsLayoutTokenNV where
  zero = IndirectCommandsLayoutTokenNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty


-- | VkIndirectCommandsLayoutCreateInfoNV - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the flags:
--
-- > void cmdProcessAllSequences(cmd, pipeline, indirectCommandsLayout, pIndirectCommandsTokens, sequencesCount, indexbuffer, indexbufferOffset)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sUsed = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV) {
-- >       sUsed = indexbuffer.load_uint32( sUsed * sizeof(uint32_t) + indexbufferOffset);
-- >     }
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV) {
-- >       sUsed = incoherent_implementation_dependent_permutation[ sUsed ];
-- >     }
-- >
-- >     cmdProcessSequence( cmd, pipeline, indirectCommandsLayout, pIndirectCommandsTokens, sUsed );
-- >   }
-- > }
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-02930#
--     The @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-tokenCount-02931#
--     @tokenCount@ /must/ be greater than @0@ and less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsTokenCount@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02932# If
--     @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV' it /must/ be the
--     first element of the array and there /must/ be only a single element
--     of such token type
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02933# If
--     @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV' there /must/ be only a
--     single element of such token type
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02934# All state
--     tokens in @pTokens@ /must/ occur prior work provoking tokens
--     ('INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV')
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02935# The
--     content of @pTokens@ /must/ include one single work provoking token
--     that is compatible with the @pipelineBindPoint@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-streamCount-02936#
--     @streamCount@ /must/ be greater than @0@ and less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamCount@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pStreamStrides-02937#
--     each element of @pStreamStrides@ /must/ be greater than \`0\`and
--     less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamStride@.
--     Furthermore the alignment of each token input /must/ be ensured
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'IndirectCommandsLayoutUsageFlagBitsNV' values
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-flags-requiredbitmask#
--     @flags@ /must/ not be @0@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-parameter#
--     @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid 'IndirectCommandsLayoutTokenNV' structures
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pStreamStrides-parameter#
--     @pStreamStrides@ /must/ be a valid pointer to an array of
--     @streamCount@ @uint32_t@ values
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-tokenCount-arraylength#
--     @tokenCount@ /must/ be greater than @0@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-streamCount-arraylength#
--     @streamCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNV', 'IndirectCommandsLayoutUsageFlagsNV',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createIndirectCommandsLayoutNV'
data IndirectCommandsLayoutCreateInfoNV = IndirectCommandsLayoutCreateInfoNV
  { -- | @flags@ is a bitmask of 'IndirectCommandsLayoutUsageFlagBitsNV'
    -- specifying usage hints of this layout.
    flags :: IndirectCommandsLayoutUsageFlagsNV
  , -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' that this
    -- layout targets.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pTokens@ is an array describing each command token in detail. See
    -- 'IndirectCommandsTokenTypeNV' and 'IndirectCommandsLayoutTokenNV' below
    -- for details.
    tokens :: Vector IndirectCommandsLayoutTokenNV
  , -- | @pStreamStrides@ is an array defining the byte stride for each input
    -- stream.
    streamStrides :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutCreateInfoNV)
#endif
deriving instance Show IndirectCommandsLayoutCreateInfoNV

instance ToCStruct IndirectCommandsLayoutCreateInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNV ((Data.Vector.length (tokens)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV))) (pPTokens')
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streamStrides)) :: Word32))
    pPStreamStrides' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (streamStrides)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (streamStrides)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    f

instance FromCStruct IndirectCommandsLayoutCreateInfoNV where
  peekCStruct p = do
    flags <- peek @IndirectCommandsLayoutUsageFlagsNV ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    tokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pTokens <- peek @(Ptr IndirectCommandsLayoutTokenNV) ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV)))
    pTokens' <- generateM (fromIntegral tokenCount) (\i -> peekCStruct @IndirectCommandsLayoutTokenNV ((pTokens `advancePtrBytes` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV)))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreamStrides <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pStreamStrides' <- generateM (fromIntegral streamCount) (\i -> peek @Word32 ((pStreamStrides `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutCreateInfoNV
             flags pipelineBindPoint pTokens' pStreamStrides'

instance Zero IndirectCommandsLayoutCreateInfoNV where
  zero = IndirectCommandsLayoutCreateInfoNV
           zero
           zero
           mempty
           mempty


-- | VkGeneratedCommandsInfoNV - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipeline-02912# The provided
--     @pipeline@ /must/ match the pipeline bound at execution time
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-02913# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV', then the @pipeline@
--     /must/ have been created with multiple shader groups
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-02914# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV', then the @pipeline@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--     set in 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@flags@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-02915# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV', then the
--     @pipeline@\`s 'Vulkan.Core10.Handles.PipelineLayout' /must/ match
--     the 'IndirectCommandsLayoutTokenNV'::@pushconstantPipelineLayout@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-streamCount-02916# @streamCount@
--     /must/ match the @indirectCommandsLayout@’s @streamCount@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCount-02917#
--     @sequencesCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectSequenceCount@
--     and 'GeneratedCommandsMemoryRequirementsInfoNV'::@maxSequencesCount@
--     that was used to determine the @preprocessSize@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-preprocessBuffer-02918#
--     @preprocessBuffer@ /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set in its usage flag
--
-- -   #VUID-VkGeneratedCommandsInfoNV-preprocessOffset-02919#
--     @preprocessOffset@ /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minIndirectCommandsBufferOffsetAlignment@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-preprocessBuffer-02971# If
--     @preprocessBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory'
--     object
--
-- -   #VUID-VkGeneratedCommandsInfoNV-preprocessSize-02920#
--     @preprocessSize@ /must/ be at least equal to the memory
--     requirement\`s size returned by
--     'getGeneratedCommandsMemoryRequirementsNV' using the matching inputs
--     (@indirectCommandsLayout@, …​) as within this structure
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCountBuffer-02921#
--     @sequencesCountBuffer@ /can/ be set if the actual used count of
--     sequences is sourced from the provided buffer. In that case the
--     @sequencesCount@ serves as upper bound
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCountBuffer-02922# If
--     @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', its usage flag /must/ have
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCountBuffer-02923# If
--     @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesCountOffset@
--     /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minSequencesCountBufferOffsetAlignment@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCountBuffer-02972# If
--     @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesIndexBuffer-02924# If
--     @indirectCommandsLayout@’s
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV' is set,
--     @sequencesIndexBuffer@ /must/ be set otherwise it /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesIndexBuffer-02925# If
--     @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', its usage flag /must/ have
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesIndexBuffer-02926# If
--     @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesIndexOffset@
--     /must/ be aligned to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minSequencesIndexBufferOffsetAlignment@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesIndexBuffer-02973# If
--     @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV'
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipeline-parameter# @pipeline@
--     /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-parameter#
--     @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pStreams-parameter# @pStreams@
--     /must/ be a valid pointer to an array of @streamCount@ valid
--     'IndirectCommandsStreamNV' structures
--
-- -   #VUID-VkGeneratedCommandsInfoNV-preprocessBuffer-parameter#
--     @preprocessBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesCountBuffer-parameter# If
--     @sequencesCountBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesCountBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkGeneratedCommandsInfoNV-sequencesIndexBuffer-parameter# If
--     @sequencesIndexBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @sequencesIndexBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkGeneratedCommandsInfoNV-streamCount-arraylength#
--     @streamCount@ /must/ be greater than @0@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-commonparent# Each of
--     @indirectCommandsLayout@, @pipeline@, @preprocessBuffer@,
--     @sequencesCountBuffer@, and @sequencesIndexBuffer@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV',
-- 'IndirectCommandsStreamNV', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdExecuteGeneratedCommandsNV', 'cmdPreprocessGeneratedCommandsNV'
data GeneratedCommandsInfoNV = GeneratedCommandsInfoNV
  { -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' used for the
    -- @pipeline@.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pipeline@ is the 'Vulkan.Core10.Handles.Pipeline' used in the
    -- generation and execution process.
    pipeline :: Pipeline
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' that provides the
    -- command sequence to generate.
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- | @pStreams@ provides an array of 'IndirectCommandsStreamNV' that provide
    -- the input data for the tokens used in @indirectCommandsLayout@.
    streams :: Vector IndirectCommandsStreamNV
  , -- | @sequencesCount@ is the maximum number of sequences to reserve. If
    -- @sequencesCountBuffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', this
    -- is also the actual number of sequences generated.
    sequencesCount :: Word32
  , -- | @preprocessBuffer@ is the 'Vulkan.Core10.Handles.Buffer' that is used
    -- for preprocessing the input data for execution. If this structure is
    -- used with 'cmdExecuteGeneratedCommandsNV' with its @isPreprocessed@ set
    -- to 'Vulkan.Core10.FundamentalTypes.TRUE', then the preprocessing step is
    -- skipped and data is only read from this buffer.
    preprocessBuffer :: Buffer
  , -- | @preprocessOffset@ is the byte offset into @preprocessBuffer@ where the
    -- preprocessed data is stored.
    preprocessOffset :: DeviceSize
  , -- | @preprocessSize@ is the maximum byte size within the @preprocessBuffer@
    -- after the @preprocessOffset@ that is available for preprocessing.
    preprocessSize :: DeviceSize
  , -- | @sequencesCountBuffer@ is a 'Vulkan.Core10.Handles.Buffer' in which the
    -- actual number of sequences is provided as single @uint32_t@ value.
    sequencesCountBuffer :: Buffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
    -- where the count value is stored.
    sequencesCountOffset :: DeviceSize
  , -- | @sequencesIndexBuffer@ is a 'Vulkan.Core10.Handles.Buffer' that encodes
    -- the used sequence indices as @uint32_t@ array.
    sequencesIndexBuffer :: Buffer
  , -- | @sequencesIndexOffset@ is the byte offset into @sequencesIndexBuffer@
    -- where the index values start.
    sequencesIndexOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsInfoNV)
#endif
deriving instance Show GeneratedCommandsInfoNV

instance ToCStruct GeneratedCommandsInfoNV where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streams)) :: Word32))
    pPStreams' <- ContT $ allocaBytesAligned @IndirectCommandsStreamNV ((Data.Vector.length (streams)) * 16) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreams' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsStreamNV) (e)) (streams)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV))) (pPStreams')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (sequencesCount)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (preprocessBuffer)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (preprocessOffset)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (preprocessSize)
    lift $ poke ((p `plusPtr` 88 :: Ptr Buffer)) (sequencesCountBuffer)
    lift $ poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (sequencesCountOffset)
    lift $ poke ((p `plusPtr` 104 :: Ptr Buffer)) (sequencesIndexBuffer)
    lift $ poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (sequencesIndexOffset)
    lift $ f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeneratedCommandsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreams <- peek @(Ptr IndirectCommandsStreamNV) ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV)))
    pStreams' <- generateM (fromIntegral streamCount) (\i -> peekCStruct @IndirectCommandsStreamNV ((pStreams `advancePtrBytes` (16 * (i)) :: Ptr IndirectCommandsStreamNV)))
    sequencesCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    preprocessBuffer <- peek @Buffer ((p `plusPtr` 64 :: Ptr Buffer))
    preprocessOffset <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    preprocessSize <- peek @DeviceSize ((p `plusPtr` 80 :: Ptr DeviceSize))
    sequencesCountBuffer <- peek @Buffer ((p `plusPtr` 88 :: Ptr Buffer))
    sequencesCountOffset <- peek @DeviceSize ((p `plusPtr` 96 :: Ptr DeviceSize))
    sequencesIndexBuffer <- peek @Buffer ((p `plusPtr` 104 :: Ptr Buffer))
    sequencesIndexOffset <- peek @DeviceSize ((p `plusPtr` 112 :: Ptr DeviceSize))
    pure $ GeneratedCommandsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout pStreams' sequencesCount preprocessBuffer preprocessOffset preprocessSize sequencesCountBuffer sequencesCountOffset sequencesIndexBuffer sequencesIndexOffset

instance Zero GeneratedCommandsInfoNV where
  zero = GeneratedCommandsInfoNV
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGeneratedCommandsMemoryRequirementsInfoNV - Structure specifying
-- parameters for the reservation of preprocess buffer space
--
-- == Valid Usage
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-maxSequencesCount-02907#
--     @maxSequencesCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectSequenceCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV'
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pNext-pNext#
--     @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-indirectCommandsLayout-parameter#
--     @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-commonparent# Both
--     of @indirectCommandsLayout@, and @pipeline@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV',
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getGeneratedCommandsMemoryRequirementsNV'
data GeneratedCommandsMemoryRequirementsInfoNV = GeneratedCommandsMemoryRequirementsInfoNV
  { -- | @pipelineBindPoint@ is the
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' of the
    -- @pipeline@ that this buffer memory is intended to be used with during
    -- the execution.
    pipelineBindPoint :: PipelineBindPoint
  , -- | @pipeline@ is the 'Vulkan.Core10.Handles.Pipeline' that this buffer
    -- memory is intended to be used with during the execution.
    pipeline :: Pipeline
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' that this buffer
    -- memory is intended to be used with.
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- | @maxSequencesCount@ is the maximum number of sequences that this buffer
    -- memory in combination with the other state provided /can/ be used with.
    maxSequencesCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsMemoryRequirementsInfoNV)
#endif
deriving instance Show GeneratedCommandsMemoryRequirementsInfoNV

instance ToCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxSequencesCount)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    maxSequencesCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ GeneratedCommandsMemoryRequirementsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout maxSequencesCount

instance Storable GeneratedCommandsMemoryRequirementsInfoNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeneratedCommandsMemoryRequirementsInfoNV where
  zero = GeneratedCommandsMemoryRequirementsInfoNV
           zero
           zero
           zero
           zero


type IndirectCommandsLayoutUsageFlagsNV = IndirectCommandsLayoutUsageFlagBitsNV

-- | VkIndirectCommandsLayoutUsageFlagBitsNV - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- 'IndirectCommandsLayoutUsageFlagsNV'
newtype IndirectCommandsLayoutUsageFlagBitsNV = IndirectCommandsLayoutUsageFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' specifies
-- that the layout is always used with the manual preprocessing step
-- through calling 'cmdPreprocessGeneratedCommandsNV' and executed by
-- 'cmdExecuteGeneratedCommandsNV' with @isPreprocessed@ set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000001
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV' specifies that
-- the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided 'Vulkan.Core10.Handles.Buffer'
-- encoding the index is provided.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV   = IndirectCommandsLayoutUsageFlagBitsNV 0x00000002
-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV' specifies
-- that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not: guaranteed to be coherent
-- using the same input data.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000004

conNameIndirectCommandsLayoutUsageFlagBitsNV :: String
conNameIndirectCommandsLayoutUsageFlagBitsNV = "IndirectCommandsLayoutUsageFlagBitsNV"

enumPrefixIndirectCommandsLayoutUsageFlagBitsNV :: String
enumPrefixIndirectCommandsLayoutUsageFlagBitsNV = "INDIRECT_COMMANDS_LAYOUT_USAGE_"

showTableIndirectCommandsLayoutUsageFlagBitsNV :: [(IndirectCommandsLayoutUsageFlagBitsNV, String)]
showTableIndirectCommandsLayoutUsageFlagBitsNV =
  [ (INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV, "EXPLICIT_PREPROCESS_BIT_NV")
  , (INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV  , "INDEXED_SEQUENCES_BIT_NV")
  , (INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV, "UNORDERED_SEQUENCES_BIT_NV")
  ]

instance Show IndirectCommandsLayoutUsageFlagBitsNV where
  showsPrec = enumShowsPrec enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
                            showTableIndirectCommandsLayoutUsageFlagBitsNV
                            conNameIndirectCommandsLayoutUsageFlagBitsNV
                            (\(IndirectCommandsLayoutUsageFlagBitsNV x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read IndirectCommandsLayoutUsageFlagBitsNV where
  readPrec = enumReadPrec enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
                          showTableIndirectCommandsLayoutUsageFlagBitsNV
                          conNameIndirectCommandsLayoutUsageFlagBitsNV
                          IndirectCommandsLayoutUsageFlagBitsNV


type IndirectStateFlagsNV = IndirectStateFlagBitsNV

-- | VkIndirectStateFlagBitsNV - Bitmask specifiying state that can be
-- altered on the device
--
-- = See Also
--
-- 'IndirectStateFlagsNV'
newtype IndirectStateFlagBitsNV = IndirectStateFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV' allows to toggle the
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace' rasterization state for
-- subsequent draw operations.
pattern INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV = IndirectStateFlagBitsNV 0x00000001

conNameIndirectStateFlagBitsNV :: String
conNameIndirectStateFlagBitsNV = "IndirectStateFlagBitsNV"

enumPrefixIndirectStateFlagBitsNV :: String
enumPrefixIndirectStateFlagBitsNV = "INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"

showTableIndirectStateFlagBitsNV :: [(IndirectStateFlagBitsNV, String)]
showTableIndirectStateFlagBitsNV = [(INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV, "")]

instance Show IndirectStateFlagBitsNV where
  showsPrec = enumShowsPrec enumPrefixIndirectStateFlagBitsNV
                            showTableIndirectStateFlagBitsNV
                            conNameIndirectStateFlagBitsNV
                            (\(IndirectStateFlagBitsNV x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read IndirectStateFlagBitsNV where
  readPrec = enumReadPrec enumPrefixIndirectStateFlagBitsNV
                          showTableIndirectStateFlagBitsNV
                          conNameIndirectStateFlagBitsNV
                          IndirectStateFlagBitsNV


-- | VkIndirectCommandsTokenTypeNV - Enum specifying token commands
--
-- = Description
--
-- \'
--
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | Token type                                      | Equivalent command                                               |
-- +=================================================+==================================================================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV'  | 'cmdBindPipelineShaderGroupNV'                                   |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV'   | -                                                                |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV'  | 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'         |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV' | 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'       |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV' | 'Vulkan.Core10.CommandBufferBuilding.cmdPushConstants'           |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV'  | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'     |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV'          | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'            |
-- +-------------------------------------------------+------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV'    | 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV' |
-- +-------------------------------------------------+------------------------------------------------------------------+
--
-- Supported indirect command tokens
--
-- = See Also
--
-- 'IndirectCommandsLayoutTokenNV'
newtype IndirectCommandsTokenTypeNV = IndirectCommandsTokenTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV  = IndirectCommandsTokenTypeNV 0
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV   = IndirectCommandsTokenTypeNV 1
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV  = IndirectCommandsTokenTypeNV 2
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV = IndirectCommandsTokenTypeNV 3
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV = IndirectCommandsTokenTypeNV 4
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV  = IndirectCommandsTokenTypeNV 5
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV          = IndirectCommandsTokenTypeNV 6
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV    = IndirectCommandsTokenTypeNV 7
{-# complete INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV :: IndirectCommandsTokenTypeNV #-}

conNameIndirectCommandsTokenTypeNV :: String
conNameIndirectCommandsTokenTypeNV = "IndirectCommandsTokenTypeNV"

enumPrefixIndirectCommandsTokenTypeNV :: String
enumPrefixIndirectCommandsTokenTypeNV = "INDIRECT_COMMANDS_TOKEN_TYPE_"

showTableIndirectCommandsTokenTypeNV :: [(IndirectCommandsTokenTypeNV, String)]
showTableIndirectCommandsTokenTypeNV =
  [ (INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV , "SHADER_GROUP_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV  , "STATE_FLAGS_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV , "INDEX_BUFFER_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV, "VERTEX_BUFFER_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV, "PUSH_CONSTANT_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV , "DRAW_INDEXED_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV         , "DRAW_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV   , "DRAW_TASKS_NV")
  ]

instance Show IndirectCommandsTokenTypeNV where
  showsPrec = enumShowsPrec enumPrefixIndirectCommandsTokenTypeNV
                            showTableIndirectCommandsTokenTypeNV
                            conNameIndirectCommandsTokenTypeNV
                            (\(IndirectCommandsTokenTypeNV x) -> x)
                            (showsPrec 11)

instance Read IndirectCommandsTokenTypeNV where
  readPrec = enumReadPrec enumPrefixIndirectCommandsTokenTypeNV
                          showTableIndirectCommandsTokenTypeNV
                          conNameIndirectCommandsTokenTypeNV
                          IndirectCommandsTokenTypeNV


type NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3


type NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

