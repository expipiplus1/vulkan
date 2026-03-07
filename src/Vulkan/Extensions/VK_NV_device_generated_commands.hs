{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_generated_commands - device extension
--
-- = VK_NV_device_generated_commands
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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_generated_commands] @pixeljetstream%0A*Here describe the issue or question you have about the VK_NV_device_generated_commands extension* >
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
--     compute queue, or for the purpose of reusing the data in multiple
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
-- not happen asynchronously to the device, therefore the primary use case
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
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
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
--     -   'ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
--
--     -   'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV'
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
--     -   'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
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
-- Both ways are desirable. AoS can provide portability to other APIs and
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
-- memory consumption can be reduced. Furthermore reuse of the memory is
-- possible, for example for doing explicit preprocessing and execution in
-- a ping-pong fashion.
--
-- The actual buffer size is implementation-dependent and may be zero, i.e.
-- not always required.
--
-- When making use of Graphics Shader Groups, the programs should behave
-- similarly with regards to vertex inputs, clipping and culling outputs of
-- the geometry stage, and sample shading behavior in fragment shaders, to
-- reduce the amount of the worst-case memory approximation.
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
-- 8) How do we allow reusing already “generated” @indirectCommands@?
--
-- Expose a @preprocessBuffer@ to reuse implementation-dependencyFlags
-- data. Set @isPreprocessed@ to 'Vulkan.Core10.FundamentalTypes.TRUE' in
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
-- a new stage 'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV', and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'.
-- This new stage has two corresponding new access types,
-- 'ACCESS_COMMAND_PREPROCESS_READ_BIT_NV' and
-- 'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV', used to synchronize reading
-- the buffer inputs and writing the preprocess memory output.
--
-- The generated output written in the preprocess buffer memory by
-- 'cmdExecuteGeneratedCommandsNV' is considered to be consumed by the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'
-- pipeline stage.
--
-- Thus, to synchronize from writing the input buffers to preprocessing via
-- 'cmdPreprocessGeneratedCommandsNV', use:
--
-- -   @dstStageMask@ = 'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--
-- -   @dstAccessMask@ = 'ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
--
-- To synchronize from 'cmdPreprocessGeneratedCommandsNV' to executing the
-- generated commands by 'cmdExecuteGeneratedCommandsNV', use:
--
-- -   @srcStageMask@ = 'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--
-- -   @srcAccessMask@ = 'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV'
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
-- -   Revision 2, 2020-03-09 (Christoph Kubisch)
--
--     -   Remove VK_EXT_debug_report interactions
--
-- -   Revision 3, 2020-03-09 (Christoph Kubisch)
--
--     -   Fix naming VkPhysicalDeviceGenerated to
--         VkPhysicalDeviceDeviceGenerated
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_device_generated_commands Vulkan Specification>.
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
                                                          , pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV
                                                          , pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_NV
                                                          , pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV
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
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV
                                                                                       , ..
                                                                                       )
                                                          , NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , IndirectCommandsLayoutNV(..)
                                                          ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipelineShaderGroupNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPreprocessGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetGeneratedCommandsMemoryRequirementsNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_heap (IndirectCommandsLayoutPushDataTokenNV)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.ComputePipeline (PipelineShaderStageCreateInfo)
import Vulkan.Core10.GraphicsPipeline (PipelineTessellationStateCreateInfo)
import Vulkan.Core10.GraphicsPipeline (PipelineVertexInputStateCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT))
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

-- | vkCmdExecuteGeneratedCommandsNV - Generate and execute commands on the
-- device
--
-- = Description
--
-- If the 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV' flag
-- was used to create the
-- 'GeneratedCommandsInfoNV'::@indirectCommandsLayout@ then the order of
-- execution of individual draws through this command /may/ execute in any
-- order, and /may/ not necessarily be in the same order as specified in
-- 'GeneratedCommandsInfoNV'::@pStreams@.
--
-- The order of execution of individual dispatches through this command
-- /may/ execute in any order and /may/ not necessarily be in the same
-- order as specified in 'GeneratedCommandsInfoNV'::@pStreams@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-magFilter-09598# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-mipmapMode-09599# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-unnormalizedCoordinates-09635#
--     If a 'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @levelCount@ and @layerCount@ /must/ be 1
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08609# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08610# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08611# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cubicRangeClamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-selectableCubicWeights-09214#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-selectableCubicWeights selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08600# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a set /n/, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08601# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10068# For each array of
--     resources that is used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     the indices used to access members of the array /must/ be less than
--     the descriptor count for the identified binding in the descriptor
--     sets used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-maintenance4-08602# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08114# Descriptors in
--     each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are accessed as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageLayout-00344# If an image
--     descriptor is accessed by a shader, the
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ match the
--     subresource accessible from the 'Vulkan.Core10.Handles.ImageView' as
--     defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08115# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08116# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08604# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08117# If the descriptors
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08608# If a pipeline is
--     bound to the pipeline bind point used by this command, there /must/
--     not have been any calls to dynamic state setting commands for any
--     state specified statically in the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command, since
--     that pipeline was bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-uniformBuffers-06935# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     and that stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-storageBuffers-06936# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     and that stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-image-dimensions ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageSampleWeightedQCOM-06971#
--     If @OpImageSampleWeightedQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageSampleWeightedQCOM-06972#
--     If @OpImageSampleWeightedQCOM@ uses a
--     'Vulkan.Core10.Handles.ImageView' as a sample weight image as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchSSDQCOM-06974#
--     If @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchSADQCOM-06975#
--     If @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchSADQCOM-06976#
--     If @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageSampleWeightedQCOM-06977#
--     If @OpImageSampleWeightedQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageSampleWeightedQCOM-06978#
--     If any command other than @OpImageSampleWeightedQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchWindow-09215#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchWindow-09216#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpImageBlockMatchWindow-09217#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ read from a reference image as result
--     of this command, then the specified reference coordinates /must/ not
--     fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07288# Any shader
--     invocation executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09600# If a descriptor
--     with type equal to any of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     is accessed as a result of this command, all image subresources
--     identified by that descriptor /must/ be in the image layout
--     identified when the descriptor was written
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-10746# The
--     'Vulkan.Core10.Handles.DeviceMemory' object allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property that is bound to a resource accessed as a result of this
--     command /must/ be the active bound
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-bind-tile-memory bound tile memory object>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10678# If this command is
--     recorded inside a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--     instance, the stages corresponding to the pipeline bind point used
--     by this command /must/ only include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and\/or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10679# If this command is
--     recorded where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, there /must/ be no access to any image while the image
--     was be transitioned to the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     layout
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDescription-09900# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the
--     underlying 'Vulkan.Extensions.Handles.TensorARM' object /must/ have
--     been created with the
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_SHADER_BIT_ARM' usage
--     flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dimensionCount-09905# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the @Rank@
--     of the @OpTypeTensorARM@ of the tensor resource variable /must/ be
--     equal to the @dimensionCount@ provided via
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@pDescription@
--     when creating the underlying 'Vulkan.Extensions.Handles.TensorARM'
--     object
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeTensorARM-09906# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the element
--     type of the @OpTypeTensorARM@ of the tensor resource variable /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-tensor-formats compatible>
--     with the 'Vulkan.Core10.Enums.Format.Format' of the
--     'Vulkan.Extensions.Handles.TensorViewARM' used for the access
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11297# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a @OpTypeStruct@ decorated with @Block@ or
--     @BufferBlock@ using that mapping, the calculated offset for the
--     resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-bufferDescriptorAlignment bufferDescriptorAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11298# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeImage@ or @OpTypeSampledImage@ using
--     that mapping, the calculated offset for the resource heap /must/ be
--     a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-imageDescriptorAlignment imageDescriptorAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11299# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeSampler@ or @OpTypeSampledImage@
--     using that mapping, the calculated offset for the sampler heap
--     /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11397# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeTensorARM@ using that mapping, the
--     calculated offset for the resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-tensorDescriptorAlignment tensorDescriptorAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11300# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 4
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11301# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11302# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11304# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 8
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11305# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11306# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address pointed to by the address in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11308# For each
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps descriptor heap>
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     either directly or via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     a valid descriptor heap /must/ be bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11309# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, execution of this command /must/ not result in any descriptor
--     read accessing data outside of the user range of the respective heap
--     bound by @vkCmdBind*HeapEXT@ commands
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11372# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer or uniform
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11373# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer or storage
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11374# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, uniform texel buffer, storage buffer, or storage texel
--     buffer, that shader /must/ not access values outside of the range of
--     the buffer as specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pBindInfo-11375# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     uses an embedded sampler via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     the value of @pBindInfo->reservedRangeSize@ set for
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdBindSamplerHeapEXT'
--     /must/ be greater than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11376# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11398# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the access
--     /must/ not be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11437# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the buffer from
--     which the address in push data was queried /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11438# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11441# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11439# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11442# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minStorageBufferOffsetAlignment minStorageBufferOffsetAlignment>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11485# If a pipeline is
--     bound to the pipeline bind point used by this command, or shader is
--     bound to a shader stage used by this command, and it was created
--     with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses an acceleration structure using that mapping,
--     the address that the acceleration structure is mapped to /must/ be
--     an acceleration structure address retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' object via
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
--     or handle retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' object via
--     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-index-11450# If a shader uses
--     a sampler descriptor to sample an image as a result of this command,
--     and that sampler descriptor uses a custom border color with an index
--     defined by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT',
--     the value of
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT'::@index@
--     /must/ have been registered before this command was recorded, and
--     still be registered during the sampling operation, with an
--     identically defined color
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-protectedNoFault-11455# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     the address that the resource is mapped to /must/ have been queried
--     from a buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-protectedNoFault-11456# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     the address of the indirect memory /must/ have been queried from a
--     buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-renderPass-02684# The current
--     render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Vulkan.Core10.Handles.Pipeline' bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-subpass-02685# The subpass
--     index of the current render pass /must/ be equal to the @subpass@
--     member of the
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Vulkan.Core10.Handles.Pipeline' bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpTypeImage-07468# If any
--     shader executed by this pipeline accesses an @OpTypeImage@ variable
--     with a @Dim@ operand of @SubpassData@, it /must/ be decorated with
--     an @InputAttachmentIndex@ that corresponds to a valid input
--     attachment in the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07469# Input attachment
--     views accessed in a subpass /must/ be created with the same
--     'Vulkan.Core10.Enums.Format.Format' as the corresponding subpass
--     definition, and be created with a 'Vulkan.Core10.Handles.ImageView'
--     that is compatible with the attachment referenced by the subpass\'
--     @pInputAttachments@[@InputAttachmentIndex@] in the bound
--     'Vulkan.Core10.Handles.Framebuffer' as specified by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#compatibility-inputattachment Fragment Input Attachment Compatibility>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthInputAttachmentIndex-09595#
--     Input attachment views accessed in a dynamic render pass with a
--     @InputAttachmentIndex@ referenced by
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo',
--     or no @InputAttachmentIndex@ if
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pDepthInputAttachmentIndex@
--     or
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pStencilInputAttachmentIndex@
--     are @NULL@, /must/ be created with a
--     'Vulkan.Core10.Handles.ImageView' that is compatible with the
--     corresponding color, depth, or stencil attachment in
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthInputAttachmentIndex-09596#
--     Input attachment views accessed in a dynamic render pass via a
--     shader object /must/ have an @InputAttachmentIndex@ if both
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pDepthInputAttachmentIndex@
--     and
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pStencilInputAttachmentIndex@
--     are non-@NULL@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-InputAttachmentIndex-09597# If
--     an input attachment view accessed in a dynamic render pass via a
--     shader object has an @InputAttachmentIndex@, the
--     @InputAttachmentIndex@ /must/ match an index in
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-06537# Memory backing
--     image subresources used as attachments in the current render pass
--     /must/ not be written in any way other than as an attachment by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10795# If a color
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' on
--     that attachment, it /must/ not be accessed in any way other than as
--     an attachment by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10796# If a depth
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' on
--     that attachment, it /must/ not be accessed in any way other than as
--     an attachment by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10797# If a stencil
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     on that attachment, it /must/ not be accessed in any way other than
--     as an attachment by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-12338# If a color
--     attachment is read in this command in any way other than as an
--     attachment, or has been read by any prior command in this subpass as
--     a non-attachment, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' on
--     that attachment, the color attachment /must/ not be written to by
--     this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-12339# If a depth
--     attachment is read in this command in any way other than as an
--     attachment, or has been read by any prior command in this subpass as
--     a non-attachment, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' on
--     that attachment, the depth attachment /must/ not be written to by
--     this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-12340# If a stencil
--     attachment is read in this command in any way other than as an
--     attachment, or has been read by any prior command in this subpass as
--     a non-attachment, and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     is not enabled for
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     on that attachment, the stencil attachment /must/ not be written to
--     by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09003# If an attachment
--     is written by any prior command in this subpass or by the load,
--     store, or resolve operations for this subpass, it /must/ not be
--     accessed in any way other than as an attachment, storage image, or
--     sampled image by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-06886# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-06887# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the stencil aspect, both front and back
--     @writeMask@ are not zero, and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07831# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07832# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' /must/ have been
--     called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08617# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' dynamic
--     state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line topology class, then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07834# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS' dynamic
--     state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthBiasEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' or
--     'Vulkan.Extensions.VK_EXT_depth_bias_control.cmdSetDepthBias2EXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07835# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and an active color
--     attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @blendEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE' with a
--     blend equations where any
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' member is
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA',
--     then 'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07836# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthBoundsTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' /must/
--     have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07837# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_COMPARE_MASK'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stencilTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07838# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stencilTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07839# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_REFERENCE'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of and @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stencilTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-02689#
--     If the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE', then the active depth
--     attachment /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07634# If the
--     @VK_EXT_sample_locations@ extension is enabled, a shader object is
--     bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-06666# If the
--     @VK_EXT_sample_locations@ extension is enabled, a shader object is
--     bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07840# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE' dynamic
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetCullMode'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07841# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE' dynamic
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetFrontFace'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07843# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE',
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07844# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07845# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07846# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthBounds depthBounds>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07847# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07848# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP' dynamic
--     state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stencilTestEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-03417# If a
--     shader object is bound to any graphics stage or a graphics pipeline
--     is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and the state is not inherited, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-scissorCount-03418# If a
--     shader object is bound to any graphics stage or a graphics pipeline
--     is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, and the state is not inherited, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-03419# If a
--     shader object is bound to any graphics stage or a graphics pipeline
--     is bound which was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled, and the state is not inherited, then the
--     @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04137# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04138# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @viewportWScalingEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08636# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @viewportWScalingEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the @viewportCount@
--     parameter in the last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-04139# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-shadingRateImage-09233# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV'
--     and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-shadingRateImage-09234# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @shadingRateImageEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08637# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @shadingRateImageEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the @viewportCount@
--     parameter in the last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.GraphicsPipeline.PipelineViewportStateCreateInfo',
--     then the bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.GraphicsPipeline.PipelineViewportStateCreateInfo',
--     then the bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07878# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusiveScissor>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_ENABLE_NV'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07879# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-exclusiveScissor exclusiveScissor>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--     dynamic state enabled, and the most recent call to
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorEnableNV'
--     in the current command buffer set any element of
--     @pExclusiveScissorEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04876# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04877# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-logicOp-04878# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @logicOpEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveFragmentShadingRateWithMultipleViewports-08642#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, and any shader object bound to a graphics
--     stage writes to the @PrimitiveShadingRateKHR@ built-in, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-blendEnable-04727# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then for each color
--     attachment, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the corresponding
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @blendEnable@ /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08644# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and none of the following is
--     enabled:
--
--     -   the @VK_AMD_mixed_attachment_samples@ extension
--
--     -   the @VK_NV_framebuffer_mixed_samples@ extension
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--         feature
--
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizationSamples@ /must/ be the same as the current color
--     and\/or depth\/stencil attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08876# If a shader object
--     is bound to any graphics stage, the current render pass instance
--     /must/ have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06172# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06173# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06174# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06175# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06176# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06177# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewMask-06178# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound graphics pipeline /must/ have been created with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-colorAttachmentCount-06179# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound graphics pipeline /must/ have been created with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08910#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08912#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @imageView@ equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the bound pipeline equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08911#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled, and the current render pass instance was begun
--     with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the bound graphics pipeline, or the corresponding
--     element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@,
--     if it exists, /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-colorAttachmentCount-09362# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, there is no shader object bound to any graphics stage,
--     and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID',
--     each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @resolveImageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with an image created with a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value equal to the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09363# If there is no
--     shader object bound to any graphics stage, the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID',
--     each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with an image created with a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value equal to the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09364# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the bound
--     graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have set the blend enable to
--     'Vulkan.Core10.FundamentalTypes.FALSE' prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09365# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the bound
--     graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have set @rasterizationSamples@ to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' prior
--     to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09366# If there is a
--     shader object bound to any graphics stage, and the current render
--     pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have set blend enable to
--     'Vulkan.Core10.FundamentalTypes.FALSE' prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizationSamples-09367# If
--     there is a shader object bound to any graphics stage, and the
--     current render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have set @rasterizationSamples@ to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' prior
--     to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09368# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the bound
--     graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->width@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09369# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, and the bound
--     graphics pipeline was created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value and with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->height@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pFragmentSize-09370# If there
--     is a shader object bound to any graphics stage, and the current
--     render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->width@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pFragmentSize-09371# If there
--     is a shader object bound to any graphics stage, and the current
--     render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->height@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07749# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled, a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-attachmentCount-07750# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled, a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the @attachmentCount@
--     parameter of most recent call to
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     in the current command buffer /must/ be greater than or equal to the
--     number of active color attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07751# If the
--     @VK_EXT_discard_rectangles@ extension is enabled, a graphics
--     pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     dynamic state enabled and the @pNext@ chain of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' included
--     a
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--     structure, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @discardRectangleEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for each
--     discard rectangle in
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'::@discardRectangleCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizerDiscardEnable-09236#
--     If the @VK_EXT_discard_rectangles@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     dynamic state enabled and the @pNext@ chain of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' did not
--     include a
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--     structure, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @discardRectangleEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for each
--     discard rectangle in
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PhysicalDeviceDiscardRectanglePropertiesEXT'::@maxDiscardRectangles@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07880# If the
--     @VK_EXT_discard_rectangles@ extension is enabled, a shader object is
--     bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07881# If the
--     @VK_EXT_discard_rectangles@ extension is enabled, a shader object is
--     bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_MODE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @discardRectangleEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08913#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08914#
--     If current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08915#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline was not equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08916#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08917#
--     If current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-08918#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline was not equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06183# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingLocalRead-11797#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is enabled, the
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
--     flag is specified, and an attachment is being used as a feedback
--     loop as specified by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#rendering-attachment-input-attachment-feedback >,
--     'Vulkan.Extensions.VK_KHR_maintenance10.RenderingAttachmentFlagsInfoKHR'::@flags@
--     for that attachment /must/ include
--     'Vulkan.Extensions.VK_KHR_maintenance10.RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-imageView-06184# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-layers-10831# If the current
--     render pass instance was created with
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--     or
--     'Vulkan.Core10.Enums.RenderPassCreateFlagBits.RENDER_PASS_CREATE_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE',
--     and the bound graphics pipeline was created with
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE',
--     then the current render pass instance /must/ have a @layers@ value
--     less than or equal to
--     'Vulkan.Extensions.VK_VALVE_fragment_density_map_layered.PipelineFragmentDensityMapLayeredCreateInfoVALVE'::@maxFragmentDensityMapLayers@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-colorAttachmentCount-06185# If
--     the bound pipeline was created with a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthAttachment-06186# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound pipeline was created with a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     used to create the bound graphics pipeline /must/ be equal to the
--     sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pStencilAttachment-06187# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound pipeline was created with a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     used to create the bound graphics pipeline /must/ be equal to the
--     sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-multisampledRenderToSingleSampled-07285#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, and the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of @rasterizationSamples@ for
--     the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-multisampledRenderToSingleSampled-07286#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-multisampledRenderToSingleSampled-07287#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pNext-07935# If this command
--     is called inside a render pass instance started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and the @pNext@ chain of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--     includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-renderPass-06198# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound pipeline /must/ have been created with a
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pColorAttachments-08963# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, there is a
--     graphics pipeline bound with a fragment shader that statically
--     writes to a color attachment, the color write mask is not zero,
--     color writes are enabled, and the corresponding element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pColorAttachments-11539# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, there is a graphics
--     pipeline bound with a fragment shader that statically writes to a
--     color attachment, the color write mask is not zero, color writes are
--     enabled, and the corresponding element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@pColorAttachmentFormats@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthAttachment-08964# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, there is a
--     graphics pipeline bound, depth test is enabled, and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthAttachment-11540# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, there is a graphics
--     pipeline bound, depth test is enabled, and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@depthAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pStencilAttachment-08965# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been recorded in the render pass instance, there is a
--     graphics pipeline bound, stencil test is enabled and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pStencilAttachment-11860# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, there is a graphics
--     pipeline bound, stencil test is enabled and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@stencilAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-flags-10582# If the current
--     render pass instance was begun with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     call in @commandBuffer@, its
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@flags@
--     parameter /must/ not have
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT'
--     set unless
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_KHR'
--     is also set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07620# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClamp depthClamp>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClampEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07621# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07622# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07623# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-alphaToCoverageEnable-08919#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic state enabled, and @alphaToCoverageEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-alphaToCoverageEnable-08920#
--     If a shader object is bound to any graphics stage, and the most
--     recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     in the current command buffer set @alphaToCoverageEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07624# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07625# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToOneEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07626# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-logicOp logicOp>
--     feature is enabled, a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLogicOpEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07627# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and there are color
--     attachments bound, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07629# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and there are color
--     attachments bound, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07630# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryStreams geometryStreams>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage or a graphics pipeline is bound which was created with both a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07631# If the
--     @VK_EXT_conservative_rasterization@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07632# If the
--     @VK_EXT_conservative_rasterization@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @conservativeRasterizationMode@ is
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_OVERESTIMATE_EXT',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetExtraPrimitiveOverestimationSizeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-conservativePointAndLineRasterization-07499#
--     If the @VK_EXT_conservative_rasterization@ extension is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-conservativePointAndLineRasterization conservativePointAndLineRasterization>
--     is not supported, a shader object is bound to any graphics stage or
--     a graphics pipeline is bound, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line or point topology class, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @conservativeRasterizationMode@ /must/ be
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07633# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClipEnable depthClipEnable>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--     dynamic state, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07636# If the
--     @VK_EXT_provoking_vertex@ extension is enabled, a shader object is
--     bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetProvokingVertexModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08666# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line topology class, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08669# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line topology class, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07849# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled and a shader object is bound to any graphics
--     stage, or a bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.cmdSetLineStipple'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10608# If a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line topology class, and the current @lineRasterizationMode@
--     is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM'
--     or
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH',
--     then the current @alphaToCoverageEnable@, @alphaToOneEnable@ and
--     @sampleShadingEnable@ states /must/ all be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07639# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClipControl depthClipControl>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetDepthClipNegativeOneToOneEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09650# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClampControl depthClampControl>
--     feature is enabled, and a shader object is bound to any graphics
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_RANGE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthClampEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_depth_clamp_control.cmdSetDepthClampRangeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07640# If the
--     @VK_NV_clip_space_w_scaling@ extension is enabled, and a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportWScalingEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07641# If the
--     @VK_NV_viewport_swizzle@ extension is enabled, and a shader object
--     is bound to any graphics stage or a graphics pipeline is bound which
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07642# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07643# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @coverageToColorEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07644# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationModeNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07645# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @coverageModulationMode@ is any value other than
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.COVERAGE_MODULATION_MODE_NONE_NV',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07646# If the
--     @VK_NV_framebuffer_mixed_samples@ extension is enabled, a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @coverageModulationTableEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07647# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetShadingRateImageEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pipelineFragmentShadingRate-09238#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07648# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-representativeFragmentTest representativeFragmentTest>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRepresentativeFragmentTestEnableNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07649# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageReductionModeNV'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizationSamples-07471# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the current subpass does not use any color
--     and\/or depth\/stencil attachments, then the @rasterizationSamples@
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ follow the rules for a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-samples-07472# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state disabled, then the @samples@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ be greater or equal to the
--     'Vulkan.Core10.GraphicsPipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     parameter used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-samples-07473# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     state and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     states enabled, then the @samples@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ be greater or equal to the @rasterizationSamples@ parameter
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizationSamples-07474# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and neither the
--     @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, then the
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09211# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, or a shader object is bound to any graphics stage,
--     and the current render pass instance includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ be the same as the @rasterizationSamples@ member of that
--     structure
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-firstAttachment-07476# If a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic states enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     in the current command buffer prior to this drawing command /must/
--     have set a value for all active color attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-firstAttachment-07478# If a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic states enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     in the current command buffer prior to this drawing command /must/
--     have set a value for all active color attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-advancedBlendMaxColorAttachments-07480#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic states enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and an active color
--     attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @blendEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     number of active color attachments /must/ not exceed
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendMaxColorAttachments advancedBlendMaxColorAttachments>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10862# If a graphics
--     pipeline is bound was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     , but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for all
--     active color attachments with the @blendEnable@
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizerDiscardEnable-10863#
--     If a graphics pipeline is bound was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT',
--     but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for all
--     active color attachments with the @blendEnable@
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10864# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then either
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for all
--     active color attachments with the @blendEnable@
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitivesGeneratedQueryWithNonZeroStreams-07481#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, and the bound graphics pipeline was created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--     state enabled, the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have set the @rasterizationStream@ to zero
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsPerPixel-07482#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state disabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then the @sampleLocationsPerPixel@ member of @pSampleLocationsInfo@
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ equal the @rasterizationSamples@ member of the
--     'Vulkan.Core10.GraphicsPipeline.PipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsPerPixel-07483#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then the @sampleLocationsPerPixel@ member of @pSampleLocationsInfo@
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ equal the @rasterizationSamples@ parameter of the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07484#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT'
--     then the current active depth attachment /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07485#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.maxSampleLocationGridSize.width@ in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07486#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.maxSampleLocationGridSize.height@ in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07487#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     the fragment shader code /must/ not statically use the extended
--     instruction @InterpolateAtSample@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07936#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @sampleLocationsInfo.sampleLocationGridSize.width@ /must/
--     evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07937#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @sampleLocationsInfo.sampleLocationGridSize.height@ /must/
--     evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-sampleLocationsEnable-07938#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @sampleLocationsInfo.sampleLocationsPerPixel@ /must/ equal
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-coverageModulationTableEnable-07488#
--     If a shader object is bound to any graphics stage or the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--     state enabled, and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableEnableNV'
--     set @coverageModulationTableEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     @coverageModulationTableCount@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageModulationTableNV'
--     /must/ equal the current @rasterizationSamples@ divided by the
--     number of color samples in the current active color attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizationSamples-07489# If
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and if
--     current subpass has a depth\/stencil attachment and depth test,
--     stencil test, or depth bounds test are enabled in the bound
--     pipeline, then the current @rasterizationSamples@ /must/ be the same
--     as the sample count of the depth\/stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-coverageToColorEnable-07490#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     state enabled and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then there /must/ be an
--     active color attachment at the location selected by the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     @coverageToColorLocation@, with a
--     'Vulkan.Core10.Enums.Format.Format' of
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizerDiscardEnable-09420#
--     If the @VK_NV_fragment_coverage_to_color@ extension is enabled, and
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then there /must/ be an
--     active color attachment at the location selected by the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorLocationNV'
--     @coverageToColorLocation@, with a
--     'Vulkan.Core10.Enums.Format.Format' of
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_UINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16_SINT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_UINT', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R32_SINT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-coverageReductionMode-07491#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is enabled, a shader object is bound to any graphics stage
--     or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     dynamic states enabled, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current values>
--     of @coverageReductionMode@, @rasterizationSamples@, the sample
--     counts for the color and depth\/stencil attachments (if the subpass
--     has them) /must/ be a valid combination returned by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-07492# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-07493# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-viewportCount-09421# If the
--     @VK_NV_viewport_swizzle@ extension is enabled, and a shader object
--     is bound to any graphics stage, then the @viewportCount@ parameter
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-rasterizationSamples-07494# If
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is not enabled, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @coverageReductionMode@ is not
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.COVERAGE_REDUCTION_MODE_TRUNCATE_NV',
--     and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizationSamples@ is greater than sample count of the color
--     attachment, then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading sample shading>
--     /must/ be disabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stippledLineEnable-07495# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stippledLineEnable-07496# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stippledLineEnable-07497# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stippledLineEnable-07498# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_DEFAULT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stage-07073# If the bound
--     pipeline was created with the
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pStages@
--     set to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     /must/ not be active
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08877# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07850# If dynamic state
--     was inherited from
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
--     it /must/ be set in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-nextStage-10745# For each
--     shader object bound to a graphics stage, except for shader object
--     bound to the last graphics stage in the logical pipeline, it /must/
--     have been created with a @nextStage@ including the corresponding bit
--     to the shader object bound to the following graphics stage in the
--     logical pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08684# If there is no
--     bound graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08685# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08686# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08687# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08688# If there is no
--     bound graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08689# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08690# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08693# If there is no
--     bound graphics pipeline, and at least one of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features is enabled, one of the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stages /must/ have a valid 'Vulkan.Extensions.Handles.ShaderEXT'
--     bound, and the other /must/ have no
--     'Vulkan.Extensions.Handles.ShaderEXT' bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08696# If there is no
--     bound graphics pipeline, and a valid
--     'Vulkan.Extensions.Handles.ShaderEXT' is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, there /must/ be no 'Vulkan.Extensions.Handles.ShaderEXT'
--     bound to either the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage or the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08698# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, then all shaders created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag in the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ also be bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08699# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, any stages in between stages whose shaders which did not
--     create a shader with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag as part of the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ not have any 'Vulkan.Extensions.Handles.ShaderEXT' bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08878# All bound graphics
--     shader objects /must/ have been created with identical or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-identically-defined identically defined>
--     push constant ranges
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08879# All bound graphics
--     shader objects /must/ have either been created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag set, or with identical or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#glossary-identically-defined identically defined>
--     arrays of descriptor set layouts
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-colorAttachmentCount-09372# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID',
--     and a fragment shader is bound, it /must/ not declare the
--     @DepthReplacing@ or @StencilRefReplacingEXT@ execution modes
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDynamicStates-08715# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     set in
--     'Vulkan.Core10.GraphicsPipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpDepthAttachmentReadEXT@, the @depthWriteEnable@
--     parameter in the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDynamicStates-08716# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     set in
--     'Vulkan.Core10.GraphicsPipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpStencilAttachmentReadEXT@, the @writeMask@
--     parameter in the last call to
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' /must/
--     be @0@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09116# If a shader object
--     is bound to any graphics stage or the bound graphics pipeline was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT',
--     and the format of any color attachment is
--     'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32', the
--     corresponding element of the @pColorWriteMasks@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ either include all of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_R_BIT',
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_G_BIT',
--     and
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_B_BIT',
--     or none of them
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-maxFragmentDualSrcAttachments-09239#
--     If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blending blending>
--     is enabled for any attachment where either the source or destination
--     blend factors for that attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-dsb use the secondary color input>,
--     the maximum value of @Location@ for any output attachment
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-staticuse statically used>
--     in the @Fragment@ @Execution@ @Model@ executed by this command
--     /must/ be less than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxFragmentDualSrcAttachments maxFragmentDualSrcAttachments>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09548# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is no shader object bound to any graphics stage, the value of
--     each element of
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingAttachmentLocationInfo'::@pColorAttachmentLocations@
--     in the bound pipeline /must/ match the value for the corresponding
--     locations set currently in the current render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09549# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and there is no shader object bound to any graphics stage, the value
--     of each element of
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pColorAttachmentInputIndices@
--     in the bound pipeline /must/ match the value for the corresponding
--     index set currently in the current render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10927# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and there is no shader object bound to any graphics stage, the value
--     of
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pDepthInputAttachmentIndex@
--     in the bound pipeline /must/ match the value set currently in the
--     current render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10928# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and there is no shader object bound to any graphics stage, the value
--     of
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@pStencilInputAttachmentIndex@
--     in the bound pipeline /must/ match the value set currently in the
--     current render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09642# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with the
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--     flag, the bound graphics pipeline /must/ have been created with
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09643# If the bound
--     graphics pipeline was created with
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT',
--     the current render pass /must/ have begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with the
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--     flag
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10677# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tileShadingPerTileDraw tileShadingPerTileDraw>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10772# If a shader object
--     is bound to any graphics stage, /multiview/ functionality /must/ not
--     be enabled in the current render pass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-multiviewPerViewViewports-12262#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, then the index of the most significant bit in
--     current render pass instance @viewMask@ /must/ be less than the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @viewportCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-multiviewPerViewViewports-12263#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, then the index of the most significant bit in
--     current render pass instance @viewMask@ /must/ be less than the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @scissorCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-flags-11521# If current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@flags@
--     which includes
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_FRAGMENT_REGION_BIT_EXT',
--     and if
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading sample shading>
--     is enabled (explicitly or implicitly), then the minimum fraction for
--     sample shading /must/ equal 0.0
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11522# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and contains a custom resolve, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, the graphics pipeline bound /must/ have been
--     created with a
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11523# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and does not contain a custom resolve, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, the graphics pipeline bound /must/ not have
--     been created with a
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-customResolve-11524# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, the graphics pipeline
--     bound /must/ have been created with
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     as 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-customResolve-11525# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and contains a custom resolve, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not been recorded in the render pass instance, the graphics
--     pipeline bound /must/ have been created with
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     as 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11861# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and contains a custom resolve, the bound graphics pipeline /must/
--     have been created with a
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11862# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @resolveImageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@pColorAttachmentFormats@
--     used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11863# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @resolveImageView@ equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have the
--     corresponding element of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@pColorAttachmentFormats@
--     used to create the bound pipeline equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-11864#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled, the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with an @resolveImageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@pColorAttachmentFormats@
--     used to create the bound graphics pipeline, or the corresponding
--     element of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@pColorAttachmentFormats@,
--     if it exists, /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11865# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11866# If current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-11867#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@depthAttachmentFormat@
--     used to create the bound graphics pipeline was not equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11868# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-11869# If current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline /must/ be equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicRenderingUnusedAttachments-11870#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it contains a custom resolve, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is enabled,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the value of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@stencilAttachmentFormat@
--     used to create the bound graphics pipeline was not equal to the
--     'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@,
--     the value of the format /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-colorAttachmentCount-11871# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@ and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, then for each element
--     of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @resolveImageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the @resolveImageView@
--     /must/ have been created with a sample count equal to the value of
--     @rasterizationSamples@ for the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pDepthAttachment-11872# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->resolveImageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pStencilAttachment-11873# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been recorded in the render pass instance,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->resolveImageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-customResolve-11529# If a
--     shader object is bound to the fragment stage, the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     a fragment density map attachment is active, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has been called, then the fragment shader object bound /must/ have
--     been created with
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     as 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-customResolve-11530# If a
--     shader object is bound to the fragment stage, the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and contains a custom resolve, a fragment density map attachment is
--     active, and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.cmdBeginCustomResolveEXT'
--     has not yet been called, then the fragment shader object bound
--     /must/ have been created with
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     as 'Vulkan.Core10.FundamentalTypes.FALSE'
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-02721# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and that pipeline was created without
--     enabling
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     for @vertexInputs@, then for a given vertex buffer binding, any
--     attribute data fetched /must/ be entirely contained within the
--     corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-format-10389# For each vertex
--     attribute accessed by this command, if its
--     'Vulkan.Core10.GraphicsPipeline.VertexInputAttributeDescription'::@format@
--     or
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-packed packed format>,
--     and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-legacyVertexAttributes legacyVertexAttributes>
--     feature is not enabled, the value of @attribAddress@, calculated as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-address-calculation Vertex Input Calculation>,
--     /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats size of the format>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-format-10390# For each vertex
--     attribute accessed by this command, if its
--     'Vulkan.Core10.GraphicsPipeline.VertexInputAttributeDescription'::@format@
--     or
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-packed packed format>,
--     and either the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-legacyVertexAttributes legacyVertexAttributes>
--     feature is not enabled or @format@ has 64-bit components, the value
--     of @attribAddress@, calculated as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-address-calculation Vertex Input Calculation>,
--     /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats component size of the format>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07842# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-dynamicPrimitiveTopologyUnrestricted-07500#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state enabled and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then the
--     @primitiveTopology@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.GraphicsPipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveTopology-10286# If a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage is bound, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @primitiveTopology@ /must/ be
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--     prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveTopology-10747# If
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     set @primitiveTopology@ to
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--     prior to this drawing command, then a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage /must/ be bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-primitiveTopology-10748# If
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     set @primitiveTopology@ to
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST'
--     prior to this drawing command, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled, both a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage are not bound, then the @Vertex@ @Execution@ @Model@ /must/
--     have a @PointSize@ decorated variable that is statically written to
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-pStrides-04913# If the bound
--     graphics pipeline was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE'
--     dynamic state enabled, but without the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this draw command, and the
--     @pStrides@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04914# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this draw command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-Input-07939# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-vertexAttributeRobustness vertexAttributeRobustness>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is not enabled, and there is a shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled then all variables with the @Input@ storage
--     class decorated with @Location@ in the @Vertex@ @Execution@ @Model@
--     @OpEntryPoint@ /must/ contain a location in
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@location@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-Input-08734# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and either the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-legacyVertexAttributes legacyVertexAttributes>
--     feature is not enabled or the SPIR-V Type associated with a given
--     @Input@ variable of the corresponding @Location@ in the @Vertex@
--     @Execution@ @Model@ @OpEntryPoint@ is 64-bit, then the numeric type
--     associated with all @Input@ variables of the corresponding
--     @Location@ in the @Vertex@ @Execution@ @Model@ @OpEntryPoint@ /must/
--     be the same as
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-format-08936# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     has a 64-bit component, then the scalar width associated with all
--     @Input@ variables of the corresponding @Location@ in the @Vertex@
--     @Execution@ @Model@ @OpEntryPoint@ /must/ be 64-bit
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-format-08937# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and the scalar width associated with a
--     @Location@ decorated @Input@ variable in the @Vertex@ @Execution@
--     @Model@ @OpEntryPoint@ is 64-bit, then the corresponding
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     /must/ have a 64-bit component
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09203# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled and
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputAttributeDescription2EXT'::@format@
--     has a 64-bit component, then all @Input@ variables at the
--     corresponding @Location@ in the @Vertex@ @Execution@ @Model@
--     @OpEntryPoint@ /must/ not use components that are not present in the
--     format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04875# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage or the bound graphics pipeline state was created with both a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @primitiveTopology@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-04879# If there is a
--     shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-09637# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveTopologyListRestart primitiveTopologyListRestart>
--     feature is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-vertex-input-assembler-topology input assembly>
--     is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     or
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY',
--     there is a shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable'
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-10909# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveTopologyPatchListRestart primitiveTopologyPatchListRestart>
--     feature is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-vertex-input-assembler-topology input assembly>
--     is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     there is a shader object bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     stage or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable'
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-stage-06481# The bound
--     graphics pipeline /must/ not have been created with the
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of any element of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pStages@
--     set to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-08885# There /must/ be no
--     shader object bound to either of the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stages
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-None-07619# If a shader object
--     is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage or a graphics pipeline is bound which was created with both a
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpExecutionMode-12239# If a
--     shader is bound to both the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stages, and if both stages contain an @OpExecutionMode@ instruction
--     specifying the type of subdivision, they /must/ be the same
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpExecutionMode-12240# If a
--     shader is bound to both the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stages, and if both stages contain an @OpExecutionMode@ instruction
--     specifying the orientation of triangles, they /must/ be the same
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpExecutionMode-12241# If a
--     shader is bound to both the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stages, and if both stages contain an @OpExecutionMode@ instruction
--     specifying the segment spacing, they /must/ be the same
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-OpExecutionMode-12242# If a
--     shader is bound to both the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stages, and if both stages contain an @OpExecutionMode@ instruction
--     specifying the output patch size, they /must/ be the same
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-commandBuffer-02970#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-isPreprocessed-02908# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then
--     'cmdPreprocessGeneratedCommandsNV' /must/ have already been executed
--     on the device, using the same @pGeneratedCommandsInfo@ content as
--     well as the content of the input buffers it references (all except
--     'GeneratedCommandsInfoNV'::@preprocessBuffer@). Furthermore,
--     @pGeneratedCommandsInfo@’s @indirectCommandsLayout@ /must/ have been
--     created with the
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
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
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-renderpass# This command
--     /must/ only be called inside of a render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-suspended# This command /must/
--     not be called between suspended render pass instances
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 | Indirection                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdExecuteGeneratedCommandsNV is affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
                              -> -- | @pGeneratedCommandsInfo@ is a pointer to a 'GeneratedCommandsInfoNV'
                                 -- structure containing parameters affecting the generation of commands.
                                 GeneratedCommandsInfoNV
                              -> io ()
cmdExecuteGeneratedCommandsNV commandBuffer
                                isPreprocessed
                                generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdExecuteGeneratedCommandsNVPtr = pVkCmdExecuteGeneratedCommandsNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdExecuteGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdExecuteGeneratedCommandsNV' = mkVkCmdExecuteGeneratedCommandsNV vkCmdExecuteGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdExecuteGeneratedCommandsNV" (vkCmdExecuteGeneratedCommandsNV'
                                                               (commandBufferHandle (commandBuffer))
                                                               (boolToBool32 (isPreprocessed))
                                                               pGeneratedCommandsInfo)
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
--     @pGeneratedCommandsInfo->indirectCommandsLayout@ /must/ have been
--     created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV' bit set
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-deviceGeneratedCommands-02928#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
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
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPreprocessGeneratedCommandsNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoNV'
cmdPreprocessGeneratedCommandsNV :: forall io
                                  . (MonadIO io)
                                 => -- | @commandBuffer@ is the command buffer which does the preprocessing.
                                    CommandBuffer
                                 -> -- | @pGeneratedCommandsInfo@ is a pointer to a 'GeneratedCommandsInfoNV'
                                    -- structure containing parameters affecting the preprocessing step.
                                    GeneratedCommandsInfoNV
                                 -> io ()
cmdPreprocessGeneratedCommandsNV commandBuffer
                                   generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdPreprocessGeneratedCommandsNVPtr = pVkCmdPreprocessGeneratedCommandsNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPreprocessGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPreprocessGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdPreprocessGeneratedCommandsNV' = mkVkCmdPreprocessGeneratedCommandsNV vkCmdPreprocessGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdPreprocessGeneratedCommandsNV" (vkCmdPreprocessGeneratedCommandsNV'
                                                                  (commandBufferHandle (commandBuffer))
                                                                  pGeneratedCommandsInfo)
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
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
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindPipelineShaderGroupNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindPipelineShaderGroupNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
cmdBindPipelineShaderGroupNV commandBuffer
                               pipelineBindPoint
                               pipeline
                               groupIndex = liftIO $ do
  let vkCmdBindPipelineShaderGroupNVPtr = pVkCmdBindPipelineShaderGroupNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBindPipelineShaderGroupNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipelineShaderGroupNV is null" Nothing Nothing
  let vkCmdBindPipelineShaderGroupNV' = mkVkCmdBindPipelineShaderGroupNV vkCmdBindPipelineShaderGroupNVPtr
  traceAroundEvent "vkCmdBindPipelineShaderGroupNV" (vkCmdBindPipelineShaderGroupNV'
                                                       (commandBufferHandle (commandBuffer))
                                                       (pipelineBindPoint)
                                                       (pipeline)
                                                       (groupIndex))
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetGeneratedCommandsMemoryRequirementsNV-pInfo-09074# If
--     @pInfo->pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCompute ::deviceGeneratedCompute>
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Device',
-- 'GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getGeneratedCommandsMemoryRequirementsNV :: forall a io
                                          . ( Extendss MemoryRequirements2 a
                                            , PokeChain a
                                            , PeekChain a
                                            , MonadIO io )
                                         => -- | @device@ is the logical device that owns the buffer.
                                            Device
                                         -> -- | @pInfo@ is a pointer to a 'GeneratedCommandsMemoryRequirementsInfoNV'
                                            -- structure containing parameters required for the memory requirements
                                            -- query.
                                            GeneratedCommandsMemoryRequirementsInfoNV
                                         -> io (MemoryRequirements2 a)
getGeneratedCommandsMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetGeneratedCommandsMemoryRequirementsNVPtr = pVkGetGeneratedCommandsMemoryRequirementsNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetGeneratedCommandsMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGeneratedCommandsMemoryRequirementsNV is null" Nothing Nothing
  let vkGetGeneratedCommandsMemoryRequirementsNV' = mkVkGetGeneratedCommandsMemoryRequirementsNV vkGetGeneratedCommandsMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetGeneratedCommandsMemoryRequirementsNV" (vkGetGeneratedCommandsMemoryRequirementsNV'
                                                                          (deviceHandle (device))
                                                                          pInfo
                                                                          (forgetExtensions (pPMemoryRequirements)))
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
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
-- -   #VUID-vkCreateIndirectCommandsLayoutNV-device-queuecount# The device
--     /must/ have been created with at least @1@ queue
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV'
createIndirectCommandsLayoutNV :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that creates the indirect command layout.
                                  Device
                               -> -- | @pCreateInfo@ is a pointer to a 'IndirectCommandsLayoutCreateInfoNV'
                                  -- structure containing parameters affecting creation of the indirect
                                  -- command layout.
                                  IndirectCommandsLayoutCreateInfoNV
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (IndirectCommandsLayoutNV)
createIndirectCommandsLayoutNV device
                                 createInfo
                                 allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutNVPtr = pVkCreateIndirectCommandsLayoutNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkCreateIndirectCommandsLayoutNV' = mkVkCreateIndirectCommandsLayoutNV vkCreateIndirectCommandsLayoutNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutNV 8) free
  r <- lift $ traceAroundEvent "vkCreateIndirectCommandsLayoutNV" (vkCreateIndirectCommandsLayoutNV'
                                                                     (deviceHandle (device))
                                                                     pCreateInfo
                                                                     pAllocator
                                                                     (pPIndirectCommandsLayout))
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
                                   -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyIndirectCommandsLayoutNV device
                                  indirectCommandsLayout
                                  allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutNVPtr = pVkDestroyIndirectCommandsLayoutNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkDestroyIndirectCommandsLayoutNV' = mkVkDestroyIndirectCommandsLayoutNV vkDestroyIndirectCommandsLayoutNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyIndirectCommandsLayoutNV" (vkDestroyIndirectCommandsLayoutNV'
                                                                 (deviceHandle (device))
                                                                 (indirectCommandsLayout)
                                                                 pAllocator)
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV"
pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV = PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT


-- No documentation found for TopLevel "VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_NV"
pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_NV = ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT


-- No documentation found for TopLevel "VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV"
pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV = ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT


-- | VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV - Structure describing
-- the device-generated commands features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDeviceGeneratedCommandsFeaturesNV', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
  { -- | #features-deviceGeneratedCommandsNV# @deviceGeneratedCommands@ indicates
    -- whether the implementation supports functionality to generate commands
    -- on the device. See
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedCommands :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  { -- | @maxGraphicsShaderGroupCount@ is the maximum number of shader groups in
    -- 'GraphicsPipelineShaderGroupsCreateInfoNV'.
    maxGraphicsShaderGroupCount :: Word32
  , -- | @maxIndirectSequenceCount@ is the maximum number of sequences in
    -- 'GeneratedCommandsInfoNV' and in
    -- 'GeneratedCommandsMemoryRequirementsInfoNV'.
    maxIndirectSequenceCount :: Word32
  , -- | @maxIndirectCommandsTokenCount@ is the maximum number of tokens in
    -- 'IndirectCommandsLayoutCreateInfoNV'.
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
  , -- | @minSequencesCountBufferOffsetAlignment@ is the minimum alignment for
    -- memory addresses which /can/ be used in 'GeneratedCommandsInfoNV'.
    minSequencesCountBufferOffsetAlignment :: Word32
  , -- | @minSequencesIndexBufferOffsetAlignment@ is the minimum alignment for
    -- memory addresses which /can/ be used in 'GeneratedCommandsInfoNV'.
    minSequencesIndexBufferOffsetAlignment :: Word32
  , -- | @minIndirectCommandsBufferOffsetAlignment@ is the minimum alignment for
    -- memory addresses used in 'IndirectCommandsStreamNV', and as preprocess
    -- buffer in 'GeneratedCommandsInfoNV'.
    minIndirectCommandsBufferOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
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
             maxGraphicsShaderGroupCount
             maxIndirectSequenceCount
             maxIndirectCommandsTokenCount
             maxIndirectCommandsStreamCount
             maxIndirectCommandsTokenOffset
             maxIndirectCommandsStreamStride
             minSequencesCountBufferOffsetAlignment
             minSequencesIndexBufferOffsetAlignment
             minIndirectCommandsBufferOffsetAlignment

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
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@stageCount@
--     apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pStages-02889# For
--     @pStages@, the same restrictions as in
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pStages@
--     apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pVertexInputState-02890# For
--     @pVertexInputState@, the same restrictions as in
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pVertexInputState@
--     apply
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-pTessellationState-02891#
--     For @pTessellationState@, the same restrictions as in
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pTessellationState@
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
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   #VUID-VkGraphicsShaderGroupCreateInfoNV-stageCount-arraylength#
--     @stageCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineTessellationStateCreateInfo',
-- 'Vulkan.Core10.GraphicsPipeline.PipelineVertexInputStateCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsShaderGroupCreateInfoNV = GraphicsShaderGroupCreateInfoNV
  { -- | @pStages@ is a pointer to an array
    -- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo' structures
    -- specifying the set of the shader stages to be included in this shader
    -- group.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pVertexInputState@ is a pointer to a
    -- 'Vulkan.Core10.GraphicsPipeline.PipelineVertexInputStateCreateInfo'
    -- structure.
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- | @pTessellationState@ is a pointer to a
    -- 'Vulkan.Core10.GraphicsPipeline.PipelineTessellationStateCreateInfo'
    -- structure, and is ignored if the shader group does not include a
    -- tessellation control shader stage and tessellation evaluation shader
    -- stage.
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsShaderGroupCreateInfoNV)
#endif
deriving instance Show GraphicsShaderGroupCreateInfoNV

instance ToCStruct GraphicsShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsShaderGroupCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytes @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48)
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
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'
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
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV'
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pGroups-parameter#
--     If @groupCount@ is not @0@, @pGroups@ /must/ be a valid pointer to
--     an array of @groupCount@ valid 'GraphicsShaderGroupCreateInfoNV'
--     structures
--
-- -   #VUID-VkGraphicsPipelineShaderGroupsCreateInfoNV-pPipelines-parameter#
--     If @pipelineCount@ is not @0@, @pPipelines@ /must/ be a valid
--     pointer to an array of @pipelineCount@ valid
--     'Vulkan.Core10.Handles.Pipeline' handles
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'GraphicsShaderGroupCreateInfoNV', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsPipelineShaderGroupsCreateInfoNV = GraphicsPipelineShaderGroupsCreateInfoNV
  { -- | @pGroups@ is a pointer to an array of 'GraphicsShaderGroupCreateInfoNV'
    -- structures specifying which state of the original
    -- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo' each shader
    -- group overrides.
    groups :: Vector GraphicsShaderGroupCreateInfoNV
  , -- | @pPipelines@ is a pointer to an array of graphics
    -- 'Vulkan.Core10.Handles.Pipeline' structures which are referenced within
    -- the created pipeline, including all their shader groups.
    pipelines :: Vector Pipeline
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineShaderGroupsCreateInfoNV)
#endif
deriving instance Show GraphicsPipelineShaderGroupsCreateInfoNV

instance ToCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineShaderGroupsCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytes @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 48)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelines)) :: Word32))
    pPPipelines' <- ContT $ allocaBytes @Pipeline ((Data.Vector.length (pipelines)) * 8)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
data BindShaderGroupIndirectCommandNV = BindShaderGroupIndirectCommandNV
  { -- | @groupIndex@ specifies which shader group of the current bound graphics
    -- pipeline is used.
    groupIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindShaderGroupIndirectCommandNV)
#endif
deriving instance Show BindShaderGroupIndirectCommandNV

instance ToCStruct BindShaderGroupIndirectCommandNV where
  withCStruct x f = allocaBytes 4 $ \p -> pokeCStruct p x (f p)
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
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-bufferAddress-parameter#
--     @bufferAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-VkBindIndexBufferIndirectCommandNV-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
    -- custom @uint32_t@ value /can/ be mapped to
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindVertexBufferIndirectCommandNV-bufferAddress-parameter#
--     @bufferAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
data BindVertexBufferIndirectCommandNV = BindVertexBufferIndirectCommandNV
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as vertex input binding.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @stride@ is the byte size stride for this vertex input binding as in
    -- 'Vulkan.Core10.GraphicsPipeline.VertexInputBindingDescription'::@stride@.
    -- It is only used if
    -- 'IndirectCommandsLayoutTokenNV'::@vertexDynamicStride@ was set,
    -- otherwise the stride is inherited from the current bound graphics
    -- pipeline.
    stride :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindVertexBufferIndirectCommandNV)
#endif
deriving instance Show BindVertexBufferIndirectCommandNV

instance ToCStruct BindVertexBufferIndirectCommandNV where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
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
  withCStruct x f = allocaBytes 4 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-offset-06888# @offset@ /must/
--     be aligned to the scalar alignment of @tokenType@ or
--     @minIndirectCommandsBufferOffsetAlignment@, whichever is lower
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
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@stageFlags@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-02984# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV',
--     @indirectStateFlags@ /must/ not be @0@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-tokenType-11334# If
--     @tokenType@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.IndirectCommandsLayoutPushDataTokenNV'::@pushDataSize@
--     /must/ be greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.IndirectCommandsLayoutPushDataTokenNV'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenNV-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'IndirectCommandsLayoutCreateInfoNV', 'IndirectCommandsTokenTypeNV',
-- 'IndirectStateFlagsNV', 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectCommandsLayoutTokenNV (es :: [Type]) = IndirectCommandsLayoutTokenNV
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @tokenType@ is a 'IndirectCommandsTokenTypeNV' specifying the token
    -- command type.
    tokenType :: IndirectCommandsTokenTypeNV
  , -- | @stream@ is the index of the input stream containing the token argument
    -- data.
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
  , -- | @indirectStateFlags@ is a 'IndirectStateFlagsNV' bitfield indicating the
    -- active states for the state flag command.
    indirectStateFlags :: IndirectStateFlagsNV
  , -- | @pIndexTypes@ is the used 'Vulkan.Core10.Enums.IndexType.IndexType' for
    -- the corresponding @uint32_t@ value entry in @pIndexTypeValues@.
    indexTypes :: Vector IndexType
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pIndexTypeValues"
    indexTypeValues :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutTokenNV (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (IndirectCommandsLayoutTokenNV es)

instance Extensible IndirectCommandsLayoutTokenNV where
  extensibleTypeName = "IndirectCommandsLayoutTokenNV"
  setNext IndirectCommandsLayoutTokenNV{..} next' = IndirectCommandsLayoutTokenNV{next = next', ..}
  getNext IndirectCommandsLayoutTokenNV{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends IndirectCommandsLayoutTokenNV e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @IndirectCommandsLayoutPushDataTokenNV = Just f
    | otherwise = Nothing

instance ( Extendss IndirectCommandsLayoutTokenNV es
         , PokeChain es ) => ToCStruct (IndirectCommandsLayoutTokenNV es) where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
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
    pPIndexTypes' <- ContT $ allocaBytes @IndexType ((Data.Vector.length (indexTypes)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (indexTypes)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (indexTypeValues)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indexTypeValues)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss IndirectCommandsLayoutTokenNV es
         , PeekChain es ) => FromCStruct (IndirectCommandsLayoutTokenNV es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
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
             next
             tokenType
             stream
             offset
             vertexBindingUnit
             (bool32ToBool vertexDynamicStride)
             pushconstantPipelineLayout
             pushconstantShaderStageFlags
             pushconstantOffset
             pushconstantSize
             indirectStateFlags
             pIndexTypes'
             pIndexTypeValues'

instance es ~ '[] => Zero (IndirectCommandsLayoutTokenNV es) where
  zero = IndirectCommandsLayoutTokenNV
           ()
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
-- When tokens are consumed, an offset is computed based on token offset
-- and stream stride. The resulting offset is required to be aligned. The
-- alignment for a specific token is equal to the scalar alignment of the
-- data type as defined in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-alignment-requirements Alignment Requirements>,
-- or
-- 'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@minIndirectCommandsBufferOffsetAlignment@,
-- whichever is lower.
--
-- A @minIndirectCommandsBufferOffsetAlignment@ of 4 allows
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' to be packed as @uvec2@
-- with scalar layout instead of @uint64_t@ with 8 byte alignment. This
-- enables direct compatibility with D3D12 command signature layouts.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-02930#
--     The @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     or
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
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
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-09585# If
--     @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV' it /must/ be the first
--     element of the array and there /must/ be only a single element of
--     such token type
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02933# If
--     @pTokens@ contains an entry of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV' there /must/ be only a
--     single element of such token type
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02934# All state
--     tokens in @pTokens@ /must/ occur before any action command tokens
--     ('INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV' ,
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV' )
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pTokens-02935# The
--     content of @pTokens@ /must/ include one single action command token
--     that is compatible with the @pipelineBindPoint@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-streamCount-02936#
--     @streamCount@ /must/ be greater than @0@ and less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamCount@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pStreamStrides-02937#
--     each element of @pStreamStrides@ /must/ be greater than @0@ and less
--     than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesNV'::@maxIndirectCommandsStreamStride@.
--     Furthermore the alignment of each token input /must/ be ensured
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-09088#
--     If @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCompute ::deviceGeneratedCompute>
--     feature /must/ be enabled
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-09089#
--     If @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--     then the state tokens in @pTokens@ /must/ only include
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV', or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoNV-pipelineBindPoint-09090#
--     If @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--     and @pTokens@ includes 'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
    tokens :: Vector (SomeStruct IndirectCommandsLayoutTokenNV)
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
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytes @(IndirectCommandsLayoutTokenNV _) ((Data.Vector.length (tokens)) * 88)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPTokens' `plusPtr` (88 * (i)) :: Ptr (IndirectCommandsLayoutTokenNV _))) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (IndirectCommandsLayoutTokenNV _)))) (pPTokens')
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streamStrides)) :: Word32))
    pPStreamStrides' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (streamStrides)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (streamStrides)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    f

instance FromCStruct IndirectCommandsLayoutCreateInfoNV where
  peekCStruct p = do
    flags <- peek @IndirectCommandsLayoutUsageFlagsNV ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    tokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pTokens <- peek @(Ptr (IndirectCommandsLayoutTokenNV _)) ((p `plusPtr` 32 :: Ptr (Ptr (IndirectCommandsLayoutTokenNV _))))
    pTokens' <- generateM (fromIntegral tokenCount) (\i -> peekSomeCStruct (forgetExtensions ((pTokens `advancePtrBytes` (88 * (i)) :: Ptr (IndirectCommandsLayoutTokenNV _)))))
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
--     set in
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@flags@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-02915# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV', then the
--     @pipeline@’s 'Vulkan.Core10.Handles.PipelineLayout' /must/ match the
--     'IndirectCommandsLayoutTokenNV'::@pushconstantPipelineLayout@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-streamCount-02916# @streamCount@
--     /must/ match the @indirectCommandsLayout@’s @streamCount@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipelineBindPoint-09084# If
--     @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     then the @pipeline@ /must/ have been created with the flag
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipelineBindPoint-09085# If
--     @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     then the @pipeline@ /must/ have been created with a
--     'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV'
--     structure specifying a valid address where its metadata will be
--     saved
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipelineBindPoint-09086# If
--     @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     then
--     'Vulkan.Extensions.VK_NV_device_generated_commands_compute.cmdUpdatePipelineIndirectBufferNV'
--     /must/ have been called on that pipeline to save its metadata to a
--     device address
--
-- -   #VUID-VkGeneratedCommandsInfoNV-pipelineBindPoint-09087# If
--     @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     and if 'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV' is used, then
--     @pipeline@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
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
--     requirement’s size returned by
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
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-07078# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV', then the @pipeline@
--     /must/ contain a shader stage using the @MeshNV@ @Execution@ @Model@
--
-- -   #VUID-VkGeneratedCommandsInfoNV-indirectCommandsLayout-07079# If the
--     @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV', then the
--     @pipeline@ /must/ contain a shader stage using the @MeshEXT@
--     @Execution@ @Model@
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
-- -   #VUID-VkGeneratedCommandsInfoNV-pipeline-parameter# If @pipeline@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @pipeline@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Pipeline' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
  , -- | @pStreams@ is a pointer to an array of @streamCount@
    -- 'IndirectCommandsStreamNV' structures providing the input data for the
    -- tokens used in @indirectCommandsLayout@.
    streams :: Vector IndirectCommandsStreamNV
  , -- | @sequencesCount@ is the maximum number of sequences to reserve. If
    -- @sequencesCountBuffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', this
    -- is also the actual number of sequences generated.
    sequencesCount :: Word32
  , -- | @preprocessBuffer@ is the 'Vulkan.Core10.Handles.Buffer' that is used
    -- for preprocessing the input data for execution. If this structure is
    -- used with 'cmdExecuteGeneratedCommandsNV' with its @isPreprocessed@ set
    -- to 'Vulkan.Core10.FundamentalTypes.TRUE', then the preprocessing step is
    -- skipped and data in this buffer will not be modified. The contents and
    -- the layout of this buffer are opaque to applications and /must/ not be
    -- modified outside functions related to device-generated commands or
    -- copied to another buffer for reuse.
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
  withCStruct x f = allocaBytes 120 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streams)) :: Word32))
    pPStreams' <- ContT $ allocaBytes @IndirectCommandsStreamNV ((Data.Vector.length (streams)) * 16)
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
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (zero)
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
             pipelineBindPoint
             pipeline
             indirectCommandsLayout
             pStreams'
             sequencesCount
             preprocessBuffer
             preprocessOffset
             preprocessSize
             sequencesCountBuffer
             sequencesCountOffset
             sequencesIndexBuffer
             sequencesIndexOffset

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
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pipelineBindPoint-09075#
--     If @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     then @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline'
--     handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pipelineBindPoint-09076#
--     If @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     and the @indirectCommandsLayout@ was not created with a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV' token, then the
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-pipelineBindPoint-09077#
--     If @pipelineBindPoint@ is of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     and the @indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV' token, then the
--     @pipeline@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
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
--     If @pipeline@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-indirectCommandsLayout-parameter#
--     @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutNV' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoNV-commonparent# Both
--     of @indirectCommandsLayout@, and @pipeline@ that are valid handles
--     of non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
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
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
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
-- = Description
--
-- -   'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV'
--     specifies that the layout is always used with the manual
--     preprocessing step through calling
--     'cmdPreprocessGeneratedCommandsNV' and executed by
--     'cmdExecuteGeneratedCommandsNV' with @isPreprocessed@ set to
--     'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- -   'INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV' specifies
--     that the input data for the sequences is not implicitly indexed from
--     0..sequencesUsed, but an application-provided
--     'Vulkan.Core10.Handles.Buffer' encoding the index is provided.
--
-- -   'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV'
--     specifies that the processing of sequences /can/ happen at an
--     implementation-dependent order, which is not guaranteed to be
--     coherent using the same input data. This flag is ignored when the
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--     as it is implied that the dispatch sequence is always unordered.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'IndirectCommandsLayoutUsageFlagsNV'
newtype IndirectCommandsLayoutUsageFlagBitsNV = IndirectCommandsLayoutUsageFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000001

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000002

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000004

conNameIndirectCommandsLayoutUsageFlagBitsNV :: String
conNameIndirectCommandsLayoutUsageFlagBitsNV = "IndirectCommandsLayoutUsageFlagBitsNV"

enumPrefixIndirectCommandsLayoutUsageFlagBitsNV :: String
enumPrefixIndirectCommandsLayoutUsageFlagBitsNV = "INDIRECT_COMMANDS_LAYOUT_USAGE_"

showTableIndirectCommandsLayoutUsageFlagBitsNV :: [(IndirectCommandsLayoutUsageFlagBitsNV, String)]
showTableIndirectCommandsLayoutUsageFlagBitsNV =
  [
    ( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
    , "EXPLICIT_PREPROCESS_BIT_NV"
    )
  ,
    ( INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
    , "INDEXED_SEQUENCES_BIT_NV"
    )
  ,
    ( INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
    , "UNORDERED_SEQUENCES_BIT_NV"
    )
  ]

instance Show IndirectCommandsLayoutUsageFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
      showTableIndirectCommandsLayoutUsageFlagBitsNV
      conNameIndirectCommandsLayoutUsageFlagBitsNV
      (\(IndirectCommandsLayoutUsageFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read IndirectCommandsLayoutUsageFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
      showTableIndirectCommandsLayoutUsageFlagBitsNV
      conNameIndirectCommandsLayoutUsageFlagBitsNV
      IndirectCommandsLayoutUsageFlagBitsNV

type IndirectStateFlagsNV = IndirectStateFlagBitsNV

-- | VkIndirectStateFlagBitsNV - Bitmask specifying state that can be altered
-- on the device
--
-- = Description
--
-- -   'INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV' allows to toggle the
--     'Vulkan.Core10.Enums.FrontFace.FrontFace' rasterization state for
--     subsequent drawing commands.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'IndirectStateFlagsNV'
newtype IndirectStateFlagBitsNV = IndirectStateFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkIndirectStateFlagBitsNV" "VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"
pattern INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV = IndirectStateFlagBitsNV 0x00000001

conNameIndirectStateFlagBitsNV :: String
conNameIndirectStateFlagBitsNV = "IndirectStateFlagBitsNV"

enumPrefixIndirectStateFlagBitsNV :: String
enumPrefixIndirectStateFlagBitsNV = "INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"

showTableIndirectStateFlagBitsNV :: [(IndirectStateFlagBitsNV, String)]
showTableIndirectStateFlagBitsNV = [(INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV, "")]

instance Show IndirectStateFlagBitsNV where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectStateFlagBitsNV
      showTableIndirectStateFlagBitsNV
      conNameIndirectStateFlagBitsNV
      (\(IndirectStateFlagBitsNV x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read IndirectStateFlagBitsNV where
  readPrec =
    enumReadPrec
      enumPrefixIndirectStateFlagBitsNV
      showTableIndirectStateFlagBitsNV
      conNameIndirectStateFlagBitsNV
      IndirectStateFlagBitsNV

-- | VkIndirectCommandsTokenTypeNV - Enum specifying token commands
--
-- = Description
--
-- \'
--
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | Token type                                        | Equivalent command                                                 |
-- +===================================================+====================================================================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV'    | 'cmdBindPipelineShaderGroupNV'                                     |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV'     | -                                                                  |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV'    | 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'           |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV'   | 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'         |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV'   | 'Vulkan.Core10.CommandBufferBuilding.cmdPushConstants'             |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV'       | 'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT'          |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV'    | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'       |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV'            | 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'              |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV'      | 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV'   |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV' | 'Vulkan.Extensions.VK_EXT_mesh_shader.cmdDrawMeshTasksIndirectEXT' |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV'        | 'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline'              |
-- +---------------------------------------------------+--------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV'        | 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'          |
-- +---------------------------------------------------+--------------------------------------------------------------------+
--
-- Supported Indirect Command Tokens
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'IndirectCommandsLayoutTokenNV'
newtype IndirectCommandsTokenTypeNV = IndirectCommandsTokenTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV = IndirectCommandsTokenTypeNV 0

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV = IndirectCommandsTokenTypeNV 1

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV = IndirectCommandsTokenTypeNV 2

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV = IndirectCommandsTokenTypeNV 3

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV = IndirectCommandsTokenTypeNV 4

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV = IndirectCommandsTokenTypeNV 5

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV = IndirectCommandsTokenTypeNV 6

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV = IndirectCommandsTokenTypeNV 7

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV = IndirectCommandsTokenTypeNV 1000428004

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV = IndirectCommandsTokenTypeNV 1000428003

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV = IndirectCommandsTokenTypeNV 1000328000

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV = IndirectCommandsTokenTypeNV 1000135000

{-# COMPLETE
  INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV
  , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV ::
    IndirectCommandsTokenTypeNV
  #-}

conNameIndirectCommandsTokenTypeNV :: String
conNameIndirectCommandsTokenTypeNV = "IndirectCommandsTokenTypeNV"

enumPrefixIndirectCommandsTokenTypeNV :: String
enumPrefixIndirectCommandsTokenTypeNV = "INDIRECT_COMMANDS_TOKEN_TYPE_"

showTableIndirectCommandsTokenTypeNV :: [(IndirectCommandsTokenTypeNV, String)]
showTableIndirectCommandsTokenTypeNV =
  [
    ( INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
    , "SHADER_GROUP_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
    , "STATE_FLAGS_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
    , "INDEX_BUFFER_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
    , "VERTEX_BUFFER_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
    , "PUSH_CONSTANT_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
    , "DRAW_INDEXED_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
    , "DRAW_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
    , "DRAW_TASKS_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV
    , "DISPATCH_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV
    , "PIPELINE_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV
    , "DRAW_MESH_TASKS_NV"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV
    , "PUSH_DATA_NV"
    )
  ]

instance Show IndirectCommandsTokenTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectCommandsTokenTypeNV
      showTableIndirectCommandsTokenTypeNV
      conNameIndirectCommandsTokenTypeNV
      (\(IndirectCommandsTokenTypeNV x) -> x)
      (showsPrec 11)

instance Read IndirectCommandsTokenTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixIndirectCommandsTokenTypeNV
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

