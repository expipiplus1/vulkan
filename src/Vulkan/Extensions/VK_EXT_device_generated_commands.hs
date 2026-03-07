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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
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
--     compute queue, or for the purpose of re-using the data in multiple
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
--     -   'ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT'
--
--     -   'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_KHR_maintenance5.BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_EXT'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_INDIRECT_EXECUTION_SET_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_device_generated_commands Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_generated_commands  ( cmdExecuteGeneratedCommandsEXT
                                                           , cmdPreprocessGeneratedCommandsEXT
                                                           , getGeneratedCommandsMemoryRequirementsEXT
                                                           , createIndirectCommandsLayoutEXT
                                                           , withIndirectCommandsLayoutEXT
                                                           , destroyIndirectCommandsLayoutEXT
                                                           , createIndirectExecutionSetEXT
                                                           , withIndirectExecutionSetEXT
                                                           , destroyIndirectExecutionSetEXT
                                                           , updateIndirectExecutionSetPipelineEXT
                                                           , updateIndirectExecutionSetShaderEXT
                                                           , pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT
                                                           , pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT
                                                           , pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT
                                                           , PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT(..)
                                                           , PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT(..)
                                                           , GeneratedCommandsPipelineInfoEXT(..)
                                                           , GeneratedCommandsShaderInfoEXT(..)
                                                           , GeneratedCommandsMemoryRequirementsInfoEXT(..)
                                                           , IndirectExecutionSetPipelineInfoEXT(..)
                                                           , IndirectExecutionSetShaderLayoutInfoEXT(..)
                                                           , IndirectExecutionSetShaderInfoEXT(..)
                                                           , IndirectExecutionSetCreateInfoEXT(..)
                                                           , GeneratedCommandsInfoEXT(..)
                                                           , WriteIndirectExecutionSetPipelineEXT(..)
                                                           , WriteIndirectExecutionSetShaderEXT(..)
                                                           , IndirectCommandsLayoutCreateInfoEXT(..)
                                                           , IndirectCommandsLayoutTokenEXT(..)
                                                           , DrawIndirectCountIndirectCommandEXT(..)
                                                           , IndirectCommandsVertexBufferTokenEXT(..)
                                                           , BindVertexBufferIndirectCommandEXT(..)
                                                           , IndirectCommandsIndexBufferTokenEXT(..)
                                                           , BindIndexBufferIndirectCommandEXT(..)
                                                           , IndirectCommandsPushConstantTokenEXT(..)
                                                           , IndirectCommandsExecutionSetTokenEXT(..)
                                                           , IndirectExecutionSetInfoEXT(..)
                                                           , IndirectCommandsTokenDataEXT(..)
                                                           , IndirectCommandsLayoutUsageFlagsEXT
                                                           , IndirectCommandsLayoutUsageFlagBitsEXT( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT
                                                                                                   , INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT
                                                                                                   , ..
                                                                                                   )
                                                           , IndirectExecutionSetInfoTypeEXT( INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT
                                                                                            , INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT
                                                                                            , ..
                                                                                            )
                                                           , IndirectCommandsInputModeFlagsEXT
                                                           , IndirectCommandsInputModeFlagBitsEXT( INDIRECT_COMMANDS_INPUT_MODE_VULKAN_INDEX_BUFFER_EXT
                                                                                                 , INDIRECT_COMMANDS_INPUT_MODE_DXGI_INDEX_BUFFER_EXT
                                                                                                 , ..
                                                                                                 )
                                                           , IndirectCommandsTokenTypeEXT( INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT
                                                                                         , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT
                                                                                         , ..
                                                                                         )
                                                           , EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                           , pattern EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                           , EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                           , pattern EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                           , IndirectCommandsLayoutEXT(..)
                                                           , IndirectExecutionSetEXT(..)
                                                           , ShaderEXT(..)
                                                           , BufferUsageFlagBits2KHR(..)
                                                           , BufferUsageFlags2KHR
                                                           , PipelineCreateFlagBits2KHR(..)
                                                           , PipelineCreateFlags2KHR
                                                           , ShaderCreateFlagBitsEXT(..)
                                                           , ShaderCreateFlagsEXT
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
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteGeneratedCommandsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPreprocessGeneratedCommandsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectExecutionSetEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectExecutionSetEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetGeneratedCommandsMemoryRequirementsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateIndirectExecutionSetPipelineEXT))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateIndirectExecutionSetShaderEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutEXT)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutEXT(..))
import Vulkan.Extensions.Handles (IndirectExecutionSetEXT)
import Vulkan.Extensions.Handles (IndirectExecutionSetEXT(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.PipelineLayout (PushConstantRange)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (ShaderEXT)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_COMMAND_PREPROCESS_READ_BIT_NV))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_maintenance5 (BufferUsageFlagBits2KHR(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (BufferUsageFlags2KHR)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutEXT(..))
import Vulkan.Extensions.Handles (IndirectExecutionSetEXT(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlagBits2KHR(..))
import Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlags2KHR)
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
import Vulkan.Extensions.Handles (ShaderEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteGeneratedCommandsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> Ptr (SomeStruct GeneratedCommandsInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> Ptr (SomeStruct GeneratedCommandsInfoEXT) -> IO ()

-- | vkCmdExecuteGeneratedCommandsEXT - Generate and execute commands on the
-- device
--
-- = Description
--
-- If the 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT' flag
-- was used to create the
-- 'GeneratedCommandsInfoEXT'::@indirectCommandsLayout@ then the execution
-- of sequences through this command /may/ use implementation-defined
-- ordering which is not guaranteed to be coherent using the same input
-- data. It does not affect the order of token processing within a
-- sequence. This is the implied ordering with
-- 'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT'.
--
-- After a call to 'cmdExecuteGeneratedCommandsEXT', command buffer state
-- will become undefined according to the tokens executed. This table
-- specifies the relationship between tokens used and state invalidation.
--
-- +---------------------------------------------------+------------------+
-- | __Common Tokens__                                 | __States         |
-- |                                                   | Invalidated__    |
-- +===================================================+==================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'  | Bound shaders    |
-- |                                                   | and pipelines    |
-- +---------------------------------------------------+------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT'  | Push constant    |
-- |                                                   | data             |
-- +---------------------------------------------------+------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT' | Push constant    |
-- |                                                   | data             |
-- +---------------------------------------------------+------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT'   | Index buffer     |
-- +---------------------------------------------------+------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT'  | Vertex buffer    |
-- +---------------------------------------------------+------------------+
--
-- Indirect Execution State Invalidation
--
-- == Valid Usage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-magFilter-04553# If a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-magFilter-09598# If a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-mipmapMode-04770# If a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-mipmapMode-09599# If a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-unnormalizedCoordinates-09635#
--     If a 'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @levelCount@ and @layerCount@ /must/ be 1
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-unnormalizedCoordinates-09636#
--     If a 'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cubicRangeClamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-selectableCubicWeights-09214#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-selectableCubicWeights selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08600# For each set /n/
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a descriptor set /must/ have been bound to /n/ at the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for set /n/, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08601# For each push
--     constant that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-10068# For each array of
--     resources that is used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     the indices used to access members of the array /must/ be less than
--     the descriptor count for the identified binding in the descriptor
--     sets used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-maintenance4-08602# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     a push constant value /must/ have been set for the same pipeline
--     bind point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT' , as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08114# Descriptors in
--     each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     if they are statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08115# If the
--     descriptors used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08116# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08604# Descriptors in
--     bound descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08117# If the
--     descriptors used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08119# If a descriptor
--     is dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08605# If a descriptor
--     is dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08608# If a pipeline is
--     bound to the pipeline bind point used by this command, there /must/
--     not have been any calls to dynamic state setting commands for any
--     state specified statically in the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command, since
--     that pipeline was bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08609# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08610# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08611# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command or any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08607# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is enabled, either a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command, or a valid combination of
--     valid and 'Vulkan.Core10.APIConstants.NULL_HANDLE' shader objects
--     /must/ be bound to every supported shader stage corresponding to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-uniformBuffers-06935# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-storageBuffers-06936# If any
--     stage of the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     and that stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06550# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-ConstOffset-06551# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-image-dimensions ???>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the image view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sparseImageInt64Atomics-04474#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sparseImageInt64Atomics-04475#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWeightedSampleQCOM-06971#
--     If @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWeightedSampleQCOM-06972#
--     If @OpImageWeightedSampleQCOM@ uses a
--     'Vulkan.Core10.Handles.ImageView' as a sample weight image as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBoxFilterQCOM-06973#
--     If @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchSSDQCOM-06974#
--     If @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchSADQCOM-06975#
--     If @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchSADQCOM-06976#
--     If @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWeightedSampleQCOM-06977#
--     If @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageWeightedSampleQCOM-06978#
--     If any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchWindow-09215#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchWindow-09216#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpImageBlockMatchWindow-09217#
--     If a @OpImageBlockMatchWindow*QCOM@ or
--     @OpImageBlockMatchGather*QCOM@ read from a reference image as result
--     of this command, then the specified reference coordinates /must/ not
--     fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07288# Any shader
--     invocation executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09600# If a descriptor
--     with type equal to any of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     is accessed as a result of this command, the image subresource
--     identified by that descriptor /must/ be in the image layout
--     identified when the descriptor was written
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-renderPass-02684# The current
--     render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-subpass-02685# The subpass
--     index of the current render pass /must/ be equal to the @subpass@
--     member of the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Vulkan.Core10.Handles.Pipeline' bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07748# If any shader
--     statically accesses an input attachment, a valid descriptor /must/
--     be bound to the pipeline via a descriptor set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-OpTypeImage-07468# If any
--     shader executed by this pipeline accesses an @OpTypeImage@ variable
--     with a @Dim@ operand of @SubpassData@, it /must/ be decorated with
--     an @InputAttachmentIndex@ that corresponds to a valid input
--     attachment in the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07469# Input attachment
--     views accessed in a subpass /must/ be created with the same
--     'Vulkan.Core10.Enums.Format.Format' as the corresponding subpass
--     definition, and be created with a 'Vulkan.Core10.Handles.ImageView'
--     that is compatible with the attachment referenced by the subpass\'
--     @pInputAttachments@[@InputAttachmentIndex@] in the bound
--     'Vulkan.Core10.Handles.Framebuffer' as specified by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#compatibility-inputattachment Fragment Input Attachment Compatibility>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDepthInputAttachmentIndex-09595#
--     Input attachment views accessed in a dynamic render pass with a
--     @InputAttachmentIndex@ referenced by
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR',
--     or no @InputAttachmentIndex@ if
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR':@pDepthInputAttachmentIndex@
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR':@pStencilInputAttachmentIndex@
--     are @NULL@, /must/ be created with a
--     'Vulkan.Core10.Handles.ImageView' that is compatible with the
--     corresponding color, depth, or stencil attachment in
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDepthInputAttachmentIndex-09596#
--     Input attachment views accessed in a dynamic render pass via a
--     shader object /must/ have an @InputAttachmentIndex@ if both
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR':@pDepthInputAttachmentIndex@
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR':@pStencilInputAttachmentIndex@
--     are non-@NULL@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-InputAttachmentIndex-09597#
--     If an input attachment view accessed in a dynamic render pass via a
--     shader object has an @InputAttachmentIndex@, the
--     @InputAttachmentIndex@ /must/ match an index in
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06537# Memory backing
--     image subresources used as attachments in the current render pass
--     /must/ not be written in any way other than as an attachment by this
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09000# If a color
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--         and
--
--         -   there is no bound graphics pipeline or
--
--         -   the bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--
--     it /must/ not be accessed in any way other than as an attachment by
--     this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09001# If a depth
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--         and
--
--         -   there is no bound graphics pipeline or
--
--         -   the bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--
--     it /must/ not be accessed in any way other than as an attachment by
--     this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09002# If a stencil
--     attachment is written by any prior command in this subpass or by the
--     load, store, or resolve operations for this subpass, it is not in
--     the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     image layout, and either:
--
--     -   the
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--         is set on the bound pipeline or
--
--     -   the last call to
--         'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state.cmdSetAttachmentFeedbackLoopEnableEXT'
--         included
--         'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--         and
--
--         -   there is no bound graphics pipeline or
--
--         -   the bound graphics pipeline was created with
--             'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--
--     it /must/ not be accessed in any way other than as an attachment by
--     this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09003# If an attachment
--     is written by any prior command in this subpass or by the load,
--     store, or resolve operations for this subpass, it /must/ not be
--     accessed in any way other than as an attachment, storage image, or
--     sampled image by this command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06539# If any previously
--     recorded command in the current subpass accessed an image
--     subresource used as an attachment in this subpass in any way other
--     than as an attachment, this command /must/ not write to that image
--     subresource as an attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06886# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06887# If the current
--     render pass instance uses a depth\/stencil attachment with a
--     read-only layout for the stencil aspect, both front and back
--     @writeMask@ are not zero, and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07831# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07832# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' /must/ have been
--     called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07833# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' dynamic
--     state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08617# If a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08618# If a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08619# If a shader
--     object that outputs line primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' /must/ have
--     been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07834# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS' dynamic
--     state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @depthBiasEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' or
--     'Vulkan.Extensions.VK_EXT_depth_bias_control.cmdSetDepthBias2EXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07835# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS'
--     dynamic state enabled then
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants' /must/
--     have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08621# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     in the current command buffer set any element of
--     @pColorBlendEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE', and
--     the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     in the current command buffer set the same element of
--     @pColorBlendEquations@ to a
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.ColorBlendEquationEXT'
--     structure with any 'Vulkan.Core10.Enums.BlendFactor.BlendFactor'
--     member with a value of
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA',
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants' /must/
--     have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07836# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07837# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07838# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07839# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-maxMultiviewInstanceIndex-02688#
--     If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-02689#
--     If the bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07634# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-06666# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07840# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07841# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07843# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07844# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     dynamic state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07845# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07846# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07847# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07848# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-03417# If a
--     shader object is bound to any graphics stage or a graphics pipeline
--     is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and the state is not inherited, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-scissorCount-03418# If a
--     shader object is bound to any graphics stage or a graphics pipeline
--     is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, and the state is not inherited, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-03419# If a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-04137# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-04138# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08636# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-04139# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-shadingRateImage-09233# If
--     the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-shadingRateImage-09234# If
--     the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08637# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-VkPipelineVieportCreateInfo-04141#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-VkPipelineVieportCreateInfo-04142#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07878# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07879# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-04876# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-04877# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-logicOp-04878# If a shader
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-primitiveFragmentShadingRateWithMultipleViewports-04552#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-primitiveFragmentShadingRateWithMultipleViewports-08642#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-blendEnable-04727# If
--     rasterization is not disabled in the bound graphics pipeline, then
--     for each color attachment in the subpass, if the corresponding image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08643# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then for each color
--     attachment in the render pass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the corresponding member of @pColorBlendEnables@ in the most
--     recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     in the current command buffer that affected that attachment index
--     /must/ have been 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-multisampledRenderToSingleSampled-07284#
--     If rasterization is not disabled in the bound graphics pipeline, and
--     none of the following is enabled:
--
--     -   the @VK_AMD_mixed_attachment_samples@ extension
--
--     -   the @VK_NV_framebuffer_mixed_samples@ extension
--
--     -   the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--         feature
--
--     then @rasterizationSamples@ for the bound graphics pipeline /must/
--     be the same as the current subpass color and\/or depth\/stencil
--     attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08644# If a shader
--     object is bound to any graphics stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
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
--     then the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     in the current command buffer /must/ have set @rasterizationSamples@
--     to be the same as the number of samples for the current render pass
--     color and\/or depth\/stencil attachments
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08876# If a shader
--     object is bound to any graphics stage, the current render pass
--     instance /must/ have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06172# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06173# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06174# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06175# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06176# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06177# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewMask-06178# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound graphics pipeline /must/ have been created with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-colorAttachmentCount-06179#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dynamicRenderingUnusedAttachments dynamicRenderingUnusedAttachments>
--     feature is not enabled and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound graphics pipeline /must/ have been created with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08910#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08912#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08911#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-colorAttachmentCount-09362#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, there is no shader object bound to any graphics stage,
--     and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09363# If there is no
--     shader object bound to any graphics stage, the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, and a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09364# If the current
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09365# If the current
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09366# If there is a
--     shader object bound to any graphics stage, and the current render
--     pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have set blend enable to
--     'Vulkan.Core10.FundamentalTypes.FALSE' prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizationSamples-09367#
--     If there is a shader object bound to any graphics stage, and the
--     current render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ have set @rasterizationSamples@ to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT' prior
--     to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09368# If the current
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09369# If the current
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pFragmentSize-09370# If there
--     is a shader object bound to any graphics stage, and the current
--     render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->width@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pFragmentSize-09371# If there
--     is a shader object bound to any graphics stage, and the current
--     render pass includes a color attachment that uses the
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--     resolve mode, then
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
--     /must/ have set @pFragmentSize->height@ to @1@ prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07749# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08646# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-attachmentCount-07750# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08647# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then the @attachmentCount@
--     parameter of most recent call to
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     in the current command buffer /must/ be greater than or equal to the
--     number of color attachments in the current render pass instance
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07751# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command for each
--     discard rectangle in
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'::@discardRectangleCount@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07880# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07881# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09236#
--     If the @VK_EXT_discard_rectangles@ extension is enabled, and a
--     shader object is bound to any graphics stage, and the most recent
--     call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEnableEXT'
--     in the current command buffer set @discardRectangleEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08913#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08914#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08915#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08916#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08917#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-dynamicRenderingUnusedAttachments-08918#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06183# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-imageView-06184# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-colorAttachmentCount-06185#
--     If the bound pipeline was created with a
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDepthAttachment-06186# If
--     the current render pass instance was begun with
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pStencilAttachment-06187# If
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-multisampledRenderToSingleSampled-07285#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-multisampledRenderToSingleSampled-07286#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-multisampledRenderToSingleSampled-07287#
--     If the bound pipeline was created without a
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     @rasterizationSamples@ for the bound graphics pipeline /must/ be
--     equal to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pNext-07935# If this command
--     has been called inside a render pass instance started with
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-renderPass-06198# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pColorAttachments-08963# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound with a fragment shader that
--     statically writes to a color attachment, the color write mask is not
--     zero, color writes are enabled, and the corresponding element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDepthAttachment-08964# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound, depth test is enabled, depth
--     write is enabled, and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pStencilAttachment-08965# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     there is a graphics pipeline bound, stencil test is enabled and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the pipeline /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07619# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetTessellationDomainOriginEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07620# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07621# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07622# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07623# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-alphaToCoverageEnable-08919#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-alphaToCoverageEnable-08920#
--     If a shader object is bound to any graphics stage, and the most
--     recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetAlphaToCoverageEnableEXT'
--     in the current command buffer set @alphaToCoverageEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07624# If a shader
--     object is bound to any graphics stage or a graphics pipeline is
--     bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07625# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07626# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07627# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08657# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and both the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE' and there are color
--     attachments bound, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07628# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08658# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     for any attachment set that attachment’s value in
--     @pColorBlendEnables@ to 'Vulkan.Core10.FundamentalTypes.TRUE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07629# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08659# If a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and both the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE' and there are color
--     attachments bound, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07630# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryStreams geometryStreams>
--     feature is enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage or a graphics pipeline is bound which was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationStreamEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07631# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07632# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07633# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07635# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09416#
--     If the @VK_EXT_blend_operation_advanced@ extension is enabled, and a
--     shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then at least one of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07636# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07637# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08666# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08667# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08668# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object that outputs line
--     primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineRasterizationModeEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07638# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08669# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to any graphics
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetPolygonModeEXT'
--     in the current command buffer set @polygonMode@ to
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_LINE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08670# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
--     in the current command buffer set @primitiveTopology@ to any line
--     topology, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08671# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled, and a shader object that outputs line
--     primitives is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetLineStippleEnableEXT'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07849# If any of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     features are enabled and a shader object is bound to any graphics
--     stage, or a bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_KHR'
--     dynamic state enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @stippledLineEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then
--     'Vulkan.Extensions.VK_KHR_line_rasterization.cmdSetLineStippleKHR'
--     /must/ have been called and not subsequently
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-lifetime invalidated>
--     in the current command buffer prior to this drawing command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07639# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09650# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07640# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07641# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07642# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07643# If the
--     @VK_NV_fragment_coverage_to_color@ extension is enabled, a shader
--     object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07644# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07645# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07646# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07647# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pipelineFragmentShadingRate-09238#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature is enabled, a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage or a graphics pipeline is bound which was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07648# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07649# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pColorBlendEnables-07470# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     state enabled and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     set @pColorBlendEnables@ for any attachment to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then for those attachments in
--     the subpass the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizationSamples-07471#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the current subpass does not use any color
--     and\/or depth\/stencil attachments, then the @rasterizationSamples@
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     /must/ follow the rules for a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-samples-07472# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state disabled, then the @samples@ parameter in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     /must/ be greater or equal to the
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     parameter used to create the bound graphics pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-samples-07473# If the bound
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizationSamples-07474#
--     If the bound graphics pipeline state was created with the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09211# If the bound
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-firstAttachment-07476# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     calls /must/ specify an enable for all active color attachments in
--     the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09417#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     calls /must/ specify an enable for all active color attachments in
--     the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-firstAttachment-07477# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     calls /must/ specify the blend equations for all active color
--     attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09418#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and both the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE' and there are color
--     attachments bound, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEquationEXT'
--     calls /must/ specify the blend equations for all active color
--     attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-firstAttachment-07478# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     calls /must/ specify the color write mask for all active color
--     attachments in the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09419#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorWriteMaskEXT'
--     calls /must/ specify the color write mask for all active color
--     attachments in the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-firstAttachment-07479# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the attachments specified by the
--     @firstAttachment@ and @attachmentCount@ parameters of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     calls /must/ specify the advanced blend equations for all active
--     color attachments in the current subpass where blending is enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-advancedBlendMaxColorAttachments-07480#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--     dynamic states enabled and the last calls to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendEnableEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetColorBlendAdvancedEXT'
--     have enabled advanced blending, then the number of active color
--     attachments in the current subpass /must/ not exceed
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-advancedBlendMaxColorAttachments advancedBlendMaxColorAttachments>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-primitivesGeneratedQueryWithNonZeroStreams-07481#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsPerPixel-07482#
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
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsPerPixel-07483#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07484#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     and the current subpass has a depth\/stencil attachment, then that
--     attachment /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07485#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.sampleLocationGridSize.width@ in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07486#
--     If a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, or the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state enabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     state enabled, and if @sampleLocationsEnable@ was
--     'Vulkan.Core10.FundamentalTypes.TRUE' in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleLocationsEnableEXT',
--     then the @sampleLocationsInfo.sampleLocationGridSize.height@ in the
--     last call to
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07487#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07936#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @sampleLocationsInfo.sampleLocationGridSize.width@ /must/
--     evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07937#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     state disabled and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     state enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dynamic-state-current-value current value>
--     of @sampleLocationsEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     then @sampleLocationsInfo.sampleLocationGridSize.height@ /must/
--     evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling the value of
--     @rasterizationSamples@ in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-sampleLocationsEnable-07938#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-coverageModulationTableEnable-07488#
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
--     number of color samples in the current subpass
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizationSamples-07489#
--     If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if current subpass has a depth\/stencil attachment and depth test,
--     stencil test, or depth bounds test are enabled in the bound
--     pipeline, then the current @rasterizationSamples@ /must/ be the same
--     as the sample count of the depth\/stencil attachment
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-coverageToColorEnable-07490#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     state enabled and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the current subpass
--     /must/ have a color attachment at the location selected by the last
--     call to
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizerDiscardEnable-09420#
--     If the @VK_NV_fragment_coverage_to_color@ extension is enabled, and
--     a shader object is bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--     stage, and the most recent call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     in the current command buffer set @rasterizerDiscardEnable@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', and the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetCoverageToColorEnableNV'
--     set the @coverageToColorEnable@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the current subpass
--     /must/ have a color attachment at the location selected by the last
--     call to
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-coverageReductionMode-07491#
--     If this @VK_NV_coverage_reduction_mode@ extension is enabled, the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--     states enabled, the current coverage reduction mode
--     @coverageReductionMode@, then the current @rasterizationSamples@,
--     and the sample counts for the color and depth\/stencil attachments
--     (if the subpass has them) /must/ be a valid combination returned by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-07492# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-07493# If the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-viewportCount-09421# If the
--     @VK_NV_viewport_swizzle@ extension is enabled, and a shader object
--     is bound to any graphics stage, then the @viewportCount@ parameter
--     in the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetViewportSwizzleNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-rasterizationSamples-07494#
--     If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if the current subpass has any color attachments and
--     @rasterizationSamples@ of the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetRasterizationSamplesEXT'
--     is greater than the number of color samples, then the pipeline
--     @sampleShadingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-stippledLineEnable-07495# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_KHR_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_KHR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-stippledLineEnable-07496# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_KHR_line_rasterization.LINE_RASTERIZATION_MODE_BRESENHAM_KHR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledBresenhamLines stippledBresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-stippledLineEnable-07497# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_KHR_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledSmoothLines stippledSmoothLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-stippledLineEnable-07498# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--     dynamic states enabled, and if the current @stippledLineEnable@
--     state is 'Vulkan.Core10.FundamentalTypes.TRUE' and the current
--     @lineRasterizationMode@ state is
--     'Vulkan.Extensions.VK_KHR_line_rasterization.LINE_RASTERIZATION_MODE_DEFAULT_KHR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-stippledRectangularLines stippledRectangularLines>
--     feature /must/ be enabled and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@strictLines@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-conservativePointAndLineRasterization-07499#
--     If the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--     dynamic state enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-conservativePointAndLineRasterization conservativePointAndLineRasterization>
--     is not supported, and the effective primitive topology output by the
--     last pre-rasterization shader stage is a line or point, then the
--     @conservativeRasterizationMode@ set by the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetConservativeRasterizationModeEXT'
--     /must/ be
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-stage-07073# If the bound
--     pipeline was created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     /must/ not be active
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08877# If a shader
--     object is bound to the
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-07850# If dynamic state
--     was inherited from
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
--     it /must/ be set in the current command buffer prior to this drawing
--     command
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08684# If there is no
--     bound graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08685# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08686# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08687# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08688# If there is no
--     bound graphics pipeline,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08689# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08690# If there is no
--     bound graphics pipeline, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is enabled,
--     'Vulkan.Extensions.VK_EXT_shader_object.cmdBindShadersEXT' /must/
--     have been called in the current command buffer with @pStages@ with
--     an element of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08693# If there is no
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08694# If there is no
--     bound graphics pipeline, and both the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features are enabled, and a valid
--     'Vulkan.Extensions.Handles.ShaderEXT' is bound the to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage, and that 'Vulkan.Extensions.Handles.ShaderEXT' was created
--     without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     flag, a valid 'Vulkan.Extensions.Handles.ShaderEXT' /must/ be bound
--     to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08695# If there is no
--     bound graphics pipeline, and both the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     features are enabled, and a valid
--     'Vulkan.Extensions.Handles.ShaderEXT' is bound the to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     stage, and that 'Vulkan.Extensions.Handles.ShaderEXT' was created
--     with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     flag, there /must/ be no 'Vulkan.Extensions.Handles.ShaderEXT' bound
--     to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     stage
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08696# If there is no
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08698# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, then all shaders created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag in the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ also be bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08699# If any graphics
--     shader is bound which was created with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag, any stages in between stages whose shaders which did not
--     create a shader with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_LINK_STAGE_BIT_EXT'
--     flag as part of the same
--     'Vulkan.Extensions.VK_EXT_shader_object.createShadersEXT' call
--     /must/ not have any 'Vulkan.Extensions.Handles.ShaderEXT' bound
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08878# All bound
--     graphics shader objects /must/ have been created with identical or
--     identically defined push constant ranges
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-08879# All bound
--     graphics shader objects /must/ have been created with identical or
--     identically defined arrays of descriptor set layouts
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-colorAttachmentCount-09372#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     equal to @1@, a color attachment with a resolve mode of
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID',
--     and a fragment shader is bound, it /must/ not declare the
--     @DepthReplacing@ or @StencilRefReplacingEXT@ execution modes
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDynamicStates-08715# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     set in
--     'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpDepthAttachmentReadEXT@, the @depthWriteEnable@
--     parameter in the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pDynamicStates-08716# If the
--     bound graphics pipeline state includes a fragment shader stage, was
--     created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     set in
--     'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpStencilAttachmentReadEXT@, the @writeMask@
--     parameter in the last call to
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' /must/
--     be @0@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09116# If a shader
--     object is bound to any graphics stage or the bound graphics pipeline
--     was created with
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-maxFragmentDualSrcAttachments-09239#
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
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09548# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and there is no shader object bound to any graphics stage, the value
--     of each element of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingAttachmentLocationInfoKHR'::@pColorAttachmentLocations@
--     set by
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.cmdSetRenderingAttachmentLocationsKHR'
--     /must/ match the value set for the corresponding element in the
--     bound pipeline
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09549# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and there is no shader object bound to any graphics stage, input
--     attachment index mappings in the bound pipeline /must/ match those
--     set for the current render pass instance via
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.RenderingInputAttachmentIndexInfoKHR'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09642# If the current
--     render pass was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with the
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--     flag, the bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-09643# If the bound
--     graphics pipeline was created with
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT',
--     the current render pass /must/ have begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with the
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--     flag
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-11045#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11046# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and
--     'getGeneratedCommandsMemoryRequirementsEXT' did not return a
--     required size of zero then 'cmdPreprocessGeneratedCommandsEXT'
--     /must/ have already been executed on the device before this command
--     executes, and the preprocessing command /must/ have used the same
--     @pGeneratedCommandsInfo@ content as well as the content of the input
--     buffers it references (all except
--     'GeneratedCommandsInfoEXT'::@preprocessBuffer@)
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11047# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then the
--     @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@ /must/
--     have been created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT' bit set
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-indirectCommandsLayout-11141#
--     If the @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@
--     was created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT' bit
--     set, then @isPreprocessed@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-preprocessAddress-11142# The
--     contents of the @preprocessAddress@ member of
--     @pGeneratedCommandsInfo@ /must/ not have been previously used to
--     record another 'cmdExecuteGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11048# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then the
--     bound descriptor sets and push constants /must/ match identically
--     with those bound during recording of the corresponding call to
--     'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-10198# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' then the
--     conditional render state and its predicate value /must/ match
--     identically with the state and value set during execution of the
--     corresponding call to 'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11049# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@ contains
--     a draw token, then the graphics state bound on @commandBuffer@
--     /must/ match identically with the graphics state bound on the
--     @stateCommandBuffer@ passed to 'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11149# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     queue family index of @commandBuffer@ /must/ be the same as the
--     queue family index used to allocate the @stateCommandBuffer@ passed
--     to 'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11051# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@ contains
--     a dispatch token, then the compute state bound on @commandBuffer@
--     /must/ match identically with the compute state bound on the
--     @stateCommandBuffer@ passed to 'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11052# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@ contains
--     a ray tracing token, then the ray tracing state bound on
--     @commandBuffer@ /must/ match identically with the ray tracing state
--     bound on the @stateCommandBuffer@ passed to
--     'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11150# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@ contains
--     a ray tracing token, the queue family index @commandBuffer@ was
--     allocated from /must/ be the same queue family index used to
--     allocate the @stateCommandBuffer@ passed to
--     'cmdPreprocessGeneratedCommandsEXT'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-indirectCommandsLayout-11053#
--     If the token sequence of the passed
--     'GeneratedCommandsInfoEXT'::@indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, the initial
--     shader state of 'GeneratedCommandsInfoEXT'::@indirectExecutionSet@
--     /must/ be bound on @commandBuffer@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-indirectCommandsLayout-11004#
--     If @indirectCommandsLayout@ was created with a token sequence that
--     contained the 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token
--     and @indirectExecutionSet@ was created using
--     'INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT', every
--     executed 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token
--     /must/ bind all the shader stages set in the
--     'IndirectCommandsExecutionSetTokenEXT'::@shaderStages@ used to
--     create @indirectCommandsLayout@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-isPreprocessed-11055# If
--     @isPreprocessed@ is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     token sequence of the passed
--     'GeneratedCommandsInfoEXT'::@indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, the members
--     of 'GeneratedCommandsInfoEXT'::@indirectExecutionSet@ accessed by
--     this command /must/ not have been modified since the preprocess
--     buffer was generated
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-indirectCommandsLayout-11056#
--     If the @indirectCommandsLayout@ member of @pGeneratedCommandsInfo@
--     contains a draw token, then the active render pass /must/ not have a
--     specified fragment density map
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-deviceGeneratedCommandsTransformFeedback-11057#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-deviceGeneratedCommandsTransformFeedback deviceGeneratedCommandsTransformFeedback>
--     is not supported on device, transform feedback /must/ not be active
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-indirectExecutionSet-11058#
--     If transform feedback is active,
--     'GeneratedCommandsInfoEXT'::@indirectExecutionSet@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-deviceGeneratedCommands-11059#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-supportedIndirectCommandsShaderStages-11060#
--     The bound shader stages /must/ be supported by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStages ::supportedIndirectCommandsShaderStages>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-supportedIndirectCommandsShaderStages-11061#
--     Only stages specified in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStages ::supportedIndirectCommandsShaderStages>
--     /can/ be set in @pGeneratedCommandsInfo->shaderStages@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-None-11062# If a rendering
--     pass is currently active, the view mask /must/ be @0@
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-11143#
--     @commandBuffer@ /must/ not have been created with
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-pGeneratedCommandsInfo-parameter#
--     @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoEXT' structure
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdExecuteGeneratedCommandsEXT-bufferlevel# @commandBuffer@
--     /must/ be a primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               | Indirection                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoEXT'
cmdExecuteGeneratedCommandsEXT :: forall a io
                                . ( Extendss GeneratedCommandsInfoEXT a
                                  , PokeChain a
                                  , MonadIO io )
                               => -- | @commandBuffer@ is the command buffer into which the command is
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @isPreprocessed@ represents whether the input data has already been
                                  -- preprocessed on the device. If it is
                                  -- 'Vulkan.Core10.FundamentalTypes.FALSE' this command will implicitly
                                  -- trigger the preprocessing step, otherwise not.
                                  ("isPreprocessed" ::: Bool)
                               -> -- | @pGeneratedCommandsInfo@ is a pointer to a 'GeneratedCommandsInfoEXT'
                                  -- structure containing parameters affecting the generation of commands.
                                  (GeneratedCommandsInfoEXT a)
                               -> io ()
cmdExecuteGeneratedCommandsEXT commandBuffer
                                 isPreprocessed
                                 generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdExecuteGeneratedCommandsEXTPtr = pVkCmdExecuteGeneratedCommandsEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdExecuteGeneratedCommandsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteGeneratedCommandsEXT is null" Nothing Nothing
  let vkCmdExecuteGeneratedCommandsEXT' = mkVkCmdExecuteGeneratedCommandsEXT vkCmdExecuteGeneratedCommandsEXTPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdExecuteGeneratedCommandsEXT" (vkCmdExecuteGeneratedCommandsEXT'
                                                                (commandBufferHandle (commandBuffer))
                                                                (boolToBool32 (isPreprocessed))
                                                                (forgetExtensions pGeneratedCommandsInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPreprocessGeneratedCommandsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct GeneratedCommandsInfoEXT) -> Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct GeneratedCommandsInfoEXT) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdPreprocessGeneratedCommandsEXT - Performs preprocessing for
-- generated commands
--
-- = Description
--
-- Note
--
-- @stateCommandBuffer@ access is not synchronized by the driver, meaning
-- that this command buffer /must/ not be modified between threads in an
-- unsafe manner.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-commandBuffer-11081#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-pGeneratedCommandsInfo-11082#
--     @pGeneratedCommandsInfo@’s @indirectCommandsLayout@ /must/ have been
--     created with the
--     'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT' bit set
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-indirectCommandsLayout-11084#
--     If the token sequence of the passed
--     'GeneratedCommandsInfoEXT'::@indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, the initial
--     shader state of 'GeneratedCommandsInfoEXT'::@indirectExecutionSet@
--     /must/ be bound on @stateCommandBuffer@
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-stateCommandBuffer-11138#
--     @stateCommandBuffer@ /must/ be in the recording state
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-deviceGeneratedCommands-11087#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-supportedIndirectCommandsShaderStages-11088#
--     Only stages specified in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStages ::supportedIndirectCommandsShaderStages>
--     /can/ be set in @pGeneratedCommandsInfo->shaderStages@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-pGeneratedCommandsInfo-parameter#
--     @pGeneratedCommandsInfo@ /must/ be a valid pointer to a valid
--     'GeneratedCommandsInfoEXT' structure
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-stateCommandBuffer-parameter#
--     @stateCommandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-bufferlevel#
--     @commandBuffer@ /must/ be a primary
--     'Vulkan.Core10.Handles.CommandBuffer'
--
-- -   #VUID-vkCmdPreprocessGeneratedCommandsEXT-commonparent# Both of
--     @commandBuffer@, and @stateCommandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to @stateCommandBuffer@ /must/ be externally
--     synchronized
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'GeneratedCommandsInfoEXT'
cmdPreprocessGeneratedCommandsEXT :: forall a io
                                   . ( Extendss GeneratedCommandsInfoEXT a
                                     , PokeChain a
                                     , MonadIO io )
                                  => -- | @commandBuffer@ is the command buffer which does the preprocessing.
                                     CommandBuffer
                                  -> -- | @pGeneratedCommandsInfo@ is a pointer to a 'GeneratedCommandsInfoEXT'
                                     -- structure containing parameters affecting the preprocessing step.
                                     (GeneratedCommandsInfoEXT a)
                                  -> -- | @stateCommandBuffer@ is a command buffer from which to snapshot current
                                     -- states affecting the preprocessing step. When a graphics command action
                                     -- token is used, graphics state is snapshotted. When a compute action
                                     -- command token is used, compute state is snapshotted. When a ray tracing
                                     -- action command token is used, ray tracing state is snapshotted. It can
                                     -- be deleted at any time after this command has been recorded.
                                     ("stateCommandBuffer" ::: CommandBuffer)
                                  -> io ()
cmdPreprocessGeneratedCommandsEXT commandBuffer
                                    generatedCommandsInfo
                                    stateCommandBuffer = liftIO . evalContT $ do
  let vkCmdPreprocessGeneratedCommandsEXTPtr = pVkCmdPreprocessGeneratedCommandsEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPreprocessGeneratedCommandsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPreprocessGeneratedCommandsEXT is null" Nothing Nothing
  let vkCmdPreprocessGeneratedCommandsEXT' = mkVkCmdPreprocessGeneratedCommandsEXT vkCmdPreprocessGeneratedCommandsEXTPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ traceAroundEvent "vkCmdPreprocessGeneratedCommandsEXT" (vkCmdPreprocessGeneratedCommandsEXT'
                                                                   (commandBufferHandle (commandBuffer))
                                                                   (forgetExtensions pGeneratedCommandsInfo)
                                                                   (commandBufferHandle (stateCommandBuffer)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGeneratedCommandsMemoryRequirementsEXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct GeneratedCommandsMemoryRequirementsInfoEXT) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct GeneratedCommandsMemoryRequirementsInfoEXT) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetGeneratedCommandsMemoryRequirementsEXT - Retrieve the buffer
-- allocation requirements for generated commands
--
-- = Description
--
-- If the size returned is zero, the preprocessing step can be skipped for
-- this layout.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Device',
-- 'GeneratedCommandsMemoryRequirementsInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getGeneratedCommandsMemoryRequirementsEXT :: forall a b io
                                           . ( Extendss GeneratedCommandsMemoryRequirementsInfoEXT a
                                             , PokeChain a
                                             , Extendss MemoryRequirements2 b
                                             , PokeChain b
                                             , PeekChain b
                                             , MonadIO io )
                                          => -- | @device@ is the logical device that owns the buffer.
                                             --
                                             -- #VUID-vkGetGeneratedCommandsMemoryRequirementsEXT-device-parameter#
                                             -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                             Device
                                          -> -- | @pInfo@ is a pointer to a 'GeneratedCommandsMemoryRequirementsInfoEXT'
                                             -- structure containing parameters required for the memory requirements
                                             -- query.
                                             --
                                             -- #VUID-vkGetGeneratedCommandsMemoryRequirementsEXT-pInfo-parameter#
                                             -- @pInfo@ /must/ be a valid pointer to a valid
                                             -- 'GeneratedCommandsMemoryRequirementsInfoEXT' structure
                                             (GeneratedCommandsMemoryRequirementsInfoEXT a)
                                          -> io (MemoryRequirements2 b)
getGeneratedCommandsMemoryRequirementsEXT device info = liftIO . evalContT $ do
  let vkGetGeneratedCommandsMemoryRequirementsEXTPtr = pVkGetGeneratedCommandsMemoryRequirementsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetGeneratedCommandsMemoryRequirementsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGeneratedCommandsMemoryRequirementsEXT is null" Nothing Nothing
  let vkGetGeneratedCommandsMemoryRequirementsEXT' = mkVkGetGeneratedCommandsMemoryRequirementsEXT vkGetGeneratedCommandsMemoryRequirementsEXTPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetGeneratedCommandsMemoryRequirementsEXT" (vkGetGeneratedCommandsMemoryRequirementsEXT'
                                                                           (deviceHandle (device))
                                                                           (forgetExtensions pInfo)
                                                                           (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutEXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct IndirectCommandsLayoutCreateInfoEXT) -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutEXT -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct IndirectCommandsLayoutCreateInfoEXT) -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutEXT -> IO Result

-- | vkCreateIndirectCommandsLayoutEXT - Create an indirect command layout
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateIndirectCommandsLayoutEXT-deviceGeneratedCommands-11089#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateIndirectCommandsLayoutEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateIndirectCommandsLayoutEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'IndirectCommandsLayoutCreateInfoEXT' structure
--
-- -   #VUID-vkCreateIndirectCommandsLayoutEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateIndirectCommandsLayoutEXT-pIndirectCommandsLayout-parameter#
--     @pIndirectCommandsLayout@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'IndirectCommandsLayoutCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT'
createIndirectCommandsLayoutEXT :: forall a io
                                 . ( Extendss IndirectCommandsLayoutCreateInfoEXT a
                                   , PokeChain a
                                   , MonadIO io )
                                => -- | @device@ is the logical device that creates the indirect command layout.
                                   Device
                                -> -- | @pCreateInfo@ is a pointer to a 'IndirectCommandsLayoutCreateInfoEXT'
                                   -- structure containing parameters affecting creation of the indirect
                                   -- command layout.
                                   (IndirectCommandsLayoutCreateInfoEXT a)
                                -> -- | @pAllocator@ controls host memory allocation as described in the
                                   -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                   -- chapter.
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io (IndirectCommandsLayoutEXT)
createIndirectCommandsLayoutEXT device
                                  createInfo
                                  allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutEXTPtr = pVkCreateIndirectCommandsLayoutEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateIndirectCommandsLayoutEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectCommandsLayoutEXT is null" Nothing Nothing
  let vkCreateIndirectCommandsLayoutEXT' = mkVkCreateIndirectCommandsLayoutEXT vkCreateIndirectCommandsLayoutEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateIndirectCommandsLayoutEXT" (vkCreateIndirectCommandsLayoutEXT'
                                                                      (deviceHandle (device))
                                                                      (forgetExtensions pCreateInfo)
                                                                      pAllocator
                                                                      (pPIndirectCommandsLayout))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectCommandsLayout <- lift $ peek @IndirectCommandsLayoutEXT pPIndirectCommandsLayout
  pure $ (pIndirectCommandsLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createIndirectCommandsLayoutEXT' and 'destroyIndirectCommandsLayoutEXT'
--
-- To ensure that 'destroyIndirectCommandsLayoutEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withIndirectCommandsLayoutEXT :: forall a io r . (Extendss IndirectCommandsLayoutCreateInfoEXT a, PokeChain a, MonadIO io) => Device -> IndirectCommandsLayoutCreateInfoEXT a -> Maybe AllocationCallbacks -> (io IndirectCommandsLayoutEXT -> (IndirectCommandsLayoutEXT -> io ()) -> r) -> r
withIndirectCommandsLayoutEXT device pCreateInfo pAllocator b =
  b (createIndirectCommandsLayoutEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectCommandsLayoutEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutEXT
  :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectCommandsLayoutEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyIndirectCommandsLayoutEXT - Destroy an indirect commands layout
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-indirectCommandsLayout-11114#
--     All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-indirectCommandsLayout-11115#
--     If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-indirectCommandsLayout-11116#
--     If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @indirectCommandsLayout@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-indirectCommandsLayout-parameter#
--     If @indirectCommandsLayout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectCommandsLayout@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' handle
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyIndirectCommandsLayoutEXT-indirectCommandsLayout-parent#
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT'
destroyIndirectCommandsLayoutEXT :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the logical device that destroys the layout.
                                    Device
                                 -> -- | @indirectCommandsLayout@ is the layout to destroy.
                                    IndirectCommandsLayoutEXT
                                 -> -- | @pAllocator@ controls host memory allocation as described in the
                                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                    -- chapter.
                                    ("allocator" ::: Maybe AllocationCallbacks)
                                 -> io ()
destroyIndirectCommandsLayoutEXT device
                                   indirectCommandsLayout
                                   allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutEXTPtr = pVkDestroyIndirectCommandsLayoutEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyIndirectCommandsLayoutEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectCommandsLayoutEXT is null" Nothing Nothing
  let vkDestroyIndirectCommandsLayoutEXT' = mkVkDestroyIndirectCommandsLayoutEXT vkDestroyIndirectCommandsLayoutEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyIndirectCommandsLayoutEXT" (vkDestroyIndirectCommandsLayoutEXT'
                                                                  (deviceHandle (device))
                                                                  (indirectCommandsLayout)
                                                                  pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectExecutionSetEXT
  :: FunPtr (Ptr Device_T -> Ptr IndirectExecutionSetCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr IndirectExecutionSetEXT -> IO Result) -> Ptr Device_T -> Ptr IndirectExecutionSetCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr IndirectExecutionSetEXT -> IO Result

-- | vkCreateIndirectExecutionSetEXT - Create an indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-vkCreateIndirectExecutionSetEXT-deviceGeneratedCommands-11013#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateIndirectExecutionSetEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateIndirectExecutionSetEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'IndirectExecutionSetCreateInfoEXT' structure
--
-- -   #VUID-vkCreateIndirectExecutionSetEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateIndirectExecutionSetEXT-pIndirectExecutionSet-parameter#
--     @pIndirectExecutionSet@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'IndirectExecutionSetCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT'
createIndirectExecutionSetEXT :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that creates the indirect execution set.
                                 Device
                              -> -- | @pCreateInfo@ is a pointer to a 'IndirectExecutionSetCreateInfoEXT'
                                 -- structure containing parameters affecting creation of the indirect
                                 -- execution set.
                                 IndirectExecutionSetCreateInfoEXT
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io (IndirectExecutionSetEXT)
createIndirectExecutionSetEXT device
                                createInfo
                                allocator = liftIO . evalContT $ do
  let vkCreateIndirectExecutionSetEXTPtr = pVkCreateIndirectExecutionSetEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateIndirectExecutionSetEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectExecutionSetEXT is null" Nothing Nothing
  let vkCreateIndirectExecutionSetEXT' = mkVkCreateIndirectExecutionSetEXT vkCreateIndirectExecutionSetEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectExecutionSet <- ContT $ bracket (callocBytes @IndirectExecutionSetEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateIndirectExecutionSetEXT" (vkCreateIndirectExecutionSetEXT'
                                                                    (deviceHandle (device))
                                                                    pCreateInfo
                                                                    pAllocator
                                                                    (pPIndirectExecutionSet))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectExecutionSet <- lift $ peek @IndirectExecutionSetEXT pPIndirectExecutionSet
  pure $ (pIndirectExecutionSet)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createIndirectExecutionSetEXT' and 'destroyIndirectExecutionSetEXT'
--
-- To ensure that 'destroyIndirectExecutionSetEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withIndirectExecutionSetEXT :: forall io r . MonadIO io => Device -> IndirectExecutionSetCreateInfoEXT -> Maybe AllocationCallbacks -> (io IndirectExecutionSetEXT -> (IndirectExecutionSetEXT -> io ()) -> r) -> r
withIndirectExecutionSetEXT device pCreateInfo pAllocator b =
  b (createIndirectExecutionSetEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectExecutionSetEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectExecutionSetEXT
  :: FunPtr (Ptr Device_T -> IndirectExecutionSetEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectExecutionSetEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyIndirectExecutionSetEXT - Destroy an indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyIndirectExecutionSetEXT-indirectExecutionSet-11025#
--     All submitted commands that refer to @indirectExecutionSet@ /must/
--     have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyIndirectExecutionSetEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyIndirectExecutionSetEXT-indirectExecutionSet-parameter#
--     If @indirectExecutionSet@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectExecutionSet@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
--
-- -   #VUID-vkDestroyIndirectExecutionSetEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyIndirectExecutionSetEXT-indirectExecutionSet-parent#
--     If @indirectExecutionSet@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @indirectExecutionSet@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT'
destroyIndirectExecutionSetEXT :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that owns the indirect execution set.
                                  Device
                               -> -- | @indirectExecutionSet@ is the indirect execution set to destroy.
                                  IndirectExecutionSetEXT
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io ()
destroyIndirectExecutionSetEXT device
                                 indirectExecutionSet
                                 allocator = liftIO . evalContT $ do
  let vkDestroyIndirectExecutionSetEXTPtr = pVkDestroyIndirectExecutionSetEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyIndirectExecutionSetEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectExecutionSetEXT is null" Nothing Nothing
  let vkDestroyIndirectExecutionSetEXT' = mkVkDestroyIndirectExecutionSetEXT vkDestroyIndirectExecutionSetEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyIndirectExecutionSetEXT" (vkDestroyIndirectExecutionSetEXT'
                                                                (deviceHandle (device))
                                                                (indirectExecutionSet)
                                                                pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateIndirectExecutionSetPipelineEXT
  :: FunPtr (Ptr Device_T -> IndirectExecutionSetEXT -> Word32 -> Ptr WriteIndirectExecutionSetPipelineEXT -> IO ()) -> Ptr Device_T -> IndirectExecutionSetEXT -> Word32 -> Ptr WriteIndirectExecutionSetPipelineEXT -> IO ()

-- | vkUpdateIndirectExecutionSetPipelineEXT - Update the contents of an
-- indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-indirectExecutionSet-11035#
--     @indirectExecutionSet@ /must/ have been created with type
--     'INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT'
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-executionSetWriteCount-11037#
--     @executionSetWriteCount@ /must/ be less than or equal to
--     'IndirectExecutionSetPipelineInfoEXT'::@maxPipelineCount@
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-pExecutionSetWrites-11042#
--     Each element in the @pExecutionSetWrites@ array must have a unique
--     'WriteIndirectExecutionSetPipelineEXT'::@index@
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-None-11038# Each
--     member of the Indirect Execution Set referenced by the update
--     command /must/ not be in use by the device
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-None-11039# The layout
--     of each pipeline in @pExecutionSetWrites@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the @initialPipeline@ used to create the Indirect Execution Set
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-None-11040# Each
--     pipeline in the Indirect Execution Set /must/ have identically
--     defined static and dynamic state values to the @initialPipeline@
--     used to create the Indirect Execution Set
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-initialPipeline-11147#
--     Each pipeline in the Indirect Execution Set /must/ have identically
--     defined
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-fragmentoutput fragment outputs interface>
--     to the @initialPipeline@ used to create the Indirect Execution Set
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-initialPipeline-11152#
--     Each pipeline in the Indirect Execution Set /must/ match the
--     @initialPipeline@ used to create the Indirect Execution Set in its
--     included shader stages
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-initialPipeline-11098#
--     Each pipeline in the Indirect Execution Set /must/ match the
--     @initialPipeline@ used to create the Indirect Execution Set in its
--     use of @FragDepth@
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-initialPipeline-11086#
--     Each pipeline in the Indirect Execution Set /must/ match the
--     @initialPipeline@ used to create the Indirect Execution Set in its
--     use of 'Vulkan.Core10.FundamentalTypes.SampleMask'
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-initialPipeline-11085#
--     Each pipeline in the Indirect Execution Set /must/ match the
--     @initialPipeline@ used to create the Indirect Execution Set in its
--     use of @StencilExportEXT@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-indirectExecutionSet-parameter#
--     @indirectExecutionSet@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-pExecutionSetWrites-parameter#
--     @pExecutionSetWrites@ /must/ be a valid pointer to an array of
--     @executionSetWriteCount@ valid
--     'WriteIndirectExecutionSetPipelineEXT' structures
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-executionSetWriteCount-arraylength#
--     @executionSetWriteCount@ /must/ be greater than @0@
--
-- -   #VUID-vkUpdateIndirectExecutionSetPipelineEXT-indirectExecutionSet-parent#
--     @indirectExecutionSet@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @indirectExecutionSet@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT',
-- 'WriteIndirectExecutionSetPipelineEXT'
updateIndirectExecutionSetPipelineEXT :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the logical device that owns the indirect execution set.
                                         Device
                                      -> -- | @indirectExecutionSet@ is the indirect execution set being updated.
                                         IndirectExecutionSetEXT
                                      -> -- | @pExecutionSetWrites@ is a pointer to an array of
                                         -- 'WriteIndirectExecutionSetPipelineEXT' structures describing the
                                         -- elements to update.
                                         ("executionSetWrites" ::: Vector WriteIndirectExecutionSetPipelineEXT)
                                      -> io ()
updateIndirectExecutionSetPipelineEXT device
                                        indirectExecutionSet
                                        executionSetWrites = liftIO . evalContT $ do
  let vkUpdateIndirectExecutionSetPipelineEXTPtr = pVkUpdateIndirectExecutionSetPipelineEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkUpdateIndirectExecutionSetPipelineEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUpdateIndirectExecutionSetPipelineEXT is null" Nothing Nothing
  let vkUpdateIndirectExecutionSetPipelineEXT' = mkVkUpdateIndirectExecutionSetPipelineEXT vkUpdateIndirectExecutionSetPipelineEXTPtr
  pPExecutionSetWrites <- ContT $ allocaBytes @WriteIndirectExecutionSetPipelineEXT ((Data.Vector.length (executionSetWrites)) * 32)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPExecutionSetWrites `plusPtr` (32 * (i)) :: Ptr WriteIndirectExecutionSetPipelineEXT) (e)) (executionSetWrites)
  lift $ traceAroundEvent "vkUpdateIndirectExecutionSetPipelineEXT" (vkUpdateIndirectExecutionSetPipelineEXT'
                                                                       (deviceHandle (device))
                                                                       (indirectExecutionSet)
                                                                       ((fromIntegral (Data.Vector.length $ (executionSetWrites)) :: Word32))
                                                                       (pPExecutionSetWrites))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateIndirectExecutionSetShaderEXT
  :: FunPtr (Ptr Device_T -> IndirectExecutionSetEXT -> Word32 -> Ptr WriteIndirectExecutionSetShaderEXT -> IO ()) -> Ptr Device_T -> IndirectExecutionSetEXT -> Word32 -> Ptr WriteIndirectExecutionSetShaderEXT -> IO ()

-- | vkUpdateIndirectExecutionSetShaderEXT - Update the contents of an
-- indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-indirectExecutionSet-11041#
--     @indirectExecutionSet@ /must/ have been created with type
--     'INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT'
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-pExecutionSetWrites-11043#
--     Each element in the @pExecutionSetWrites@ array must have a unique
--     'WriteIndirectExecutionSetShaderEXT'::@index@
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-None-11044# Each member
--     of the Indirect Execution Set referenced by the update command
--     /must/ not be in use by the device
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-pExecutionSetWrites-11140#
--     The descriptor layout of each shader in @pExecutionSetWrites@ /must/
--     be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the initial layout info used to create the Indirect Execution
--     Set
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-None-11148# Each
--     fragment shader element in the Indirect Execution Set /must/ have
--     identically defined
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-fragmentoutput fragment outputs interface>
--     to the initial shader state used to create the Indirect Execution
--     Set
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-FragDepth-11054# Each
--     fragment shader element in the Indirect Execution Set /must/ match
--     the initial shader state used to create the Indirect Execution Set
--     in its use of @FragDepth@
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-SampleMask-11050# Each
--     fragment shader element in the Indirect Execution Set /must/ match
--     the initial shader state used to create the Indirect Execution Set
--     in its use of 'Vulkan.Core10.FundamentalTypes.SampleMask'
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-StencilExportEXT-11003#
--     Each fragment shader element in the Indirect Execution Set /must/
--     match the initial shader state used to create the Indirect Execution
--     Set in its use of @StencilExportEXT@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-indirectExecutionSet-parameter#
--     @indirectExecutionSet@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-pExecutionSetWrites-parameter#
--     @pExecutionSetWrites@ /must/ be a valid pointer to an array of
--     @executionSetWriteCount@ valid 'WriteIndirectExecutionSetShaderEXT'
--     structures
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-executionSetWriteCount-arraylength#
--     @executionSetWriteCount@ /must/ be greater than @0@
--
-- -   #VUID-vkUpdateIndirectExecutionSetShaderEXT-indirectExecutionSet-parent#
--     @indirectExecutionSet@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @indirectExecutionSet@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT',
-- 'WriteIndirectExecutionSetShaderEXT'
updateIndirectExecutionSetShaderEXT :: forall io
                                     . (MonadIO io)
                                    => -- | @device@ is the logical device that owns the indirect execution set.
                                       Device
                                    -> -- | @indirectExecutionSet@ is the indirect execution set being updated.
                                       IndirectExecutionSetEXT
                                    -> -- | @pExecutionSetWrites@ is a pointer to an array of
                                       -- 'WriteIndirectExecutionSetShaderEXT' structures describing the elements
                                       -- to update.
                                       ("executionSetWrites" ::: Vector WriteIndirectExecutionSetShaderEXT)
                                    -> io ()
updateIndirectExecutionSetShaderEXT device
                                      indirectExecutionSet
                                      executionSetWrites = liftIO . evalContT $ do
  let vkUpdateIndirectExecutionSetShaderEXTPtr = pVkUpdateIndirectExecutionSetShaderEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkUpdateIndirectExecutionSetShaderEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUpdateIndirectExecutionSetShaderEXT is null" Nothing Nothing
  let vkUpdateIndirectExecutionSetShaderEXT' = mkVkUpdateIndirectExecutionSetShaderEXT vkUpdateIndirectExecutionSetShaderEXTPtr
  pPExecutionSetWrites <- ContT $ allocaBytes @WriteIndirectExecutionSetShaderEXT ((Data.Vector.length (executionSetWrites)) * 32)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPExecutionSetWrites `plusPtr` (32 * (i)) :: Ptr WriteIndirectExecutionSetShaderEXT) (e)) (executionSetWrites)
  lift $ traceAroundEvent "vkUpdateIndirectExecutionSetShaderEXT" (vkUpdateIndirectExecutionSetShaderEXT'
                                                                     (deviceHandle (device))
                                                                     (indirectExecutionSet)
                                                                     ((fromIntegral (Data.Vector.length $ (executionSetWrites)) :: Word32))
                                                                     (pPExecutionSetWrites))
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT"
pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT = PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV


-- No documentation found for TopLevel "VK_ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT"
pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_EXT = ACCESS_COMMAND_PREPROCESS_READ_BIT_NV


-- No documentation found for TopLevel "VK_ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT"
pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_EXT = ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV


-- | VkPhysicalDeviceDeviceGeneratedCommandsFeaturesEXT - Structure
-- describing the device-generated compute features that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT = PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT
  { -- | #features-deviceGeneratedCommands# @deviceGeneratedCommands@ indicates
    -- whether the implementation supports functionality to generate commands
    -- on the device.
    deviceGeneratedCommands :: Bool
  , -- | #features-dynamicGeneratedPipelineLayout#
    -- @dynamicGeneratedPipelineLayout@ indicates the implementation allows the
    -- @pipelineLayout@ member of 'IndirectCommandsLayoutCreateInfoEXT' to be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' /can/ be chained
    -- off those structures\' @pNext@ instead.
    dynamicGeneratedPipelineLayout :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommands))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (dynamicGeneratedPipelineLayout))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT where
  peekCStruct p = do
    deviceGeneratedCommands <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    dynamicGeneratedPipelineLayout <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT
             (bool32ToBool deviceGeneratedCommands)
             (bool32ToBool dynamicGeneratedPipelineLayout)

instance Storable PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT where
  zero = PhysicalDeviceDeviceGeneratedCommandsFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceDeviceGeneratedCommandsPropertiesEXT - Structure
-- describing push descriptor limits that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'IndirectCommandsInputModeFlagsEXT',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT = PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT
  { -- | #limits-maxIndirectPipelineCount# @maxIndirectPipelineCount@ is the
    -- maximum number of pipelines passed to 'createIndirectExecutionSetEXT'.
    maxIndirectPipelineCount :: Word32
  , -- | #limits-maxIndirectShaderObjectCount# @maxIndirectShaderObjectCount@ is
    -- the maximum number of shader objects passed to
    -- 'createIndirectExecutionSetEXT'. If this value is zero, binding shader
    -- objects indirectly is not supported.
    maxIndirectShaderObjectCount :: Word32
  , -- | #limits-maxIndirectSequenceCount# @maxIndirectSequenceCount@ is the
    -- maximum number of sequences in 'GeneratedCommandsInfoEXT' and in
    -- 'GeneratedCommandsMemoryRequirementsInfoEXT'.
    maxIndirectSequenceCount :: Word32
  , -- | #limits-maxIndirectCommandsTokenCount# @maxIndirectCommandsTokenCount@
    -- is the maximum number of tokens in
    -- 'IndirectCommandsLayoutCreateInfoEXT'.
    maxIndirectCommandsTokenCount :: Word32
  , -- | #limits-maxIndirectCommandsTokenOffset# @maxIndirectCommandsTokenOffset@
    -- is the maximum offset in 'IndirectCommandsLayoutTokenEXT'.
    maxIndirectCommandsTokenOffset :: Word32
  , -- | #limits-maxIndirectCommandsIndirectStride#
    -- @maxIndirectCommandsIndirectStride@ is the maximum stream stride in
    -- 'IndirectCommandsLayoutCreateInfoEXT'.
    maxIndirectCommandsIndirectStride :: Word32
  , -- | #limits-supportedIndirectCommandsInputModes#
    -- @supportedIndirectCommandsInputModes@ indicates the supported input
    -- modes.
    supportedIndirectCommandsInputModes :: IndirectCommandsInputModeFlagsEXT
  , -- | #limits-supportedIndirectCommandsShaderStages#
    -- @supportedIndirectCommandsShaderStages@ indicates the stages which /can/
    -- be used to generate indirect commands. Implementations are required to
    -- support, at minimum:
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'.
    supportedIndirectCommandsShaderStages :: ShaderStageFlags
  , -- | #limits-supportedIndirectCommandsShaderStagesPipelineBinding#
    -- @supportedIndirectCommandsShaderStagesPipelineBinding@ indicates the
    -- stages which /can/ be used within indirect execution sets for indirectly
    -- binding shader stages using pipelines.
    supportedIndirectCommandsShaderStagesPipelineBinding :: ShaderStageFlags
  , -- | #limits-supportedIndirectCommandsShaderStagesShaderBinding#
    -- @supportedIndirectCommandsShaderStagesShaderBinding@ indicates the
    -- stages which /can/ be used within indirect execution sets for indirectly
    -- binding shader stages using shader objects.
    supportedIndirectCommandsShaderStagesShaderBinding :: ShaderStageFlags
  , -- | #limits-deviceGeneratedCommandsTransformFeedback#
    -- @deviceGeneratedCommandsTransformFeedback@ indicates whether the
    -- implementation supports interactions with @VK_EXT_transform_feedback@
    -- for pipelines not created with
    -- 'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT'.
    deviceGeneratedCommandsTransformFeedback :: Bool
  , -- | #limits-deviceGeneratedCommandsMultiDrawIndirectCount#
    -- @deviceGeneratedCommandsMultiDrawIndirectCount@ indicates whether the
    -- implementation supports COUNT variants of multi-draw indirect tokens.
    deviceGeneratedCommandsMultiDrawIndirectCount :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxIndirectPipelineCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxIndirectShaderObjectCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxIndirectSequenceCount)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxIndirectCommandsTokenCount)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxIndirectCommandsTokenOffset)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxIndirectCommandsIndirectStride)
    poke ((p `plusPtr` 40 :: Ptr IndirectCommandsInputModeFlagsEXT)) (supportedIndirectCommandsInputModes)
    poke ((p `plusPtr` 44 :: Ptr ShaderStageFlags)) (supportedIndirectCommandsShaderStages)
    poke ((p `plusPtr` 48 :: Ptr ShaderStageFlags)) (supportedIndirectCommandsShaderStagesPipelineBinding)
    poke ((p `plusPtr` 52 :: Ptr ShaderStageFlags)) (supportedIndirectCommandsShaderStagesShaderBinding)
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommandsTransformFeedback))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommandsMultiDrawIndirectCount))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr IndirectCommandsInputModeFlagsEXT)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 48 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 52 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT where
  peekCStruct p = do
    maxIndirectPipelineCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxIndirectShaderObjectCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxIndirectSequenceCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxIndirectCommandsTokenCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxIndirectCommandsTokenOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxIndirectCommandsIndirectStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    supportedIndirectCommandsInputModes <- peek @IndirectCommandsInputModeFlagsEXT ((p `plusPtr` 40 :: Ptr IndirectCommandsInputModeFlagsEXT))
    supportedIndirectCommandsShaderStages <- peek @ShaderStageFlags ((p `plusPtr` 44 :: Ptr ShaderStageFlags))
    supportedIndirectCommandsShaderStagesPipelineBinding <- peek @ShaderStageFlags ((p `plusPtr` 48 :: Ptr ShaderStageFlags))
    supportedIndirectCommandsShaderStagesShaderBinding <- peek @ShaderStageFlags ((p `plusPtr` 52 :: Ptr ShaderStageFlags))
    deviceGeneratedCommandsTransformFeedback <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    deviceGeneratedCommandsMultiDrawIndirectCount <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT
             maxIndirectPipelineCount
             maxIndirectShaderObjectCount
             maxIndirectSequenceCount
             maxIndirectCommandsTokenCount
             maxIndirectCommandsTokenOffset
             maxIndirectCommandsIndirectStride
             supportedIndirectCommandsInputModes
             supportedIndirectCommandsShaderStages
             supportedIndirectCommandsShaderStagesPipelineBinding
             supportedIndirectCommandsShaderStagesShaderBinding
             (bool32ToBool deviceGeneratedCommandsTransformFeedback)
             (bool32ToBool deviceGeneratedCommandsMultiDrawIndirectCount)

instance Storable PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT where
  zero = PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT
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
           zero
           zero


-- | VkGeneratedCommandsPipelineInfoEXT - Structure specifying a pipeline for
-- use with indirect command preprocessing
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeneratedCommandsPipelineInfoEXT = GeneratedCommandsPipelineInfoEXT
  { -- | @pipeline@ is a valid pipeline object.
    --
    -- #VUID-VkGeneratedCommandsPipelineInfoEXT-pipeline-parameter# @pipeline@
    -- /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
    pipeline :: Pipeline }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsPipelineInfoEXT)
#endif
deriving instance Show GeneratedCommandsPipelineInfoEXT

instance ToCStruct GeneratedCommandsPipelineInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsPipelineInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_PIPELINE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct GeneratedCommandsPipelineInfoEXT where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    pure $ GeneratedCommandsPipelineInfoEXT
             pipeline

instance Storable GeneratedCommandsPipelineInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeneratedCommandsPipelineInfoEXT where
  zero = GeneratedCommandsPipelineInfoEXT
           zero


-- | VkGeneratedCommandsShaderInfoEXT - Structure specifying shader objects
-- for use with indirect command preprocessing
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Extensions.Handles.ShaderEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeneratedCommandsShaderInfoEXT = GeneratedCommandsShaderInfoEXT
  { -- | @pShaders@ is a pointer to an array of shader objects.
    --
    -- #VUID-VkGeneratedCommandsShaderInfoEXT-pShaders-11127# @pShaders@ /must/
    -- not contain more than one shader object for a given
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' stage
    --
    -- #VUID-VkGeneratedCommandsShaderInfoEXT-pShaders-parameter# @pShaders@
    -- /must/ be a valid pointer to an array of @shaderCount@ valid
    -- 'Vulkan.Extensions.Handles.ShaderEXT' handles
    shaders :: Vector ShaderEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsShaderInfoEXT)
#endif
deriving instance Show GeneratedCommandsShaderInfoEXT

instance ToCStruct GeneratedCommandsShaderInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsShaderInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (shaders)) :: Word32))
    pPShaders' <- ContT $ allocaBytes @ShaderEXT ((Data.Vector.length (shaders)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPShaders' `plusPtr` (8 * (i)) :: Ptr ShaderEXT) (e)) (shaders)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShaderEXT))) (pPShaders')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_SHADER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct GeneratedCommandsShaderInfoEXT where
  peekCStruct p = do
    shaderCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pShaders <- peek @(Ptr ShaderEXT) ((p `plusPtr` 24 :: Ptr (Ptr ShaderEXT)))
    pShaders' <- generateM (fromIntegral shaderCount) (\i -> peek @ShaderEXT ((pShaders `advancePtrBytes` (8 * (i)) :: Ptr ShaderEXT)))
    pure $ GeneratedCommandsShaderInfoEXT
             pShaders'

instance Zero GeneratedCommandsShaderInfoEXT where
  zero = GeneratedCommandsShaderInfoEXT
           mempty


-- | VkGeneratedCommandsMemoryRequirementsInfoEXT - Structure specifying
-- parameters for the reservation of preprocess buffer space
--
-- = Description
--
-- If the action command token for the layout is not a COUNT-type
-- multi-draw indirect token, @maxDrawCount@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-maxSequencesCount-11009#
--     @maxSequencesCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectSequenceCount@
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectCommandsLayout-11010#
--     If @indirectCommandsLayout@ was created with a token sequence that
--     contained the 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'
--     token, @indirectExecutionSet@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectCommandsLayout-11151#
--     If @indirectCommandsLayout@ was created with a token sequence that
--     contained the 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'
--     token, the shader stages used to create the initial shader state of
--     @indirectExecutionSet@ /must/ equal the
--     'IndirectCommandsExecutionSetTokenEXT'::@shaderStages@ used to
--     create @indirectCommandsLayout@
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectCommandsLayout-11011#
--     If @indirectCommandsLayout@ was not created with a token sequence
--     that contained the 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'
--     token, @indirectExecutionSet@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-maxDrawCount-11146#
--     When not ignored, @maxDrawCount@ × @maxSequenceCount@ /must/ be less
--     than 2^24
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectExecutionSet-11012#
--     If @indirectExecutionSet@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', either a
--     'GeneratedCommandsPipelineInfoEXT' or a
--     'GeneratedCommandsShaderInfoEXT' /must/ be included in the @pNext@
--     chain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT'
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-pNext-pNext# Each
--     @pNext@ member of any structure (including this one) in the @pNext@
--     chain /must/ be either @NULL@ or a pointer to a valid instance of
--     'GeneratedCommandsPipelineInfoEXT' or
--     'GeneratedCommandsShaderInfoEXT'
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectExecutionSet-parameter#
--     If @indirectExecutionSet@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectExecutionSet@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-indirectCommandsLayout-parameter#
--     @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' handle
--
-- -   #VUID-VkGeneratedCommandsMemoryRequirementsInfoEXT-commonparent#
--     Both of @indirectCommandsLayout@, and @indirectExecutionSet@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getGeneratedCommandsMemoryRequirementsEXT'
data GeneratedCommandsMemoryRequirementsInfoEXT (es :: [Type]) = GeneratedCommandsMemoryRequirementsInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @indirectExecutionSet@ is the indirect execution set to be used for
    -- binding shaders.
    indirectExecutionSet :: IndirectExecutionSetEXT
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' that this buffer
    -- memory is intended to be used with.
    indirectCommandsLayout :: IndirectCommandsLayoutEXT
  , -- | @maxSequenceCount@ is the maximum number of sequences that this buffer
    -- memory can be used with.
    maxSequenceCount :: Word32
  , -- | @maxDrawCount@ is the maximum number of indirect draws that can be
    -- executed by any COUNT-type multi-draw indirect tokens. The draw count in
    -- the indirect buffer is clamped to this value for these token types.
    maxDrawCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsMemoryRequirementsInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (GeneratedCommandsMemoryRequirementsInfoEXT es)

instance Extensible GeneratedCommandsMemoryRequirementsInfoEXT where
  extensibleTypeName = "GeneratedCommandsMemoryRequirementsInfoEXT"
  setNext GeneratedCommandsMemoryRequirementsInfoEXT{..} next' = GeneratedCommandsMemoryRequirementsInfoEXT{next = next', ..}
  getNext GeneratedCommandsMemoryRequirementsInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends GeneratedCommandsMemoryRequirementsInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @GeneratedCommandsShaderInfoEXT = Just f
    | Just Refl <- eqT @e @GeneratedCommandsPipelineInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss GeneratedCommandsMemoryRequirementsInfoEXT es
         , PokeChain es ) => ToCStruct (GeneratedCommandsMemoryRequirementsInfoEXT es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsMemoryRequirementsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectExecutionSetEXT)) (indirectExecutionSet)
    lift $ poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutEXT)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (maxSequenceCount)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (maxDrawCount)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutEXT)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss GeneratedCommandsMemoryRequirementsInfoEXT es
         , PeekChain es ) => FromCStruct (GeneratedCommandsMemoryRequirementsInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    indirectExecutionSet <- peek @IndirectExecutionSetEXT ((p `plusPtr` 16 :: Ptr IndirectExecutionSetEXT))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutEXT ((p `plusPtr` 24 :: Ptr IndirectCommandsLayoutEXT))
    maxSequenceCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxDrawCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pure $ GeneratedCommandsMemoryRequirementsInfoEXT
             next
             indirectExecutionSet
             indirectCommandsLayout
             maxSequenceCount
             maxDrawCount

instance es ~ '[] => Zero (GeneratedCommandsMemoryRequirementsInfoEXT es) where
  zero = GeneratedCommandsMemoryRequirementsInfoEXT
           ()
           zero
           zero
           zero
           zero


-- | VkIndirectExecutionSetPipelineInfoEXT - Struct specifying parameters of
-- a newly created indirect execution set containing only pipelines
--
-- = Description
--
-- The characteristics of @initialPipeline@ will be used to validate all
-- pipelines added to the set even if they are removed from the set or
-- destroyed.
--
-- When an Indirect Execution Set created with pipelines is used,
-- @initialPipeline@ constitutes the initial shader state.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-supportedIndirectCommandsShaderStagesPipelineBinding-11015#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesPipelineBinding ::supportedIndirectCommandsShaderStagesPipelineBinding>
--     does not contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     the 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' of
--     @initialPipeline@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-supportedIndirectCommandsShaderStagesPipelineBinding-11016#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesPipelineBinding ::supportedIndirectCommandsShaderStagesPipelineBinding>
--     does not contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     the 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' of
--     @initialPipeline@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-supportedIndirectCommandsShaderStagesPipelineBinding-11017#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesPipelineBinding ::supportedIndirectCommandsShaderStagesPipelineBinding>
--     does not contain ray tracing stages, the
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' of
--     @initialPipeline@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR'
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-maxPipelineCount-11018#
--     @maxPipelineCount@ /must/ be between @1@ and
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectPipelineCount@
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-initialPipeline-11019#
--     @initialPipeline@ /must/ not use descriptors of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-initialPipeline-11153#
--     @initialPipeline@ /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT'
--
-- -   #VUID-VkIndirectExecutionSetPipelineInfoEXT-initialPipeline-parameter#
--     @initialPipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectExecutionSetInfoEXT', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectExecutionSetPipelineInfoEXT = IndirectExecutionSetPipelineInfoEXT
  { -- | @initialPipeline@ is the initial pipeline for the set. This pipeline
    -- will be automatically added to the set at index @0@.
    initialPipeline :: Pipeline
  , -- | @maxPipelineCount@ is the maximum number of pipelines stored in the set.
    maxPipelineCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectExecutionSetPipelineInfoEXT)
#endif
deriving instance Show IndirectExecutionSetPipelineInfoEXT

instance ToCStruct IndirectExecutionSetPipelineInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectExecutionSetPipelineInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (initialPipeline)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxPipelineCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_PIPELINE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectExecutionSetPipelineInfoEXT where
  peekCStruct p = do
    initialPipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    maxPipelineCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ IndirectExecutionSetPipelineInfoEXT
             initialPipeline maxPipelineCount

instance Storable IndirectExecutionSetPipelineInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectExecutionSetPipelineInfoEXT where
  zero = IndirectExecutionSetPipelineInfoEXT
           zero
           zero


-- | VkIndirectExecutionSetShaderLayoutInfoEXT - Struct specifying descriptor
-- layout parameters of a newly created indirect execution set containing
-- only shader objects
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectExecutionSetShaderLayoutInfoEXT-pSetLayouts-11024#
--     All members of @pSetLayouts@ /must/ not contain descriptors of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectExecutionSetShaderLayoutInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT'
--
-- -   #VUID-VkIndirectExecutionSetShaderLayoutInfoEXT-pSetLayouts-parameter#
--     If @setLayoutCount@ is not @0@, @pSetLayouts@ /must/ be a valid
--     pointer to an array of @setLayoutCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handles
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.DescriptorSetLayout',
-- 'IndirectExecutionSetShaderInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectExecutionSetShaderLayoutInfoEXT = IndirectExecutionSetShaderLayoutInfoEXT
  { -- | @pSetLayouts@ is a pointer to an array containing
    -- 'Vulkan.Core10.Handles.DescriptorSetLayout' objects used by the shader
    -- stage.
    setLayouts :: Vector DescriptorSetLayout }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectExecutionSetShaderLayoutInfoEXT)
#endif
deriving instance Show IndirectExecutionSetShaderLayoutInfoEXT

instance ToCStruct IndirectExecutionSetShaderLayoutInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectExecutionSetShaderLayoutInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (setLayouts)) :: Word32))
    pPSetLayouts' <- ContT $ allocaBytes @DescriptorSetLayout ((Data.Vector.length (setLayouts)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (setLayouts)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_LAYOUT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct IndirectExecutionSetShaderLayoutInfoEXT where
  peekCStruct p = do
    setLayoutCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pSetLayouts <- peek @(Ptr DescriptorSetLayout) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayout)))
    pSetLayouts' <- generateM (fromIntegral setLayoutCount) (\i -> peek @DescriptorSetLayout ((pSetLayouts `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSetLayout)))
    pure $ IndirectExecutionSetShaderLayoutInfoEXT
             pSetLayouts'

instance Zero IndirectExecutionSetShaderLayoutInfoEXT where
  zero = IndirectExecutionSetShaderLayoutInfoEXT
           mempty


-- | VkIndirectExecutionSetShaderInfoEXT - Struct specifying parameters of a
-- newly created indirect execution set containing only shader objects
--
-- = Description
--
-- The characteristics of @pInitialShaders@ will be used to validate all
-- shaders added to the set even if they are removed from the set or
-- destroyed.
--
-- When an Indirect Execution Set created with shader objects is used,
-- @pInitialShaders@ constitutes the initial shader state.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-pInitialShaders-11020# All
--     members of @pInitialShaders@ /must/ have a @stage@ supported by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesShaderBinding ::supportedIndirectCommandsShaderStagesShaderBinding>
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-maxShaderCount-11021#
--     @maxShaderCount@ /must/ not be zero
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-maxShaderCount-11022#
--     @maxShaderCount@ /must/ be less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectShaderObjectCount@
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-maxShaderCount-11036#
--     @maxShaderCount@ /must/ be greater than or equal to @shaderCount@
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-stage-11023# The @stage@
--     of each element in the @pInitialShaders@ array /must/ be unique
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-pInitialShaders-11154#
--     Each member of @pInitialShaders@ /must/ have been created with
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDIRECT_BINDABLE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT'
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-pInitialShaders-parameter#
--     @pInitialShaders@ /must/ be a valid pointer to an array of
--     @shaderCount@ valid 'Vulkan.Extensions.Handles.ShaderEXT' handles
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-pSetLayoutInfos-parameter#
--     If @pSetLayoutInfos@ is not @NULL@, @pSetLayoutInfos@ /must/ be a
--     valid pointer to an array of @shaderCount@ valid
--     'IndirectExecutionSetShaderLayoutInfoEXT' structures
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-pPushConstantRanges-parameter#
--     If @pushConstantRangeCount@ is not @0@, @pPushConstantRanges@ /must/
--     be a valid pointer to an array of @pushConstantRangeCount@ valid
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' structures
--
-- -   #VUID-VkIndirectExecutionSetShaderInfoEXT-shaderCount-arraylength#
--     @shaderCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectExecutionSetInfoEXT',
-- 'IndirectExecutionSetShaderLayoutInfoEXT',
-- 'Vulkan.Core10.PipelineLayout.PushConstantRange',
-- 'Vulkan.Extensions.Handles.ShaderEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectExecutionSetShaderInfoEXT = IndirectExecutionSetShaderInfoEXT
  { -- | @pInitialShaders@ is a pointer to an array containing a
    -- 'Vulkan.Extensions.Handles.ShaderEXT' object for each shader stage that
    -- will be used in the set. These shaders will be automatically added to
    -- the set beginning at index @0@.
    initialShaders :: Vector ShaderEXT
  , -- | @pSetLayoutInfos@ is a pointer to an array containing a
    -- 'IndirectExecutionSetShaderLayoutInfoEXT' used by each corresponding
    -- @pInitialShaders@ shader stage in the set.
    setLayoutInfos :: Vector IndirectExecutionSetShaderLayoutInfoEXT
  , -- | @maxShaderCount@ is the maximum number of shader objects stored in the
    -- set.
    maxShaderCount :: Word32
  , -- | @pPushConstantRanges@ is a pointer to the array of
    -- 'Vulkan.Core10.PipelineLayout.PushConstantRange' ranges used by all
    -- shaders in the set.
    pushConstantRanges :: Vector PushConstantRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectExecutionSetShaderInfoEXT)
#endif
deriving instance Show IndirectExecutionSetShaderInfoEXT

instance ToCStruct IndirectExecutionSetShaderInfoEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectExecutionSetShaderInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pInitialShadersLength = Data.Vector.length $ (initialShaders)
    let pSetLayoutInfosLength = Data.Vector.length $ (setLayoutInfos)
    lift $ unless (fromIntegral pSetLayoutInfosLength == pInitialShadersLength || pSetLayoutInfosLength == 0) $
      throwIO $ IOError Nothing InvalidArgument "" "pSetLayoutInfos and pInitialShaders must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral pInitialShadersLength :: Word32))
    pPInitialShaders' <- ContT $ allocaBytes @ShaderEXT ((Data.Vector.length (initialShaders)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPInitialShaders' `plusPtr` (8 * (i)) :: Ptr ShaderEXT) (e)) (initialShaders)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ShaderEXT))) (pPInitialShaders')
    pSetLayoutInfos'' <- if Data.Vector.null (setLayoutInfos)
      then pure nullPtr
      else do
        pPSetLayoutInfos <- ContT $ allocaBytes @IndirectExecutionSetShaderLayoutInfoEXT (((Data.Vector.length (setLayoutInfos))) * 32)
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPSetLayoutInfos `plusPtr` (32 * (i)) :: Ptr IndirectExecutionSetShaderLayoutInfoEXT) (e) . ($ ())) ((setLayoutInfos))
        pure $ pPSetLayoutInfos
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectExecutionSetShaderLayoutInfoEXT))) pSetLayoutInfos''
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (maxShaderCount)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pushConstantRanges)) :: Word32))
    pPPushConstantRanges' <- ContT $ allocaBytes @PushConstantRange ((Data.Vector.length (pushConstantRanges)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPushConstantRanges' `plusPtr` (12 * (i)) :: Ptr PushConstantRange) (e)) (pushConstantRanges)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr PushConstantRange))) (pPPushConstantRanges')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_SHADER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectExecutionSetShaderInfoEXT where
  peekCStruct p = do
    shaderCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pInitialShaders <- peek @(Ptr ShaderEXT) ((p `plusPtr` 24 :: Ptr (Ptr ShaderEXT)))
    pInitialShaders' <- generateM (fromIntegral shaderCount) (\i -> peek @ShaderEXT ((pInitialShaders `advancePtrBytes` (8 * (i)) :: Ptr ShaderEXT)))
    pSetLayoutInfos <- peek @(Ptr IndirectExecutionSetShaderLayoutInfoEXT) ((p `plusPtr` 32 :: Ptr (Ptr IndirectExecutionSetShaderLayoutInfoEXT)))
    let pSetLayoutInfosLength = if pSetLayoutInfos == nullPtr then 0 else (fromIntegral shaderCount)
    pSetLayoutInfos' <- generateM pSetLayoutInfosLength (\i -> peekCStruct @IndirectExecutionSetShaderLayoutInfoEXT ((pSetLayoutInfos `advancePtrBytes` (32 * (i)) :: Ptr IndirectExecutionSetShaderLayoutInfoEXT)))
    maxShaderCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pushConstantRangeCount <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pPushConstantRanges <- peek @(Ptr PushConstantRange) ((p `plusPtr` 48 :: Ptr (Ptr PushConstantRange)))
    pPushConstantRanges' <- generateM (fromIntegral pushConstantRangeCount) (\i -> peekCStruct @PushConstantRange ((pPushConstantRanges `advancePtrBytes` (12 * (i)) :: Ptr PushConstantRange)))
    pure $ IndirectExecutionSetShaderInfoEXT
             pInitialShaders'
             pSetLayoutInfos'
             maxShaderCount
             pPushConstantRanges'

instance Zero IndirectExecutionSetShaderInfoEXT where
  zero = IndirectExecutionSetShaderInfoEXT
           mempty
           mempty
           zero
           mempty


-- | VkIndirectExecutionSetCreateInfoEXT - Structure specifying parameters of
-- a newly created indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectExecutionSetCreateInfoEXT-maxIndirectShaderObjectCount-11014#
--     If
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectShaderObjectCount@
--     is zero or the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled @type@ /must/ not be
--     'INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectExecutionSetCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT'
--
-- -   #VUID-VkIndirectExecutionSetCreateInfoEXT-type-parameter# @type@
--     /must/ be a valid 'IndirectExecutionSetInfoTypeEXT' value
--
-- -   #VUID-VkIndirectExecutionSetCreateInfoEXT-pPipelineInfo-parameter#
--     If @type@ is 'INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT', the
--     @pPipelineInfo@ member of @info@ /must/ be a valid pointer to a
--     valid 'IndirectExecutionSetPipelineInfoEXT' structure
--
-- -   #VUID-VkIndirectExecutionSetCreateInfoEXT-pShaderInfo-parameter# If
--     @type@ is 'INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT', the
--     @pShaderInfo@ member of @info@ /must/ be a valid pointer to a valid
--     'IndirectExecutionSetShaderInfoEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectExecutionSetInfoEXT', 'IndirectExecutionSetInfoTypeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createIndirectExecutionSetEXT'
data IndirectExecutionSetCreateInfoEXT = IndirectExecutionSetCreateInfoEXT
  { -- | @type@ is a 'IndirectExecutionSetInfoTypeEXT' describing the type of set
    -- being created and determining which field of the @info@ union will be
    -- used.
    type' :: IndirectExecutionSetInfoTypeEXT
  , -- | @info@ is a 'IndirectExecutionSetInfoEXT' union containing layout
    -- information for the set.
    info :: IndirectExecutionSetInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectExecutionSetCreateInfoEXT)
#endif
deriving instance Show IndirectExecutionSetCreateInfoEXT

instance ToCStruct IndirectExecutionSetCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectExecutionSetCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectExecutionSetInfoTypeEXT)) (type')
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr IndirectExecutionSetInfoEXT)) (info) . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_EXECUTION_SET_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectExecutionSetInfoTypeEXT)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr IndirectExecutionSetInfoEXT)) (zero) . ($ ())
    lift $ f

instance Zero IndirectExecutionSetCreateInfoEXT where
  zero = IndirectExecutionSetCreateInfoEXT
           zero
           zero


-- | VkGeneratedCommandsInfoEXT - Structure specifying parameters for the
-- generation of commands
--
-- = Description
--
-- If @sequenceCountAddress@ is not @NULL@, then @maxSequenceCount@ is the
-- maximum number of sequences that can be executed. The actual number is
-- @min(maxSequenceCount, *sequenceCountAddress)@. If
-- @sequenceCountAddress@ is @NULL@, then @maxSequenceCount@ is the exact
-- number of sequences to execute.
--
-- If the action command token for the layout is not a COUNT-type
-- multi-draw indirect token, @maxDrawCount@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-preprocessAddress-11063# If
--     'getGeneratedCommandsMemoryRequirementsEXT' returns a non-zero size,
--     @preprocessAddress@ /must/ not be @NULL@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-preprocessAddress-11064#
--     'Vulkan.Core10.Handles.DeviceMemory' objects bound to the underlying
--     buffer for @preprocessAddress@ /must/ have been allocated using one
--     of the memory types allowed in the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned by 'getGeneratedCommandsMemoryRequirementsEXT'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11065# If
--     the @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT', then the
--     @indirectExecutionSet@’s push constant layout /must/ contain the
--     @updateRange@ specified in 'IndirectCommandsPushConstantTokenEXT'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11066# If
--     the @indirectCommandsLayout@ uses a token of
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT', then the
--     @indirectExecutionSet@’s push constant layout /must/ contain the
--     @updateRange@ specified in 'IndirectCommandsPushConstantTokenEXT'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-maxSequenceCount-11067#
--     @maxSequenceCount@ /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectSequenceCount@
--     and
--     'GeneratedCommandsMemoryRequirementsInfoEXT'::@maxSequencesCount@
--     that was used to determine the @preprocessSize@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-sequenceCountAddress-11068# If
--     @sequenceCountAddress@ is not @NULL@, the value contained in the
--     address /must/ be less or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectSequenceCount@
--     and
--     'GeneratedCommandsMemoryRequirementsInfoEXT'::@maxSequencesCount@
--     that was used to determine the @preprocessSize@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-maxSequenceCount-10246#
--     @maxSequenceCount@ /must/ not be zero
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-preprocessAddress-11069# The
--     underlying buffer for @preprocessAddress@ /must/ have the
--     'Vulkan.Extensions.VK_KHR_maintenance5.BUFFER_USAGE_2_PREPROCESS_BUFFER_BIT_EXT'
--     bit set in its usage flag
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-preprocessAddress-11070# If the
--     underlying buffer for @preprocessAddress@ is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11144# If
--     the @indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, then the
--     descriptor and push constant layout info provided either by
--     @pipelineLayout@ or through a
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' in @pNext@
--     of the 'IndirectCommandsLayoutCreateInfoEXT' used to create
--     @indirectCommandsLayout@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the descriptor and push constant layout info used by
--     @indirectExecutionSet@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11002# If
--     @indirectCommandsLayout@ was created with a token sequence that
--     contained the 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'
--     token, the shader stages used to create the initial shader state of
--     @indirectExecutionSet@ /must/ equal the
--     'IndirectCommandsExecutionSetTokenEXT'::@shaderStages@ used to
--     create @indirectCommandsLayout@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-preprocessSize-11071#
--     @preprocessSize@ /must/ be greater than or equal to the memory
--     requirement’s size returned by
--     'getGeneratedCommandsMemoryRequirementsEXT' using the matching
--     inputs (@indirectCommandsLayout@, …​) as within this structure
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-sequenceCountAddress-11072# The
--     underlying buffer for @sequenceCountAddress@ /must/ have the
--     'Vulkan.Extensions.VK_KHR_maintenance5.BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR'
--     bit set in its usage flag
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-sequenceCountAddress-11073# If
--     @sequenceCountAddress@ is not @NULL@, @sequenceCountAddress@ /must/
--     be aligned to @4@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectAddress-11074#
--     @indirectAddress@ /must/ be aligned to @4@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-sequenceCountAddress-11075# If the
--     underlying buffer for @sequenceCountAddress@ is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectAddress-11076#
--     @indirectAddress@ /must/ not be @NULL@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectAddressSize-11077#
--     @indirectAddressSize@ /must/ be greater than zero
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-maxDrawCount-11078# When not
--     ignored, @maxDrawCount@ × @maxSequenceCount@ /must/ be less than
--     2^24
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11079# If
--     @indirectCommandsLayout@ was created using a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT' token and shader
--     objects are not bound then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE'
--     in @pDynamicStates@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-11083# If
--     the token sequence of the passed @indirectCommandsLayout@ contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, the
--     @indirectExecutionSet@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-10241# If
--     the token sequence of the passed @indirectCommandsLayout@ does not
--     contains a 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token,
--     the @indirectExecutionSet@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectExecutionSet-11080# If
--     @indirectExecutionSet@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     a 'GeneratedCommandsPipelineInfoEXT' or
--     'GeneratedCommandsShaderInfoEXT' /must/ be included in the @pNext@
--     chain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT'
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-shaderStages-parameter#
--     @shaderStages@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-shaderStages-requiredbitmask#
--     @shaderStages@ /must/ not be @0@
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectExecutionSet-parameter# If
--     @indirectExecutionSet@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @indirectExecutionSet@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT' handle
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-indirectCommandsLayout-parameter#
--     @indirectCommandsLayout@ /must/ be a valid
--     'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' handle
--
-- -   #VUID-VkGeneratedCommandsInfoEXT-commonparent# Both of
--     @indirectCommandsLayout@, and @indirectExecutionSet@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT',
-- 'Vulkan.Extensions.Handles.IndirectExecutionSetEXT',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdExecuteGeneratedCommandsEXT', 'cmdPreprocessGeneratedCommandsEXT'
data GeneratedCommandsInfoEXT (es :: [Type]) = GeneratedCommandsInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @shaderStages@ is the mask of shader stages used by the commands.
    shaderStages :: ShaderStageFlags
  , -- | @indirectExecutionSet@ is the indirect execution set to be used for
    -- binding shaders.
    indirectExecutionSet :: IndirectExecutionSetEXT
  , -- | @indirectCommandsLayout@ is the
    -- 'Vulkan.Extensions.Handles.IndirectCommandsLayoutEXT' that specifies the
    -- command sequence data.
    indirectCommandsLayout :: IndirectCommandsLayoutEXT
  , -- | @indirectAddress@ is an address that holds the indirect buffer data.
    indirectAddress :: DeviceAddress
  , -- | @indirectAddressSize@ is the size in bytes of indirect buffer data
    -- starting at @indirectAddress@.
    indirectAddressSize :: DeviceSize
  , -- | @preprocessAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used for preprocessing the input data for
    -- execution. If this structure is used with
    -- 'cmdExecuteGeneratedCommandsEXT' with its @isPreprocessed@ set to
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', then the preprocessing step is
    -- skipped but data in this address /may/ still be modified. The contents
    -- and the layout of this address are opaque to applications and /must/ not
    -- be modified outside functions related to device-generated commands or
    -- copied to another buffer for reuse.
    preprocessAddress :: DeviceAddress
  , -- | @preprocessSize@ is the maximum byte size within @preprocessAddress@
    -- that is available for preprocessing.
    preprocessSize :: DeviceSize
  , -- | @maxSequenceCount@ is used to determine the number of sequences to
    -- execute.
    maxSequenceCount :: Word32
  , -- | @sequenceCountAddress@ specifies an optional physical address of a
    -- single @uint32_t@ value containing the requested number of sequences to
    -- execute.
    sequenceCountAddress :: DeviceAddress
  , -- | @maxDrawCount@ is the maximum number of indirect draws that can be
    -- executed by any COUNT-type multi-draw indirect tokens. The draw count in
    -- the indirect buffer is clamped to this value for these token types.
    maxDrawCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (GeneratedCommandsInfoEXT es)

instance Extensible GeneratedCommandsInfoEXT where
  extensibleTypeName = "GeneratedCommandsInfoEXT"
  setNext GeneratedCommandsInfoEXT{..} next' = GeneratedCommandsInfoEXT{next = next', ..}
  getNext GeneratedCommandsInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends GeneratedCommandsInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @GeneratedCommandsShaderInfoEXT = Just f
    | Just Refl <- eqT @e @GeneratedCommandsPipelineInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss GeneratedCommandsInfoEXT es
         , PokeChain es ) => ToCStruct (GeneratedCommandsInfoEXT es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (shaderStages)
    lift $ poke ((p `plusPtr` 24 :: Ptr IndirectExecutionSetEXT)) (indirectExecutionSet)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutEXT)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (indirectAddress)
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (indirectAddressSize)
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceAddress)) (preprocessAddress)
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (preprocessSize)
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) (maxSequenceCount)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceAddress)) (sequenceCountAddress)
    lift $ poke ((p `plusPtr` 88 :: Ptr Word32)) (maxDrawCount)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutEXT)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss GeneratedCommandsInfoEXT es
         , PeekChain es ) => FromCStruct (GeneratedCommandsInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    shaderStages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    indirectExecutionSet <- peek @IndirectExecutionSetEXT ((p `plusPtr` 24 :: Ptr IndirectExecutionSetEXT))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutEXT ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutEXT))
    indirectAddress <- peek @DeviceAddress ((p `plusPtr` 40 :: Ptr DeviceAddress))
    indirectAddressSize <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    preprocessAddress <- peek @DeviceAddress ((p `plusPtr` 56 :: Ptr DeviceAddress))
    preprocessSize <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    maxSequenceCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    sequenceCountAddress <- peek @DeviceAddress ((p `plusPtr` 80 :: Ptr DeviceAddress))
    maxDrawCount <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    pure $ GeneratedCommandsInfoEXT
             next
             shaderStages
             indirectExecutionSet
             indirectCommandsLayout
             indirectAddress
             indirectAddressSize
             preprocessAddress
             preprocessSize
             maxSequenceCount
             sequenceCountAddress
             maxDrawCount

instance es ~ '[] => Zero (GeneratedCommandsInfoEXT es) where
  zero = GeneratedCommandsInfoEXT
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


-- | VkWriteIndirectExecutionSetPipelineEXT - Struct specifying pipeline
-- update information for an indirect execution set
--
-- == Valid Usage
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-index-11026# @index@
--     /must/ be less than the value of
--     'IndirectExecutionSetPipelineInfoEXT'::@maxPipelineCount@ used to
--     create the set
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-pipeline-11027#
--     @pipeline@ /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT'
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-index-11029# @index@
--     /must/ not be referenced by submitted command buffers
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-pipeline-11030# The
--     shader stages contained in @pipeline@ /must/ be supported by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesPipelineBinding ::supportedIndirectCommandsShaderStagesPipelineBinding>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT'
--
-- -   #VUID-VkWriteIndirectExecutionSetPipelineEXT-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'updateIndirectExecutionSetPipelineEXT'
data WriteIndirectExecutionSetPipelineEXT = WriteIndirectExecutionSetPipelineEXT
  { -- | @index@ is the element of the set to update
    index :: Word32
  , -- | @pipeline@ is the pipeline to store in the indirect execution set
    pipeline :: Pipeline
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteIndirectExecutionSetPipelineEXT)
#endif
deriving instance Show WriteIndirectExecutionSetPipelineEXT

instance ToCStruct WriteIndirectExecutionSetPipelineEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteIndirectExecutionSetPipelineEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (index)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_PIPELINE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct WriteIndirectExecutionSetPipelineEXT where
  peekCStruct p = do
    index <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    pure $ WriteIndirectExecutionSetPipelineEXT
             index pipeline

instance Storable WriteIndirectExecutionSetPipelineEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero WriteIndirectExecutionSetPipelineEXT where
  zero = WriteIndirectExecutionSetPipelineEXT
           zero
           zero


-- | VkWriteIndirectExecutionSetShaderEXT - Struct specifying shader object
-- update information for an indirect execution set
--
-- = Description
--
-- Shaders need not be stored in the Indirect Execution Set according to
-- their stage. The only restriction for shader indices within a set is
-- that the value of the index /must/ be less than the maximum number of
-- shaders in the set.
--
-- == Valid Usage
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-index-11031# @index@
--     /must/ be less than
--     'IndirectExecutionSetShaderInfoEXT'::@maxShaderCount@
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-shader-11032# @shader@
--     /must/ have been created with
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDIRECT_BINDABLE_BIT_EXT'
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-pInitialShaders-11033# A
--     shader created with the same
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' /must/
--     have been passed in the
--     'IndirectExecutionSetShaderInfoEXT'::@pInitialShaders@ array
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-index-11034# @index@
--     /must/ not be in use by submitted command buffers
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT'
--
-- -   #VUID-VkWriteIndirectExecutionSetShaderEXT-shader-parameter#
--     @shader@ /must/ be a valid 'Vulkan.Extensions.Handles.ShaderEXT'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Extensions.Handles.ShaderEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'updateIndirectExecutionSetShaderEXT'
data WriteIndirectExecutionSetShaderEXT = WriteIndirectExecutionSetShaderEXT
  { -- | @index@ is the element of the set to update
    index :: Word32
  , -- | @shader@ is the shader to store in the indirect execution set
    shader :: ShaderEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteIndirectExecutionSetShaderEXT)
#endif
deriving instance Show WriteIndirectExecutionSetShaderEXT

instance ToCStruct WriteIndirectExecutionSetShaderEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteIndirectExecutionSetShaderEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (index)
    poke ((p `plusPtr` 24 :: Ptr ShaderEXT)) (shader)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_INDIRECT_EXECUTION_SET_SHADER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ShaderEXT)) (zero)
    f

instance FromCStruct WriteIndirectExecutionSetShaderEXT where
  peekCStruct p = do
    index <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    shader <- peek @ShaderEXT ((p `plusPtr` 24 :: Ptr ShaderEXT))
    pure $ WriteIndirectExecutionSetShaderEXT
             index shader

instance Storable WriteIndirectExecutionSetShaderEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero WriteIndirectExecutionSetShaderEXT where
  zero = WriteIndirectExecutionSetShaderEXT
           zero
           zero


-- | VkIndirectCommandsLayoutCreateInfoEXT - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the flags:
--
-- > void cmdProcessAllSequences(cmd, indirectExecutionSet, indirectCommandsLayout, indirectAddress, sequencesCount)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sUsed = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT) {
-- >       sUsed = incoherent_implementation_dependent_permutation[ sUsed ];
-- >     }
-- >
-- >     cmdProcessSequence( cmd, indirectExecutionSet, indirectCommandsLayout, indirectAddress, sUsed );
-- >   }
-- > }
--
-- When tokens are consumed, an offset is computed based on token offset
-- and stream stride. The resulting offset is required to be aligned. The
-- alignment for a specific token is equal to the scalar alignment of the
-- data type as defined in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-alignment-requirements Alignment Requirements>,
-- or @4@, whichever is lower.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-indirectStride-11090#
--     @indirectStride@ /must/ be less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectCommandsIndirectStride@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11091#
--     @shaderStages@ /must/ only contain stages supported by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStages ::supportedIndirectCommandsShaderStages>
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-tokenCount-11092#
--     @tokenCount@ /must/ less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectCommandsTokenCount@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11093# The
--     number of tokens in the @pTokens@ array with @type@ equal to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' /must/ be less than
--     or equal to @1@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11145# The
--     number of tokens in the @pTokens@ array with @type@ equal to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT' /must/ be less
--     than or equal to @1@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11094# The
--     number of tokens in the @pTokens@ array with @type@ equal to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT' /must/ be less than
--     or equal to @1@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11095# If the
--     action command token in the @pTokens@ array is not an indexed draw
--     token, then @pTokens@ /must/ not contain a member with @type@ set to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11096# If the
--     action command token in the @pTokens@ array is not a non-mesh draw
--     token, then @pTokens@ /must/ not contain a member with @type@ set to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11097# If the
--     @pTokens@ array contains multiple tokens with @type@ equal to
--     'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT', then there /must/
--     be no duplicate
--     'IndirectCommandsVertexBufferTokenEXT'::@vertexBindingUnit@ values
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11099# For all
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT' and
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT' type tokens in
--     @pTokens@, there /must/ be no overlapping ranges between any
--     specified push constant ranges
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11100# The
--     action command token /must/ be the last token in the @pTokens@ array
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11139# If the
--     @pTokens@ array contains a
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT' token, then this
--     token /must/ be the first token in the array
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11101# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT' and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicGeneratedPipelineLayout dynamicGeneratedPipelineLayout>
--     feature is not enabled, then the @pipelineLayout@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11102# For any
--     element of @pTokens@, if @type@ is either
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT' and
--     @pipelineLayout@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', then
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' struct
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11103# For any
--     element of @pTokens@, the @offset@ /must/ be greater than or equal
--     to the @offset@ member of the previous tokens
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11104# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT', or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT', then @shaderStages@ /must/
--     contain graphics stages
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11105# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT', then @shaderStages@
--     /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11106# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT', then
--     @shaderStages@ /must/ contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11107# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT', then
--     the @shaderStages@ /must/ contain
--     'Vulkan.Extensions.VK_NV_mesh_shader.SHADER_STAGE_MESH_BIT_NV'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-11108# For any
--     element of @pTokens@, if @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT', then @shaderStages@
--     /must/ contain ray tracing stages
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11109# If
--     @shaderStages@ contains graphics stages then the state tokens in
--     @pTokens@ /must/ not include
--     'INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11110# If
--     @shaderStages@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--     then the state tokens in @pTokens@ /must/ only include
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT', or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11111# If
--     @shaderStages@ contains ray tracing stages then the state tokens in
--     @pTokens@ /must/ only include
--     'INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT', or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11112# The
--     @shaderStages@ /must/ only contain stages from one of the following:
--
--     -   graphics stages
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
--     -   mesh stages and
--         'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
--     -   ray tracing stages
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-11113# If
--     @shaderStages@ contains
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     then @shaderStages@ /must/ also contain
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-sType-unique# The
--     @sType@ value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'IndirectCommandsLayoutUsageFlagBitsEXT' values
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-parameter#
--     @shaderStages@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-shaderStages-requiredbitmask#
--     @shaderStages@ /must/ not be @0@
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pipelineLayout-parameter#
--     If @pipelineLayout@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-pTokens-parameter#
--     @pTokens@ /must/ be a valid pointer to an array of @tokenCount@
--     valid 'IndirectCommandsLayoutTokenEXT' structures
--
-- -   #VUID-VkIndirectCommandsLayoutCreateInfoEXT-tokenCount-arraylength#
--     @tokenCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsLayoutTokenEXT', 'IndirectCommandsLayoutUsageFlagsEXT',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createIndirectCommandsLayoutEXT'
data IndirectCommandsLayoutCreateInfoEXT (es :: [Type]) = IndirectCommandsLayoutCreateInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'IndirectCommandsLayoutUsageFlagBitsEXT'
    -- specifying usage rules for this layout.
    flags :: IndirectCommandsLayoutUsageFlagsEXT
  , -- | @shaderStages@ is the
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags' that this
    -- layout supports.
    shaderStages :: ShaderStageFlags
  , -- | @indirectStride@ is the distance in bytes between sequences in the
    -- indirect buffer
    indirectStride :: Word32
  , -- | @pipelineLayout@ is the optional 'Vulkan.Core10.Handles.PipelineLayout'
    -- that tokens in this layout use. If the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dynamicGeneratedPipelineLayout dynamicGeneratedPipelineLayout>
    -- feature is enabled, @pipelineLayout@ /can/ be
    -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the layout /must/ be
    -- specified by chaining the
    -- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure off
    -- the @pNext@
    pipelineLayout :: PipelineLayout
  , -- | @pTokens@ is a pointer to an array of 'IndirectCommandsLayoutTokenEXT'
    -- describing each command token in detail.
    tokens :: Vector IndirectCommandsLayoutTokenEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutCreateInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (IndirectCommandsLayoutCreateInfoEXT es)

instance Extensible IndirectCommandsLayoutCreateInfoEXT where
  extensibleTypeName = "IndirectCommandsLayoutCreateInfoEXT"
  setNext IndirectCommandsLayoutCreateInfoEXT{..} next' = IndirectCommandsLayoutCreateInfoEXT{next = next', ..}
  getNext IndirectCommandsLayoutCreateInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends IndirectCommandsLayoutCreateInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineLayoutCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss IndirectCommandsLayoutCreateInfoEXT es
         , PokeChain es ) => ToCStruct (IndirectCommandsLayoutCreateInfoEXT es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (shaderStages)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (indirectStride)
    lift $ poke ((p `plusPtr` 32 :: Ptr PipelineLayout)) (pipelineLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytes @IndirectCommandsLayoutTokenEXT ((Data.Vector.length (tokens)) * 40)
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (40 * (i)) :: Ptr IndirectCommandsLayoutTokenEXT) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsLayoutTokenEXT))) (pPTokens')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance es ~ '[] => Zero (IndirectCommandsLayoutCreateInfoEXT es) where
  zero = IndirectCommandsLayoutCreateInfoEXT
           ()
           zero
           zero
           zero
           zero
           mempty


-- | VkIndirectCommandsLayoutTokenEXT - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-offset-11124# @offset@ /must/
--     be less than or equal to
--     'PhysicalDeviceDeviceGeneratedCommandsPropertiesEXT'::@maxIndirectCommandsTokenOffset@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-offset-11125# @offset@ /must/
--     be aligned to @4@
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-meshShader-11126# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-meshShader meshShader>
--     or
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-taskShader taskShader>
--     are not enabled, @type@ /must/ not be
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT'
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT',
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-rayTracingMaintenance1-11128#
--     If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingMaintenance1 rayTracingMaintenance1>
--     feature is not enabled, @type@ /must/ not be
--     'INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-deviceGeneratedCommandsMultiDrawIndirectCount-11129#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-deviceGeneratedCommandsMultiDrawIndirectCount ::deviceGeneratedCommandsMultiDrawIndirectCount>
--     is not supported, @type@ /must/ not be
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT' or
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-deviceGeneratedCommandsMultiDrawIndirectCount-11130#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-deviceGeneratedCommandsMultiDrawIndirectCount ::deviceGeneratedCommandsMultiDrawIndirectCount>
--     is not supported, @type@ /must/ not be
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-deviceGeneratedCommandsMultiDrawIndirectCount-11131#
--     If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-deviceGeneratedCommandsMultiDrawIndirectCount ::deviceGeneratedCommandsMultiDrawIndirectCount>
--     is not supported, @type@ /must/ not be
--     'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT'
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-type-parameter# @type@ /must/
--     be a valid 'IndirectCommandsTokenTypeEXT' value
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-pPushConstant-parameter# If
--     @type@ is
--     'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT',VK_INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT,
--     the @pPushConstant@ member of @data@ /must/ be a valid pointer to a
--     valid 'IndirectCommandsPushConstantTokenEXT' structure
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-pVertexBuffer-parameter# If
--     @type@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT', the
--     @pVertexBuffer@ member of @data@ /must/ be a valid pointer to a
--     valid 'IndirectCommandsVertexBufferTokenEXT' structure
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-pIndexBuffer-parameter# If
--     @type@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT', the
--     @pIndexBuffer@ member of @data@ /must/ be a valid pointer to a valid
--     'IndirectCommandsIndexBufferTokenEXT' structure
--
-- -   #VUID-VkIndirectCommandsLayoutTokenEXT-pExecutionSet-parameter# If
--     @type@ is 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT', the
--     @pExecutionSet@ member of @data@ /must/ be a valid pointer to a
--     valid 'IndirectCommandsExecutionSetTokenEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsLayoutCreateInfoEXT', 'IndirectCommandsTokenDataEXT',
-- 'IndirectCommandsTokenTypeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectCommandsLayoutTokenEXT = IndirectCommandsLayoutTokenEXT
  { -- | @type@ specifies the 'IndirectCommandsTokenTypeEXT' for @data@.
    type' :: IndirectCommandsTokenTypeEXT
  , -- | @data@ specifies a 'IndirectCommandsTokenDataEXT' containing
    -- token-specific details for command execution. It is ignored if @type@
    -- does not match any member of the 'IndirectCommandsTokenDataEXT' union.
    data' :: IndirectCommandsTokenDataEXT
  , -- | @offset@ is the relative byte offset for the token within one sequence
    -- of the indirect buffer. The data stored at that offset is the command
    -- data for the token, e.g.
    -- 'Vulkan.Core10.OtherTypes.DispatchIndirectCommand'.
    offset :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutTokenEXT)
#endif
deriving instance Show IndirectCommandsLayoutTokenEXT

instance ToCStruct IndirectCommandsLayoutTokenEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeEXT)) (type')
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr IndirectCommandsTokenDataEXT)) (data') . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (offset)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeEXT)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr IndirectCommandsTokenDataEXT)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ f

instance Zero IndirectCommandsLayoutTokenEXT where
  zero = IndirectCommandsLayoutTokenEXT
           zero
           zero
           zero


-- | VkDrawIndirectCountIndirectCommandEXT - Structure specifying input data
-- for a single draw-type command token
--
-- = Description
--
-- The corresponding indirect draw struct data will be read from the buffer
-- address.
--
-- == Valid Usage
--
-- -   #VUID-VkDrawIndirectCountIndirectCommandEXT-None-11122# The buffer’s
--     usage flag from which the address was acquired /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkDrawIndirectCountIndirectCommandEXT-None-11123# Each element
--     of the buffer from which the address was acquired and that is
--     non-sparse /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
data DrawIndirectCountIndirectCommandEXT = DrawIndirectCountIndirectCommandEXT
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used for draw commands.
    bufferAddress :: DeviceAddress
  , -- | @stride@ is the byte size stride for the command arguments
    stride :: Word32
  , -- | @commandCount@ is the number of commands to execute
    commandCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndirectCountIndirectCommandEXT)
#endif
deriving instance Show DrawIndirectCountIndirectCommandEXT

instance ToCStruct DrawIndirectCountIndirectCommandEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndirectCountIndirectCommandEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (commandCount)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndirectCountIndirectCommandEXT where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    stride <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    commandCount <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ DrawIndirectCountIndirectCommandEXT
             bufferAddress stride commandCount

instance Storable DrawIndirectCountIndirectCommandEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndirectCountIndirectCommandEXT where
  zero = DrawIndirectCountIndirectCommandEXT
           zero
           zero
           zero


-- | VkIndirectCommandsVertexBufferTokenEXT - Structure specifying layout
-- token info for a single index buffer command token
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsTokenDataEXT'
data IndirectCommandsVertexBufferTokenEXT = IndirectCommandsVertexBufferTokenEXT
  { -- | @vertexBindingUnit@ is the vertex input binding number to be bound.
    --
    -- #VUID-VkIndirectCommandsVertexBufferTokenEXT-vertexBindingUnit-11134#
    -- @vertexBindingUnit@ /must/ be less than the total number of vertex input
    -- bindings in use by the current graphics state
    vertexBindingUnit :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsVertexBufferTokenEXT)
#endif
deriving instance Show IndirectCommandsVertexBufferTokenEXT

instance ToCStruct IndirectCommandsVertexBufferTokenEXT where
  withCStruct x f = allocaBytes 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsVertexBufferTokenEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (vertexBindingUnit)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectCommandsVertexBufferTokenEXT where
  peekCStruct p = do
    vertexBindingUnit <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ IndirectCommandsVertexBufferTokenEXT
             vertexBindingUnit

instance Storable IndirectCommandsVertexBufferTokenEXT where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsVertexBufferTokenEXT where
  zero = IndirectCommandsVertexBufferTokenEXT
           zero


-- | VkBindVertexBufferIndirectCommandEXT - Structure specifying input data
-- for a single vertex buffer command token
--
-- == Valid Usage
--
-- -   #VUID-VkBindVertexBufferIndirectCommandEXT-None-11120# The buffer’s
--     usage flag from which the address was acquired /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkBindVertexBufferIndirectCommandEXT-None-11121# Each element
--     of the buffer from which the address was acquired and that is
--     non-sparse /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
data BindVertexBufferIndirectCommandEXT = BindVertexBufferIndirectCommandEXT
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as vertex input binding.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @stride@ is the byte size stride for this vertex input binding as in
    -- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription'::@stride@.
    stride :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindVertexBufferIndirectCommandEXT)
#endif
deriving instance Show BindVertexBufferIndirectCommandEXT

instance ToCStruct BindVertexBufferIndirectCommandEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindVertexBufferIndirectCommandEXT{..} f = do
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

instance FromCStruct BindVertexBufferIndirectCommandEXT where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ BindVertexBufferIndirectCommandEXT
             bufferAddress size stride

instance Storable BindVertexBufferIndirectCommandEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindVertexBufferIndirectCommandEXT where
  zero = BindVertexBufferIndirectCommandEXT
           zero
           zero
           zero


-- | VkIndirectCommandsIndexBufferTokenEXT - Structure specifying layout
-- token info for a single index buffer command token
--
-- = Description
--
-- This allows for easy layering of Vulkan atop other APIs. When
-- 'INDIRECT_COMMANDS_INPUT_MODE_DXGI_INDEX_BUFFER_EXT' is specified, the
-- indirect buffer can contain a @D3D12_INDEX_BUFFER_VIEW@ instead of
-- 'BindIndexBufferIndirectCommandEXT' as D3D’s DXGI format value is mapped
-- to the 'Vulkan.Core10.Enums.IndexType.IndexType'. It works as both
-- structs are otherwise binary compatible.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsInputModeFlagBitsEXT', 'IndirectCommandsTokenDataEXT'
data IndirectCommandsIndexBufferTokenEXT = IndirectCommandsIndexBufferTokenEXT
  { -- | @mode@ specifies the mode to use with this token.
    --
    -- #VUID-VkIndirectCommandsIndexBufferTokenEXT-mode-11135# @mode@ /must/ be
    -- non-zero
    --
    -- #VUID-VkIndirectCommandsIndexBufferTokenEXT-mode-11136# @mode@ /must/ be
    -- one of the bits supported in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsInputModes ::supportedIndirectCommandsInputModes>
    --
    -- #VUID-VkIndirectCommandsIndexBufferTokenEXT-mode-parameter# @mode@
    -- /must/ be a valid 'IndirectCommandsInputModeFlagBitsEXT' value
    mode :: IndirectCommandsInputModeFlagBitsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsIndexBufferTokenEXT)
#endif
deriving instance Show IndirectCommandsIndexBufferTokenEXT

instance ToCStruct IndirectCommandsIndexBufferTokenEXT where
  withCStruct x f = allocaBytes 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsIndexBufferTokenEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsInputModeFlagBitsEXT)) (mode)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectCommandsInputModeFlagBitsEXT)) (zero)
    f

instance FromCStruct IndirectCommandsIndexBufferTokenEXT where
  peekCStruct p = do
    mode <- peek @IndirectCommandsInputModeFlagBitsEXT ((p `plusPtr` 0 :: Ptr IndirectCommandsInputModeFlagBitsEXT))
    pure $ IndirectCommandsIndexBufferTokenEXT
             mode

instance Storable IndirectCommandsIndexBufferTokenEXT where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsIndexBufferTokenEXT where
  zero = IndirectCommandsIndexBufferTokenEXT
           zero


-- | VkBindIndexBufferIndirectCommandEXT - Structure specifying input data
-- for a single index buffer command token
--
-- == Valid Usage
--
-- -   #VUID-VkBindIndexBufferIndirectCommandEXT-None-11117# The buffer’s
--     usage flags from which the address was acquired /must/ have the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     bit set
--
-- -   #VUID-VkBindIndexBufferIndirectCommandEXT-bufferAddress-11118# The
--     @bufferAddress@ /must/ be aligned to the
--     'Vulkan.Core10.Enums.IndexType.IndexType' of the @indexType@ used
--
-- -   #VUID-VkBindIndexBufferIndirectCommandEXT-None-11119# Each element
--     of the buffer from which the address was acquired and that is
--     non-sparse /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindIndexBufferIndirectCommandEXT-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
data BindIndexBufferIndirectCommandEXT = BindIndexBufferIndirectCommandEXT
  { -- | @bufferAddress@ specifies a physical address of the
    -- 'Vulkan.Core10.Handles.Buffer' used as index buffer.
    bufferAddress :: DeviceAddress
  , -- | @size@ is the byte size range which is available for this operation from
    -- the provided address.
    size :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
    -- specifying how indices are treated.
    indexType :: IndexType
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindIndexBufferIndirectCommandEXT)
#endif
deriving instance Show BindIndexBufferIndirectCommandEXT

instance ToCStruct BindIndexBufferIndirectCommandEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindIndexBufferIndirectCommandEXT{..} f = do
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

instance FromCStruct BindIndexBufferIndirectCommandEXT where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 12 :: Ptr IndexType))
    pure $ BindIndexBufferIndirectCommandEXT
             bufferAddress size indexType

instance Storable BindIndexBufferIndirectCommandEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindIndexBufferIndirectCommandEXT where
  zero = BindIndexBufferIndirectCommandEXT
           zero
           zero
           zero


-- | VkIndirectCommandsPushConstantTokenEXT - Structure specifying layout
-- token info for a single push constant command token
--
-- = Description
--
-- The @stageFlags@ member of @updateRange@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsPushConstantTokenEXT-updateRange-11132#
--     @updateRange@ /must/ be contained within the push constant info used
--     by 'IndirectCommandsLayoutCreateInfoEXT'
--
-- -   #VUID-VkIndirectCommandsPushConstantTokenEXT-size-11133# If the
--     token type is 'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT', the
--     @size@ member of @updateRange@ /must/ be 4
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsPushConstantTokenEXT-updateRange-parameter#
--     @updateRange@ /must/ be a valid
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsTokenDataEXT',
-- 'Vulkan.Core10.PipelineLayout.PushConstantRange'
data IndirectCommandsPushConstantTokenEXT = IndirectCommandsPushConstantTokenEXT
  { -- | @updateRange@ is the push constant range that will be updated by the
    -- token.
    updateRange :: PushConstantRange }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsPushConstantTokenEXT)
#endif
deriving instance Show IndirectCommandsPushConstantTokenEXT

instance ToCStruct IndirectCommandsPushConstantTokenEXT where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsPushConstantTokenEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PushConstantRange)) (updateRange)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PushConstantRange)) (zero)
    f

instance FromCStruct IndirectCommandsPushConstantTokenEXT where
  peekCStruct p = do
    updateRange <- peekCStruct @PushConstantRange ((p `plusPtr` 0 :: Ptr PushConstantRange))
    pure $ IndirectCommandsPushConstantTokenEXT
             updateRange

instance Storable IndirectCommandsPushConstantTokenEXT where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsPushConstantTokenEXT where
  zero = IndirectCommandsPushConstantTokenEXT
           zero


-- | VkIndirectCommandsExecutionSetTokenEXT - Structure specifying input data
-- for a single execution set command token
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsExecutionSetTokenEXT-shaderStages-11137#
--     Each bit in @shaderStages@ /must/ be supported by
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesPipelineBinding ::supportedIndirectCommandsShaderStagesPipelineBinding>
--     or
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedIndirectCommandsShaderStagesShaderBinding ::supportedIndirectCommandsShaderStagesShaderBinding>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsExecutionSetTokenEXT-type-parameter# @type@
--     /must/ be a valid 'IndirectExecutionSetInfoTypeEXT' value
--
-- -   #VUID-VkIndirectCommandsExecutionSetTokenEXT-shaderStages-parameter#
--     @shaderStages@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-VkIndirectCommandsExecutionSetTokenEXT-shaderStages-requiredbitmask#
--     @shaderStages@ /must/ not be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsTokenDataEXT', 'IndirectExecutionSetInfoTypeEXT',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
data IndirectCommandsExecutionSetTokenEXT = IndirectCommandsExecutionSetTokenEXT
  { -- | @type@ describes the type of indirect execution set in use.
    type' :: IndirectExecutionSetInfoTypeEXT
  , -- | @shaderStages@ specifies the shaders that will be changed by this token.
    shaderStages :: ShaderStageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsExecutionSetTokenEXT)
#endif
deriving instance Show IndirectCommandsExecutionSetTokenEXT

instance ToCStruct IndirectCommandsExecutionSetTokenEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsExecutionSetTokenEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectExecutionSetInfoTypeEXT)) (type')
    poke ((p `plusPtr` 4 :: Ptr ShaderStageFlags)) (shaderStages)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr IndirectExecutionSetInfoTypeEXT)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct IndirectCommandsExecutionSetTokenEXT where
  peekCStruct p = do
    type' <- peek @IndirectExecutionSetInfoTypeEXT ((p `plusPtr` 0 :: Ptr IndirectExecutionSetInfoTypeEXT))
    shaderStages <- peek @ShaderStageFlags ((p `plusPtr` 4 :: Ptr ShaderStageFlags))
    pure $ IndirectCommandsExecutionSetTokenEXT
             type' shaderStages

instance Storable IndirectCommandsExecutionSetTokenEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsExecutionSetTokenEXT where
  zero = IndirectCommandsExecutionSetTokenEXT
           zero
           zero


data IndirectExecutionSetInfoEXT
  = PipelineInfo IndirectExecutionSetPipelineInfoEXT
  | ShaderInfo IndirectExecutionSetShaderInfoEXT
  deriving (Show)

instance ToCStruct IndirectExecutionSetInfoEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr IndirectExecutionSetInfoEXT -> IndirectExecutionSetInfoEXT -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    PipelineInfo v -> do
      pPipelineInfo <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectExecutionSetPipelineInfoEXT) p) pPipelineInfo
    ShaderInfo v -> do
      pShaderInfo <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectExecutionSetShaderInfoEXT) p) pShaderInfo
  pokeZeroCStruct :: Ptr IndirectExecutionSetInfoEXT -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero IndirectExecutionSetInfoEXT where
  zero = PipelineInfo zero


data IndirectCommandsTokenDataEXT
  = PushConstant IndirectCommandsPushConstantTokenEXT
  | VertexBuffer IndirectCommandsVertexBufferTokenEXT
  | IndexBuffer IndirectCommandsIndexBufferTokenEXT
  | ExecutionSet IndirectCommandsExecutionSetTokenEXT
  deriving (Show)

instance ToCStruct IndirectCommandsTokenDataEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr IndirectCommandsTokenDataEXT -> IndirectCommandsTokenDataEXT -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    PushConstant v -> do
      pPushConstant <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectCommandsPushConstantTokenEXT) p) pPushConstant
    VertexBuffer v -> do
      pVertexBuffer <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectCommandsVertexBufferTokenEXT) p) pVertexBuffer
    IndexBuffer v -> do
      pIndexBuffer <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectCommandsIndexBufferTokenEXT) p) pIndexBuffer
    ExecutionSet v -> do
      pExecutionSet <- ContT $ withCStruct (v)
      lift $ poke (castPtr @_ @(Ptr IndirectCommandsExecutionSetTokenEXT) p) pExecutionSet
  pokeZeroCStruct :: Ptr IndirectCommandsTokenDataEXT -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero IndirectCommandsTokenDataEXT where
  zero = PushConstant zero


type IndirectCommandsLayoutUsageFlagsEXT = IndirectCommandsLayoutUsageFlagBitsEXT

-- | VkIndirectCommandsLayoutUsageFlagBitsEXT - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsLayoutUsageFlagsEXT'
newtype IndirectCommandsLayoutUsageFlagBitsEXT = IndirectCommandsLayoutUsageFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT' specifies
-- that the layout is always used with the manual preprocessing step
-- through calling 'cmdPreprocessGeneratedCommandsEXT' and executed by
-- 'cmdExecuteGeneratedCommandsEXT' with @isPreprocessed@ set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT = IndirectCommandsLayoutUsageFlagBitsEXT 0x00000001

-- | 'INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT' specifies
-- that the processing of sequences will happen at an
-- implementation-dependent order, which is not guaranteed to be
-- deterministic using the same input data. This flag is ignored when the
-- @shaderStages@ is
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT' as it
-- is implied that the dispatch sequence is always unordered.
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT = IndirectCommandsLayoutUsageFlagBitsEXT 0x00000002

conNameIndirectCommandsLayoutUsageFlagBitsEXT :: String
conNameIndirectCommandsLayoutUsageFlagBitsEXT = "IndirectCommandsLayoutUsageFlagBitsEXT"

enumPrefixIndirectCommandsLayoutUsageFlagBitsEXT :: String
enumPrefixIndirectCommandsLayoutUsageFlagBitsEXT = "INDIRECT_COMMANDS_LAYOUT_USAGE_"

showTableIndirectCommandsLayoutUsageFlagBitsEXT :: [(IndirectCommandsLayoutUsageFlagBitsEXT, String)]
showTableIndirectCommandsLayoutUsageFlagBitsEXT =
  [
    ( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_EXT
    , "EXPLICIT_PREPROCESS_BIT_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_EXT
    , "UNORDERED_SEQUENCES_BIT_EXT"
    )
  ]

instance Show IndirectCommandsLayoutUsageFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectCommandsLayoutUsageFlagBitsEXT
      showTableIndirectCommandsLayoutUsageFlagBitsEXT
      conNameIndirectCommandsLayoutUsageFlagBitsEXT
      (\(IndirectCommandsLayoutUsageFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read IndirectCommandsLayoutUsageFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixIndirectCommandsLayoutUsageFlagBitsEXT
      showTableIndirectCommandsLayoutUsageFlagBitsEXT
      conNameIndirectCommandsLayoutUsageFlagBitsEXT
      IndirectCommandsLayoutUsageFlagBitsEXT

-- | VkIndirectExecutionSetInfoTypeEXT - Enum specifying allowed usage of an
-- indirect execution set
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsExecutionSetTokenEXT',
-- 'IndirectExecutionSetCreateInfoEXT'
newtype IndirectExecutionSetInfoTypeEXT = IndirectExecutionSetInfoTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT' specifies that the
-- indirect execution set contains 'Vulkan.Core10.Handles.Pipeline'
-- objects.
pattern INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT = IndirectExecutionSetInfoTypeEXT 0

-- | 'INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT' specifies that the
-- indirect execution set contains 'Vulkan.Extensions.Handles.ShaderEXT'
-- objects.
pattern INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT = IndirectExecutionSetInfoTypeEXT 1

{-# COMPLETE
  INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT
  , INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT ::
    IndirectExecutionSetInfoTypeEXT
  #-}

conNameIndirectExecutionSetInfoTypeEXT :: String
conNameIndirectExecutionSetInfoTypeEXT = "IndirectExecutionSetInfoTypeEXT"

enumPrefixIndirectExecutionSetInfoTypeEXT :: String
enumPrefixIndirectExecutionSetInfoTypeEXT = "INDIRECT_EXECUTION_SET_INFO_TYPE_"

showTableIndirectExecutionSetInfoTypeEXT :: [(IndirectExecutionSetInfoTypeEXT, String)]
showTableIndirectExecutionSetInfoTypeEXT =
  [
    ( INDIRECT_EXECUTION_SET_INFO_TYPE_PIPELINES_EXT
    , "PIPELINES_EXT"
    )
  ,
    ( INDIRECT_EXECUTION_SET_INFO_TYPE_SHADER_OBJECTS_EXT
    , "SHADER_OBJECTS_EXT"
    )
  ]

instance Show IndirectExecutionSetInfoTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectExecutionSetInfoTypeEXT
      showTableIndirectExecutionSetInfoTypeEXT
      conNameIndirectExecutionSetInfoTypeEXT
      (\(IndirectExecutionSetInfoTypeEXT x) -> x)
      (showsPrec 11)

instance Read IndirectExecutionSetInfoTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixIndirectExecutionSetInfoTypeEXT
      showTableIndirectExecutionSetInfoTypeEXT
      conNameIndirectExecutionSetInfoTypeEXT
      IndirectExecutionSetInfoTypeEXT

type IndirectCommandsInputModeFlagsEXT = IndirectCommandsInputModeFlagBitsEXT

-- | VkIndirectCommandsInputModeFlagBitsEXT - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsIndexBufferTokenEXT',
-- 'IndirectCommandsInputModeFlagsEXT'
newtype IndirectCommandsInputModeFlagBitsEXT = IndirectCommandsInputModeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'INDIRECT_COMMANDS_INPUT_MODE_VULKAN_INDEX_BUFFER_EXT' specifies that
-- the indirect buffer contains 'BindIndexBufferIndirectCommandEXT'.
pattern INDIRECT_COMMANDS_INPUT_MODE_VULKAN_INDEX_BUFFER_EXT = IndirectCommandsInputModeFlagBitsEXT 0x00000001

-- | 'INDIRECT_COMMANDS_INPUT_MODE_DXGI_INDEX_BUFFER_EXT' specifies that the
-- indirect buffer contains @D3D12_INDEX_BUFFER_VIEW@.
pattern INDIRECT_COMMANDS_INPUT_MODE_DXGI_INDEX_BUFFER_EXT = IndirectCommandsInputModeFlagBitsEXT 0x00000002

conNameIndirectCommandsInputModeFlagBitsEXT :: String
conNameIndirectCommandsInputModeFlagBitsEXT = "IndirectCommandsInputModeFlagBitsEXT"

enumPrefixIndirectCommandsInputModeFlagBitsEXT :: String
enumPrefixIndirectCommandsInputModeFlagBitsEXT = "INDIRECT_COMMANDS_INPUT_MODE_"

showTableIndirectCommandsInputModeFlagBitsEXT :: [(IndirectCommandsInputModeFlagBitsEXT, String)]
showTableIndirectCommandsInputModeFlagBitsEXT =
  [
    ( INDIRECT_COMMANDS_INPUT_MODE_VULKAN_INDEX_BUFFER_EXT
    , "VULKAN_INDEX_BUFFER_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_INPUT_MODE_DXGI_INDEX_BUFFER_EXT
    , "DXGI_INDEX_BUFFER_EXT"
    )
  ]

instance Show IndirectCommandsInputModeFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectCommandsInputModeFlagBitsEXT
      showTableIndirectCommandsInputModeFlagBitsEXT
      conNameIndirectCommandsInputModeFlagBitsEXT
      (\(IndirectCommandsInputModeFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read IndirectCommandsInputModeFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixIndirectCommandsInputModeFlagBitsEXT
      showTableIndirectCommandsInputModeFlagBitsEXT
      conNameIndirectCommandsInputModeFlagBitsEXT
      IndirectCommandsInputModeFlagBitsEXT

-- | VkIndirectCommandsTokenTypeEXT - Enum specifying token commands
--
-- = Description
--
-- \'
--
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Common Tokens__                                           | __Command Data__                                                                 |
-- +=============================================================+==================================================================================+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT'            | @u32[]@ array of indices into the indirect execution set                         |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT'            | @u32[]@ raw data                                                                 |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT'           | @u32@ placeholder data (not accessed by shader)                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Compute Tokens__                                          |                                                                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT'                 | 'Vulkan.Core10.OtherTypes.DispatchIndirectCommand'                               |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Ray Tracing Tokens__                                      |                                                                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT'              | 'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.TraceRaysIndirectCommand2KHR' |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Graphics State Tokens__                                   |                                                                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT'             | 'BindIndexBufferIndirectCommandEXT'                                              |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT'            | 'BindVertexBufferIndirectCommandEXT'                                             |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Graphics Draw Tokens__                                    |                                                                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT'             | 'Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'                            |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT'                     | 'Vulkan.Core10.OtherTypes.DrawIndirectCommand'                                   |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT'          | 'Vulkan.Extensions.VK_EXT_mesh_shader.DrawMeshTasksIndirectCommandEXT'           |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT'       | 'Vulkan.Extensions.VK_NV_mesh_shader.DrawMeshTasksIndirectCommandNV'             |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | __Graphics Draw Count Tokens__                              |                                                                                  |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT'       | 'DrawIndirectCountIndirectCommandEXT' with VkDrawIndexedIndirectCommand          |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT'               | 'DrawIndirectCountIndirectCommandEXT' with VkDrawIndirectCommand                 |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT'    | 'DrawIndirectCountIndirectCommandEXT' with VkDrawMeshTasksIndirectCommandEXT     |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
-- | 'INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT' | 'DrawIndirectCountIndirectCommandEXT' with VkDrawMeshTasksIndirectCommandNV      |
-- +-------------------------------------------------------------+----------------------------------------------------------------------------------+
--
-- Supported Indirect Command Tokens
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>,
-- 'IndirectCommandsLayoutTokenEXT'
newtype IndirectCommandsTokenTypeEXT = IndirectCommandsTokenTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT = IndirectCommandsTokenTypeEXT 0

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT = IndirectCommandsTokenTypeEXT 1

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT = IndirectCommandsTokenTypeEXT 2

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT = IndirectCommandsTokenTypeEXT 3

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT = IndirectCommandsTokenTypeEXT 4

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT = IndirectCommandsTokenTypeEXT 5

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT = IndirectCommandsTokenTypeEXT 6

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT = IndirectCommandsTokenTypeEXT 7

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT = IndirectCommandsTokenTypeEXT 8

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT = IndirectCommandsTokenTypeEXT 9

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT = IndirectCommandsTokenTypeEXT 1000386004

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT = IndirectCommandsTokenTypeEXT 1000328001

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT = IndirectCommandsTokenTypeEXT 1000328000

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT = IndirectCommandsTokenTypeEXT 1000202003

-- No documentation found for Nested "VkIndirectCommandsTokenTypeEXT" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT = IndirectCommandsTokenTypeEXT 1000202002

{-# COMPLETE
  INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT
  , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT ::
    IndirectCommandsTokenTypeEXT
  #-}

conNameIndirectCommandsTokenTypeEXT :: String
conNameIndirectCommandsTokenTypeEXT = "IndirectCommandsTokenTypeEXT"

enumPrefixIndirectCommandsTokenTypeEXT :: String
enumPrefixIndirectCommandsTokenTypeEXT = "INDIRECT_COMMANDS_TOKEN_TYPE_"

showTableIndirectCommandsTokenTypeEXT :: [(IndirectCommandsTokenTypeEXT, String)]
showTableIndirectCommandsTokenTypeEXT =
  [
    ( INDIRECT_COMMANDS_TOKEN_TYPE_EXECUTION_SET_EXT
    , "EXECUTION_SET_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_EXT
    , "PUSH_CONSTANT_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_SEQUENCE_INDEX_EXT
    , "SEQUENCE_INDEX_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_EXT
    , "INDEX_BUFFER_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_EXT
    , "VERTEX_BUFFER_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_EXT
    , "DRAW_INDEXED_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_EXT
    , "DRAW_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_COUNT_EXT
    , "DRAW_INDEXED_COUNT_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_COUNT_EXT
    , "DRAW_COUNT_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_EXT
    , "DISPATCH_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_TRACE_RAYS2_EXT
    , "TRACE_RAYS2_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_EXT
    , "DRAW_MESH_TASKS_COUNT_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_EXT
    , "DRAW_MESH_TASKS_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_COUNT_NV_EXT
    , "DRAW_MESH_TASKS_COUNT_NV_EXT"
    )
  ,
    ( INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV_EXT
    , "DRAW_MESH_TASKS_NV_EXT"
    )
  ]

instance Show IndirectCommandsTokenTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixIndirectCommandsTokenTypeEXT
      showTableIndirectCommandsTokenTypeEXT
      conNameIndirectCommandsTokenTypeEXT
      (\(IndirectCommandsTokenTypeEXT x) -> x)
      (showsPrec 11)

instance Read IndirectCommandsTokenTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixIndirectCommandsTokenTypeEXT
      showTableIndirectCommandsTokenTypeEXT
      conNameIndirectCommandsTokenTypeEXT
      IndirectCommandsTokenTypeEXT

type EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 1


type EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_EXT_device_generated_commands"

-- No documentation found for TopLevel "VK_EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_EXT_device_generated_commands"

