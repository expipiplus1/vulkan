{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_generated_commands_compute - device extension
--
-- == VK_NV_device_generated_commands_compute
--
-- [__Name String__]
--     @VK_NV_device_generated_commands_compute@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     429
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_generated_commands_compute] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_device_generated_commands_compute extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-21
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Mike Blumenkrantz, VALVE
--
-- == Description
--
-- This extension allows the device to generate commands for binding
-- compute pipelines, setting push constants and launching compute
-- dispatches.
--
-- == New Commands
--
-- -   'cmdUpdatePipelineIndirectBufferNV'
--
-- -   'getPipelineIndirectDeviceAddressNV'
--
-- -   'getPipelineIndirectMemoryRequirementsNV'
--
-- == New Structures
--
-- -   'BindPipelineIndirectCommandNV'
--
-- -   'ComputePipelineIndirectBufferInfoNV'
--
-- -   'PipelineIndirectDeviceAddressInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME'
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsTokenTypeNV':
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV'
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV'
--
-- == Version History
--
-- -   Revision 2, 2023-07-21 (Vikram Kushwaha)
--
--     -   Rename vkCmdUpdatePipelineIndirectBuffer to
--         vkCmdUpdatePipelineIndirectBufferNV
--
-- -   Revision 1, 2023-06-09 (Vikram Kushwaha)
--
--     -   First Revision
--
-- == See Also
--
-- 'BindPipelineIndirectCommandNV', 'ComputePipelineIndirectBufferInfoNV',
-- 'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV',
-- 'PipelineIndirectDeviceAddressInfoNV',
-- 'cmdUpdatePipelineIndirectBufferNV',
-- 'getPipelineIndirectDeviceAddressNV',
-- 'getPipelineIndirectMemoryRequirementsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_generated_commands_compute  ( cmdUpdatePipelineIndirectBufferNV
                                                                  , getPipelineIndirectMemoryRequirementsNV
                                                                  , getPipelineIndirectDeviceAddressNV
                                                                  , ComputePipelineIndirectBufferInfoNV(..)
                                                                  , PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV(..)
                                                                  , PipelineIndirectDeviceAddressInfoNV(..)
                                                                  , BindPipelineIndirectCommandNV(..)
                                                                  , NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION
                                                                  , pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION
                                                                  , NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME
                                                                  , pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME
                                                                  , IndirectCommandsTokenTypeNV(..)
                                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Pipeline (ComputePipelineCreateInfo)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdUpdatePipelineIndirectBufferNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineIndirectDeviceAddressNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineIndirectMemoryRequirementsNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV))
import Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsTokenTypeNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdUpdatePipelineIndirectBufferNV
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()

-- | vkCmdUpdatePipelineIndirectBufferNV - Update the indirect compute
-- pipeline’s metadata
--
-- = Description
--
-- 'cmdUpdatePipelineIndirectBufferNV' is only allowed outside of a render
-- pass. This command is treated as a “transfer” operation for the purposes
-- of synchronization barriers. The writes to the address /must/ be
-- synchronized using stages
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT' and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
-- and with access masks
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_MEMORY_WRITE_BIT' and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'
-- respectively before using the results in preprocessing.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-pipelineBindPoint-09018#
--     @pipelineBindPoint@ /must/ be
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-pipeline-09019# @pipeline@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--     flag set
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-pipeline-09020# @pipeline@
--     /must/ have been created with 'ComputePipelineIndirectBufferInfoNV'
--     structure specifying a valid address where its metadata will be
--     saved
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-deviceGeneratedComputePipelines-09021#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdUpdatePipelineIndirectBufferNV-commonparent# Both of
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
cmdUpdatePipelineIndirectBufferNV :: forall io
                                   . (MonadIO io)
                                  => -- | @commandBuffer@ is the command buffer into which the command will be
                                     -- recorded.
                                     CommandBuffer
                                  -> -- | @pipelineBindPoint@ is a
                                     -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
                                     -- specifying the type of pipeline whose metadata will be saved.
                                     PipelineBindPoint
                                  -> -- | @pipeline@ is the pipeline whose metadata will be saved.
                                     Pipeline
                                  -> io ()
cmdUpdatePipelineIndirectBufferNV commandBuffer
                                    pipelineBindPoint
                                    pipeline = liftIO $ do
  let vkCmdUpdatePipelineIndirectBufferNVPtr = pVkCmdUpdatePipelineIndirectBufferNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdUpdatePipelineIndirectBufferNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdUpdatePipelineIndirectBufferNV is null" Nothing Nothing
  let vkCmdUpdatePipelineIndirectBufferNV' = mkVkCmdUpdatePipelineIndirectBufferNV vkCmdUpdatePipelineIndirectBufferNVPtr
  traceAroundEvent "vkCmdUpdatePipelineIndirectBufferNV" (vkCmdUpdatePipelineIndirectBufferNV'
                                                            (commandBufferHandle (commandBuffer))
                                                            (pipelineBindPoint)
                                                            (pipeline))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineIndirectMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- | vkGetPipelineIndirectMemoryRequirementsNV - Get the memory requirements
-- for the compute indirect pipeline
--
-- = Description
--
-- If @pCreateInfo@::@pNext@ chain includes a pointer to a
-- 'ComputePipelineIndirectBufferInfoNV' structure, then the contents of
-- that structure are ignored.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineIndirectMemoryRequirementsNV-deviceGeneratedComputePipelines-09082#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetPipelineIndirectMemoryRequirementsNV-pCreateInfo-09083#
--     @pCreateInfo@::@flags@ /must/ include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelineIndirectMemoryRequirementsNV-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelineIndirectMemoryRequirementsNV-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo' structure
--
-- -   #VUID-vkGetPipelineIndirectMemoryRequirementsNV-pMemoryRequirements-parameter#
--     @pMemoryRequirements@ /must/ be a valid pointer to a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
getPipelineIndirectMemoryRequirementsNV :: forall a b io
                                         . ( Extendss ComputePipelineCreateInfo a
                                           , PokeChain a
                                           , Extendss MemoryRequirements2 b
                                           , PokeChain b
                                           , PeekChain b
                                           , MonadIO io )
                                        => -- | @device@ is the logical device that owns the buffer.
                                           Device
                                        -> -- | @pCreateInfo@ is a 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'
                                           -- structure specifying the creation parameters of the compute pipeline
                                           -- whose memory requirements are being queried.
                                           (ComputePipelineCreateInfo a)
                                        -> io (MemoryRequirements2 b)
getPipelineIndirectMemoryRequirementsNV device
                                          createInfo = liftIO . evalContT $ do
  let vkGetPipelineIndirectMemoryRequirementsNVPtr = pVkGetPipelineIndirectMemoryRequirementsNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineIndirectMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineIndirectMemoryRequirementsNV is null" Nothing Nothing
  let vkGetPipelineIndirectMemoryRequirementsNV' = mkVkGetPipelineIndirectMemoryRequirementsNV vkGetPipelineIndirectMemoryRequirementsNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ traceAroundEvent "vkGetPipelineIndirectMemoryRequirementsNV" (vkGetPipelineIndirectMemoryRequirementsNV'
                                                                         (deviceHandle (device))
                                                                         (forgetExtensions pCreateInfo)
                                                                         (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineIndirectDeviceAddressNV
  :: FunPtr (Ptr Device_T -> Ptr PipelineIndirectDeviceAddressInfoNV -> IO DeviceAddress) -> Ptr Device_T -> Ptr PipelineIndirectDeviceAddressInfoNV -> IO DeviceAddress

-- | vkGetPipelineIndirectDeviceAddressNV - Get pipeline’s 64-bit device
-- address
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelineIndirectDeviceAddressNV-deviceGeneratedComputePipelines-09078#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.Handles.Device', 'PipelineIndirectDeviceAddressInfoNV'
getPipelineIndirectDeviceAddressNV :: forall io
                                    . (MonadIO io)
                                   => -- | #VUID-vkGetPipelineIndirectDeviceAddressNV-device-parameter# @device@
                                      -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                      Device
                                   -> -- | #VUID-vkGetPipelineIndirectDeviceAddressNV-pInfo-parameter# @pInfo@
                                      -- /must/ be a valid pointer to a valid
                                      -- 'PipelineIndirectDeviceAddressInfoNV' structure
                                      PipelineIndirectDeviceAddressInfoNV
                                   -> io (DeviceAddress)
getPipelineIndirectDeviceAddressNV device info = liftIO . evalContT $ do
  let vkGetPipelineIndirectDeviceAddressNVPtr = pVkGetPipelineIndirectDeviceAddressNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelineIndirectDeviceAddressNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineIndirectDeviceAddressNV is null" Nothing Nothing
  let vkGetPipelineIndirectDeviceAddressNV' = mkVkGetPipelineIndirectDeviceAddressNV vkGetPipelineIndirectDeviceAddressNVPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetPipelineIndirectDeviceAddressNV" (vkGetPipelineIndirectDeviceAddressNV'
                                                                         (deviceHandle (device))
                                                                         pInfo)
  pure $ (r)


-- | VkComputePipelineIndirectBufferInfoNV - Structure describing the device
-- address where pipeline’s metadata will be saved
--
-- = Members
--
-- If @pipelineDeviceAddressCaptureReplay@ is zero, no specific address is
-- requested. If @pipelineDeviceAddressCaptureReplay@ is not zero, then it
-- /must/ be an address retrieved from an identically created pipeline on
-- the same implementation. The pipeline metadata /must/ also be placed on
-- an identically created buffer and at the same offset using the
-- 'cmdUpdatePipelineIndirectBufferNV' command.
--
-- == Valid Usage
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-deviceGeneratedComputePipelines-09009#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-flags-09010# The
--     pipeline creation flags in
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'::@flags@ /must/
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-deviceAddress-09011#
--     @deviceAddress@ /must/ be aligned to the
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'::@alignment@,
--     as returned by 'getPipelineIndirectMemoryRequirementsNV'
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-deviceAddress-09012#
--     @deviceAddress@ /must/ have been allocated from a buffer that was
--     created with usage
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     and
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-size-09013# @size@
--     /must/ be greater than or equal to the
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'::@size@,
--     as returned by 'getPipelineIndirectMemoryRequirementsNV'
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-pipelineDeviceAddressCaptureReplay-09014#
--     If @pipelineDeviceAddressCaptureReplay@ is non-zero then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputeCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-pipelineDeviceAddressCaptureReplay-09015#
--     If @pipelineDeviceAddressCaptureReplay@ is non-zero then that
--     address /must/ have been allocated with flag
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--     set
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-pipelineDeviceAddressCaptureReplay-09016#
--     If @pipelineDeviceAddressCaptureReplay@ is non-zero, the @pipeline@
--     /must/ have been recreated for replay
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-pipelineDeviceAddressCaptureReplay-09017#
--     @pipelineDeviceAddressCaptureReplay@ /must/ satisfy the @alignment@
--     and @size@ requirements similar to @deviceAddress@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV'
--
-- -   #VUID-VkComputePipelineIndirectBufferInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ComputePipelineIndirectBufferInfoNV = ComputePipelineIndirectBufferInfoNV
  { -- No documentation found for Nested "VkComputePipelineIndirectBufferInfoNV" "deviceAddress"
    deviceAddress :: DeviceAddress
  , -- No documentation found for Nested "VkComputePipelineIndirectBufferInfoNV" "size"
    size :: DeviceSize
  , -- No documentation found for Nested "VkComputePipelineIndirectBufferInfoNV" "pipelineDeviceAddressCaptureReplay"
    pipelineDeviceAddressCaptureReplay :: DeviceAddress
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComputePipelineIndirectBufferInfoNV)
#endif
deriving instance Show ComputePipelineIndirectBufferInfoNV

instance ToCStruct ComputePipelineIndirectBufferInfoNV where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ComputePipelineIndirectBufferInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (deviceAddress)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 32 :: Ptr DeviceAddress)) (pipelineDeviceAddressCaptureReplay)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceAddress)) (zero)
    f

instance FromCStruct ComputePipelineIndirectBufferInfoNV where
  peekCStruct p = do
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pipelineDeviceAddressCaptureReplay <- peek @DeviceAddress ((p `plusPtr` 32 :: Ptr DeviceAddress))
    pure $ ComputePipelineIndirectBufferInfoNV
             deviceAddress size pipelineDeviceAddressCaptureReplay

instance Storable ComputePipelineIndirectBufferInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ComputePipelineIndirectBufferInfoNV where
  zero = ComputePipelineIndirectBufferInfoNV
           zero
           zero
           zero


-- | VkPhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV - Structure
-- describing the device-generated compute features that can be supported
-- by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV = PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
  { -- | #features-deviceGeneratedCompute# @deviceGeneratedCompute@ indicates
    -- whether the implementation supports functionality to generate dispatch
    -- commands and push constants for the compute pipeline on the device. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedCompute :: Bool
  , -- | #features-deviceGeneratedComputePipelines#
    -- @deviceGeneratedComputePipelines@ indicates whether the implementation
    -- supports functionality to generate commands to bind compute pipelines on
    -- the device. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedComputePipelines :: Bool
  , -- | #features-deviceGeneratedComputeCaptureReplay#
    -- @deviceGeneratedComputeCaptureReplay@ indicates whether the
    -- implementation supports functionality to capture compute pipeline
    -- address and reuse later for replay in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-generated-commands Device-Generated Commands>.
    deviceGeneratedComputeCaptureReplay :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCompute))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedComputePipelines))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedComputeCaptureReplay))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV where
  peekCStruct p = do
    deviceGeneratedCompute <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    deviceGeneratedComputePipelines <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    deviceGeneratedComputeCaptureReplay <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
             (bool32ToBool deviceGeneratedCompute)
             (bool32ToBool deviceGeneratedComputePipelines)
             (bool32ToBool deviceGeneratedComputeCaptureReplay)

instance Storable PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
           zero
           zero
           zero


-- | VkPipelineIndirectDeviceAddressInfoNV - Structure specifying the
-- pipeline to query an address for
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineIndirectDeviceAddressInfoNV-pipelineBindPoint-09079#
--     The provided @pipelineBindPoint@ /must/ be of type
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--
-- -   #VUID-VkPipelineIndirectDeviceAddressInfoNV-pipeline-09080#
--     @pipeline@ /must/ have been created with flag
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--     set
--
-- -   #VUID-VkPipelineIndirectDeviceAddressInfoNV-pipeline-09081#
--     @pipeline@ /must/ have been created with a
--     'ComputePipelineIndirectBufferInfoNV' structure specifying a valid
--     address where its metadata will be saved
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineIndirectDeviceAddressNV'
data PipelineIndirectDeviceAddressInfoNV = PipelineIndirectDeviceAddressInfoNV
  { -- | #VUID-VkPipelineIndirectDeviceAddressInfoNV-pipelineBindPoint-parameter#
    -- @pipelineBindPoint@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
    pipelineBindPoint :: PipelineBindPoint
  , -- | #VUID-VkPipelineIndirectDeviceAddressInfoNV-pipeline-parameter#
    -- @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
    pipeline :: Pipeline
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineIndirectDeviceAddressInfoNV)
#endif
deriving instance Show PipelineIndirectDeviceAddressInfoNV

instance ToCStruct PipelineIndirectDeviceAddressInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineIndirectDeviceAddressInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct PipelineIndirectDeviceAddressInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    pure $ PipelineIndirectDeviceAddressInfoNV
             pipelineBindPoint pipeline

instance Storable PipelineIndirectDeviceAddressInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineIndirectDeviceAddressInfoNV where
  zero = PipelineIndirectDeviceAddressInfoNV
           zero
           zero


-- | VkBindPipelineIndirectCommandNV - Structure specifying input data for
-- the compute pipeline dispatch token
--
-- == Valid Usage
--
-- -   #VUID-VkBindPipelineIndirectCommandNV-deviceGeneratedComputePipelines-09091#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature /must/ be enabled
--
-- -   #VUID-VkBindPipelineIndirectCommandNV-None-09092# The referenced
--     pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkBindPipelineIndirectCommandNV-None-09093# The referenced
--     pipeline /must/ have been updated with
--     'cmdUpdatePipelineIndirectBufferNV'
--
-- -   #VUID-VkBindPipelineIndirectCommandNV-None-09094# The referenced
--     pipeline’s address /must/ have been queried with
--     'getPipelineIndirectDeviceAddressNV'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute VK_NV_device_generated_commands_compute>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
data BindPipelineIndirectCommandNV = BindPipelineIndirectCommandNV
  { -- | @pipelineAddress@ specifies the pipeline address of the compute pipeline
    -- that will be used in device generated rendering.
    pipelineAddress :: DeviceAddress }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindPipelineIndirectCommandNV)
#endif
deriving instance Show BindPipelineIndirectCommandNV

instance ToCStruct BindPipelineIndirectCommandNV where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindPipelineIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (pipelineAddress)
    f
  cStructSize = 8
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    f

instance FromCStruct BindPipelineIndirectCommandNV where
  peekCStruct p = do
    pipelineAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    pure $ BindPipelineIndirectCommandNV
             pipelineAddress

instance Storable BindPipelineIndirectCommandNV where
  sizeOf ~_ = 8
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindPipelineIndirectCommandNV where
  zero = BindPipelineIndirectCommandNV
           zero


type NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION"
pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION = 2


type NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME = "VK_NV_device_generated_commands_compute"

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME"
pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME = "VK_NV_device_generated_commands_compute"

