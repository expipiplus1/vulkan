{-# language CPP #-}
-- | = Name
--
-- VK_KHR_copy_memory_indirect - device extension
--
-- = VK_KHR_copy_memory_indirect
--
-- [__Name String__]
--     @VK_KHR_copy_memory_indirect@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     550
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
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_copy_memory_indirect] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_KHR_copy_memory_indirect extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_copy_memory_indirect.adoc VK_KHR_copy_memory_indirect>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-25
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Stuart Smith, AMD
--
--     -   Faith Ekstrand, Collabora
--
--     -   Caterina Shablia, Collabora
--
--     -   Spencer Fricke, LunarG
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Alyssa Rosenzweig, Valve
--
-- == Description
--
-- This extension adds support for performing copies between memory and
-- image regions using indirect parameters that are read by the device from
-- a buffer during execution. This functionality may be useful for
-- performing copies where the copy parameters are not known during the
-- command buffer creation time.
--
-- == New Commands
--
-- -   'cmdCopyMemoryIndirectKHR'
--
-- -   'cmdCopyMemoryToImageIndirectKHR'
--
-- == New Structures
--
-- -   'CopyMemoryIndirectCommandKHR'
--
-- -   'CopyMemoryIndirectInfoKHR'
--
-- -   'CopyMemoryToImageIndirectCommandKHR'
--
-- -   'CopyMemoryToImageIndirectInfoKHR'
--
-- -   'StridedDeviceAddressRangeKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCopyMemoryIndirectFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCopyMemoryIndirectPropertiesKHR'
--
-- == New Enums
--
-- -   'AddressCopyFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'AddressCopyFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME'
--
-- -   'KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_COPY_IMAGE_INDIRECT_DST_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-01-25 (Daniel Koch, Vikram Kushwaha)
--
--     -   Initial external release
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_copy_memory_indirect Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_copy_memory_indirect  ( cmdCopyMemoryIndirectKHR
                                                      , cmdCopyMemoryToImageIndirectKHR
                                                      , StridedDeviceAddressRangeKHR(..)
                                                      , CopyMemoryIndirectCommandKHR(..)
                                                      , CopyMemoryIndirectInfoKHR(..)
                                                      , CopyMemoryToImageIndirectCommandKHR(..)
                                                      , CopyMemoryToImageIndirectInfoKHR(..)
                                                      , PhysicalDeviceCopyMemoryIndirectFeaturesKHR(..)
                                                      , PhysicalDeviceCopyMemoryIndirectPropertiesKHR(..)
                                                      , AddressCopyFlagsKHR
                                                      , AddressCopyFlagBitsKHR( ADDRESS_COPY_DEVICE_LOCAL_BIT_KHR
                                                                              , ADDRESS_COPY_SPARSE_BIT_KHR
                                                                              , ADDRESS_COPY_PROTECTED_BIT_KHR
                                                                              , ..
                                                                              )
                                                      , KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                      , pattern KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                      , KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME
                                                      , pattern KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME
                                                      ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToImageIndirectKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryIndirectInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMemoryIndirectInfoKHR -> IO ()

-- | vkCmdCopyMemoryIndirectKHR - Copy data between memory regions
--
-- = Description
--
-- Each region specified in the memory referenced by
-- @pCopyMemoryIndirectInfo->copyAddressRange@ is copied from the source
-- region to the specified destination region. The results are undefined if
-- any of the source and destination regions overlap in memory.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-indirectMemoryCopy-10935# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectMemoryCopy indirectMemoryCopy>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-commandBuffer-10936# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support at least one of the queue types
--     specified in
--     'PhysicalDeviceCopyMemoryIndirectPropertiesKHR'::@supportedQueues@
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-commandBuffer-10937#
--     @commandBuffer@ must not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-pCopyMemoryIndirectInfo-parameter#
--     @pCopyMemoryIndirectInfo@ /must/ be a valid pointer to a valid
--     'CopyMemoryIndirectInfoKHR' structure
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyMemoryIndirectKHR-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdCopyMemoryIndirectKHR is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyMemoryIndirectInfoKHR'
cmdCopyMemoryIndirectKHR :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @pCopyMemoryIndirectInfo@ is a pointer to a 'CopyMemoryIndirectInfoKHR'
                            -- structure containing the copy parameters, including the number of copies
                            -- to execute and a strided array of 'CopyMemoryIndirectCommandKHR'
                            -- structures.
                            CopyMemoryIndirectInfoKHR
                         -> io ()
cmdCopyMemoryIndirectKHR commandBuffer
                           copyMemoryIndirectInfo = liftIO . evalContT $ do
  let vkCmdCopyMemoryIndirectKHRPtr = pVkCmdCopyMemoryIndirectKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMemoryIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryIndirectKHR is null" Nothing Nothing
  let vkCmdCopyMemoryIndirectKHR' = mkVkCmdCopyMemoryIndirectKHR vkCmdCopyMemoryIndirectKHRPtr
  pCopyMemoryIndirectInfo <- ContT $ withCStruct (copyMemoryIndirectInfo)
  lift $ traceAroundEvent "vkCmdCopyMemoryIndirectKHR" (vkCmdCopyMemoryIndirectKHR'
                                                          (commandBufferHandle (commandBuffer))
                                                          pCopyMemoryIndirectInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToImageIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryToImageIndirectInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMemoryToImageIndirectInfoKHR -> IO ()

-- | vkCmdCopyMemoryToImageIndirectKHR - Copy data from a memory region to an
-- image object
--
-- = Description
--
-- Each region specified in the memory referenced by
-- @pCopyMemoryToImageIndirectInfo->copyAddressRange@ is copied from the
-- source region to an image region in the destination image. If the
-- destination image is of type
-- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the starting slice and
-- number of slices to copy are specified in
-- @pImageSubresources->baseArrayLayer@ and
-- @pImageSubresources->layerCount@ respectively as @imageOffset@ and
-- @imageExtent@ from 'CopyMemoryToImageIndirectCommandKHR' are only
-- available at device execution time. The results are undefined if any of
-- the source and destination regions overlap in memory.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-indirectMemoryToImageCopy-10947#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectMemoryToImageCopy indirectMemoryToImageCopy>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-commandBuffer-10948# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support at least one of the queue types
--     specified in
--     'PhysicalDeviceCopyMemoryIndirectPropertiesKHR'::@supportedQueues@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-commandBuffer-10949#
--     @commandBuffer@ must not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-pCopyMemoryToImageIndirectInfo-parameter#
--     @pCopyMemoryToImageIndirectInfo@ /must/ be a valid pointer to a
--     valid 'CopyMemoryToImageIndirectInfoKHR' structure
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectKHR-videocoding# This command
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
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdCopyMemoryToImageIndirectKHR is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'CopyMemoryToImageIndirectInfoKHR'
cmdCopyMemoryToImageIndirectKHR :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pCopyMemoryToImageIndirectInfo@ is a pointer to a
                                   -- 'CopyMemoryToImageIndirectInfoKHR' structure which contains the copy
                                   -- parameters, including the number of copies to execute and a strided
                                   -- array of 'CopyMemoryToImageIndirectCommandKHR' structures.
                                   CopyMemoryToImageIndirectInfoKHR
                                -> io ()
cmdCopyMemoryToImageIndirectKHR commandBuffer
                                  copyMemoryToImageIndirectInfo = liftIO . evalContT $ do
  let vkCmdCopyMemoryToImageIndirectKHRPtr = pVkCmdCopyMemoryToImageIndirectKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMemoryToImageIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryToImageIndirectKHR is null" Nothing Nothing
  let vkCmdCopyMemoryToImageIndirectKHR' = mkVkCmdCopyMemoryToImageIndirectKHR vkCmdCopyMemoryToImageIndirectKHRPtr
  pCopyMemoryToImageIndirectInfo <- ContT $ withCStruct (copyMemoryToImageIndirectInfo)
  lift $ traceAroundEvent "vkCmdCopyMemoryToImageIndirectKHR" (vkCmdCopyMemoryToImageIndirectKHR'
                                                                 (commandBufferHandle (commandBuffer))
                                                                 pCopyMemoryToImageIndirectInfo)
  pure $ ()


-- | VkStridedDeviceAddressRangeKHR - Structure specifying a device address
-- range with a stride
--
-- == Valid Usage
--
-- -   #VUID-VkStridedDeviceAddressRangeKHR-size-11411# If @size@ is not 0,
--     @address@ /must/ not be 0
--
-- -   #VUID-VkStridedDeviceAddressRangeKHR-address-11365# The sum of
--     @address@ and @size@ /must/ be less than or equal to the sum of an
--     address retrieved from a 'Vulkan.Core10.Handles.Buffer' and the
--     value of 'Vulkan.Core10.Buffer.BufferCreateInfo'::@size@ used to
--     create that 'Vulkan.Core10.Handles.Buffer'
--
-- -   #VUID-VkStridedDeviceAddressRangeKHR-stride-10957# @stride@ /must/
--     be less than or equal to @size@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkStridedDeviceAddressRangeKHR-address-parameter# If @address@
--     is not @0@, @address@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'CopyMemoryIndirectInfoKHR', 'CopyMemoryToImageIndirectInfoKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data StridedDeviceAddressRangeKHR = StridedDeviceAddressRangeKHR
  { -- | @address@ is a 'Vulkan.Core10.FundamentalTypes.DeviceAddress' specifying
    -- the start of the range.
    address :: DeviceAddress
  , -- | @size@ is a 'Vulkan.Core10.FundamentalTypes.DeviceSize' specifying the
    -- size of the range.
    size :: DeviceSize
  , -- | @stride@ is a 'Vulkan.Core10.FundamentalTypes.DeviceSize' specifying the
    -- stride of elements over the range.
    stride :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StridedDeviceAddressRangeKHR)
#endif
deriving instance Show StridedDeviceAddressRangeKHR

instance ToCStruct StridedDeviceAddressRangeKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StridedDeviceAddressRangeKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (address)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (stride)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct StridedDeviceAddressRangeKHR where
  peekCStruct p = do
    address <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    stride <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ StridedDeviceAddressRangeKHR
             address size stride

instance Storable StridedDeviceAddressRangeKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StridedDeviceAddressRangeKHR where
  zero = StridedDeviceAddressRangeKHR
           zero
           zero
           zero


-- | VkCopyMemoryIndirectCommandKHR - Structure specifying indirect memory
-- region copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-srcAddress-10958# The
--     @srcAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-dstAddress-10959# The
--     @dstAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-size-10960# The @size@ /must/
--     be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-srcAddress-10961# The memory in
--     range [@srcAddress@, @srcAddress@ + @size@ - 1] /must/ be within the
--     bounds of the memory allocation backing @srcAddress@
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-dstAddress-10962# The memory in
--     range [@dstAddress@, @dstAddress@ + @size@ - 1] /must/ be within the
--     bounds of the memory allocation backing @dstAddress@
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-srcAddress-12211# The range of
--     memory defined by @srcAddress@ and @size@ /must/ be a device address
--     range allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-dstAddress-12212# The range of
--     memory defined by @dstAddress@ and @size@ /must/ be a device address
--     range allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-srcAddress-parameter#
--     @srcAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-VkCopyMemoryIndirectCommandKHR-dstAddress-parameter#
--     @dstAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data CopyMemoryIndirectCommandKHR = CopyMemoryIndirectCommandKHR
  { -- | @srcAddress@ is the starting address of the source device memory to copy
    -- from.
    srcAddress :: DeviceAddress
  , -- | @dstAddress@ is the starting address of the destination device memory to
    -- copy to.
    dstAddress :: DeviceAddress
  , -- | @size@ is the size of the copy in bytes.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryIndirectCommandKHR)
#endif
deriving instance Show CopyMemoryIndirectCommandKHR

instance ToCStruct CopyMemoryIndirectCommandKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryIndirectCommandKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (srcAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (dstAddress)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct CopyMemoryIndirectCommandKHR where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    dstAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ CopyMemoryIndirectCommandKHR
             srcAddress dstAddress size

instance Storable CopyMemoryIndirectCommandKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMemoryIndirectCommandKHR where
  zero = CopyMemoryIndirectCommandKHR
           zero
           zero
           zero


-- | VkCopyMemoryIndirectInfoKHR - Parameters describing indirect copy
-- parameters
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-srcCopyFlags-10938# If
--     @srcCopyFlags@ contains 'ADDRESS_COPY_SPARSE_BIT_KHR', the source
--     memory regions accessed /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory bound to memory>
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-dstCopyFlags-10939# If
--     @dstCopyFlags@ contains 'ADDRESS_COPY_SPARSE_BIT_KHR', the
--     destination memory regions accessed /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory bound to memory>
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-srcCopyFlags-10940# @srcCopyFlags@
--     /must/ not contain 'ADDRESS_COPY_PROTECTED_BIT_KHR'
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-dstCopyFlags-10941# @dstCopyFlags@
--     /must/ not contain 'ADDRESS_COPY_PROTECTED_BIT_KHR'
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-copyAddressRange-10942#
--     @copyAddressRange.address@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-copyAddressRange-10943#
--     @copyAddressRange.stride@ /must/ be a multiple of @4@ and /must/ be
--     greater than or equal to sizeof('CopyMemoryIndirectCommandKHR')
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-copyCount-10944# @copyCount@
--     /must/ be less than or equal to @copyAddressRange.size@ \/
--     @copyAddressRange.stride@
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-copyAddressRange-10945# Any of the
--     source or destination memory regions specified in @copyAddressRange@
--     /must/ not overlap with any of the specified destination memory
--     regions
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-copyAddressRange-12210#
--     @copyAddressRange@ /must/ be a device address range allocated to the
--     application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR'
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-srcCopyFlags-parameter#
--     @srcCopyFlags@ /must/ be a valid combination of
--     'AddressCopyFlagBitsKHR' values
--
-- -   #VUID-VkCopyMemoryIndirectInfoKHR-dstCopyFlags-parameter#
--     @dstCopyFlags@ /must/ be a valid combination of
--     'AddressCopyFlagBitsKHR' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'AddressCopyFlagsKHR', 'StridedDeviceAddressRangeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMemoryIndirectKHR'
data CopyMemoryIndirectInfoKHR = CopyMemoryIndirectInfoKHR
  { -- | @srcCopyFlags@ is a 'AddressCopyFlagsKHR' value defining the copy flags
    -- for the source address range.
    srcCopyFlags :: AddressCopyFlagsKHR
  , -- | @dstCopyFlags@ is a 'AddressCopyFlagsKHR' value defining the copy flags
    -- for the destination address range.
    dstCopyFlags :: AddressCopyFlagsKHR
  , -- | @copyCount@ is the number of copies to execute, and /can/ be zero.
    copyCount :: Word32
  , -- | @copyAddressRange@ is a memory region specifying the copy parameters. It
    -- is laid out as an array of 'CopyMemoryIndirectCommandKHR' structures.
    copyAddressRange :: StridedDeviceAddressRangeKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryIndirectInfoKHR)
#endif
deriving instance Show CopyMemoryIndirectInfoKHR

instance ToCStruct CopyMemoryIndirectInfoKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryIndirectInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AddressCopyFlagsKHR)) (srcCopyFlags)
    poke ((p `plusPtr` 20 :: Ptr AddressCopyFlagsKHR)) (dstCopyFlags)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (copyCount)
    poke ((p `plusPtr` 32 :: Ptr StridedDeviceAddressRangeKHR)) (copyAddressRange)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr StridedDeviceAddressRangeKHR)) (zero)
    f

instance FromCStruct CopyMemoryIndirectInfoKHR where
  peekCStruct p = do
    srcCopyFlags <- peek @AddressCopyFlagsKHR ((p `plusPtr` 16 :: Ptr AddressCopyFlagsKHR))
    dstCopyFlags <- peek @AddressCopyFlagsKHR ((p `plusPtr` 20 :: Ptr AddressCopyFlagsKHR))
    copyCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    copyAddressRange <- peekCStruct @StridedDeviceAddressRangeKHR ((p `plusPtr` 32 :: Ptr StridedDeviceAddressRangeKHR))
    pure $ CopyMemoryIndirectInfoKHR
             srcCopyFlags dstCopyFlags copyCount copyAddressRange

instance Storable CopyMemoryIndirectInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMemoryIndirectInfoKHR where
  zero = CopyMemoryIndirectInfoKHR
           zero
           zero
           zero
           zero


-- | VkCopyMemoryToImageIndirectCommandKHR - Structure specifying indirect
-- memory region to image copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-srcAddress-10963# The
--     @srcAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-dstImage-12282# If
--     @dstImage@ does not have either a depth\/stencil format or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     @srcAddress@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block size>
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-dstImage-12283# If
--     @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     @srcAddress@ /must/ be a multiple of the element size of the
--     compatible format for the format and the @aspectMask@ of the
--     @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-bufferRowLength-10964#
--     @bufferRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-bufferImageHeight-10965#
--     @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageOffset-10966#
--     @imageOffset@ /must/ specify a valid offset in the destination image
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageExtent-10967#
--     @imageExtent@ /must/ specify a valid region in the destination image
--     and /can/ be @0@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-srcAddress-10968# The
--     memory region starting at @srcAddress@ and described by
--     @bufferRowLength@ and @bufferImageHeight@ /must/ not exceed the
--     bounds of the memory allocation backing memory at @srcAddress@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageOffset-10969# The
--     @imageOffset@ and @imageExtent@ members of each region /must/
--     respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageOffset-10970# For
--     each destination region, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified subresource
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageOffset-10971# For
--     each destination region, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified subresource
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageSubresource-12284#
--     The members of @imageSubresource@ /must/ be identical to the members
--     of the 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure specified in the corresponding index of the
--     @pCopyMemoryToImageIndirectInfo->pImageSubresources@ array of
--     'cmdCopyMemoryToImageIndirectKHR' during command recording
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-dstImage-12285# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D',
--     @imageOffset.y@ /must/ be @0@ and @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-dstImage-12286# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D'
--     or 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', @imageOffset.z@
--     /must/ be @0@ and @imageExtent.depth@ /must/ be @1@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-srcAddress-12214#
--     @srcAddress@ /must/ be a device address allocated to the application
--     from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-srcAddress-parameter#
--     @srcAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandKHR-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D'
data CopyMemoryToImageIndirectCommandKHR = CopyMemoryToImageIndirectCommandKHR
  { -- | @srcAddress@ is the starting address of the source device memory to copy
    -- from.
    srcAddress :: DeviceAddress
  , -- | @bufferRowLength@ and @bufferImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in buffer memory, and
    -- control the addressing calculations. If either of these values is zero,
    -- that aspect of the buffer memory is considered to be tightly packed
    -- according to the @imageExtent@.
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkCopyMemoryToImageIndirectCommandKHR" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structure
    -- used to specify the specific image subresources of the image used for
    -- the destination image data, which /must/ match the value specified in
    -- corresponding index of the
    -- @pCopyMemoryToImageIndirectInfo->pImageSubresources@ array of
    -- 'cmdCopyMemoryToImageIndirectKHR' during command recording.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the destination image in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToImageIndirectCommandKHR)
#endif
deriving instance Show CopyMemoryToImageIndirectCommandKHR

instance ToCStruct CopyMemoryToImageIndirectCommandKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToImageIndirectCommandKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (srcAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (bufferRowLength)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (bufferImageHeight)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct CopyMemoryToImageIndirectCommandKHR where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 44 :: Ptr Extent3D))
    pure $ CopyMemoryToImageIndirectCommandKHR
             srcAddress
             bufferRowLength
             bufferImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable CopyMemoryToImageIndirectCommandKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMemoryToImageIndirectCommandKHR where
  zero = CopyMemoryToImageIndirectCommandKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCopyMemoryToImageIndirectInfoKHR - Parameters describing indirect
-- image copy parameters
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-srcCopyFlags-10950# If
--     @srcCopyFlags@ contains 'ADDRESS_COPY_SPARSE_BIT_KHR', the source
--     memory regions accessed /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory bound to memory>
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyCount-10951#
--     @copyCount@ /must/ be less than or equal to @copyAddressRange.size@
--     \/ @copyAddressRange.stride@
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyAddressRange-10952#
--     @copyAddressRange.address@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyAddressRange-10953#
--     @copyAddressRange.stride@ /must/ be a multiple of @4@ and /must/ be
--     greater than or equal to
--     sizeof('CopyMemoryToImageIndirectCommandKHR')
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-10955# The format
--     features of @dstImage@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_COPY_IMAGE_INDIRECT_DST_BIT_KHR'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyAddressRange-12213#
--     @copyAddressRange@ /must/ be a device address range allocated to the
--     application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-07661# @dstImage@
--     /must/ not be a protected image
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-aspectMask-07662# The
--     @aspectMask@ member for every subresource in @pImageSubresources@
--     /must/ only have a single bit set
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-aspectMask-12287# The
--     @aspectMask@ member for every subresource in @pImageSubresources@
--     /must/ specify an aspect present in @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-07664# @dstImage@
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag set
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-07665# If
--     @dstImage@ is non-sparse then the image or each specified /disjoint/
--     plane /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-07973# @dstImage@
--     /must/ have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImageLayout-07667#
--     @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImageLayout-07669#
--     @dstImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-mipLevel-07670# The
--     specified @mipLevel@ of each region in @pImageSubresources@ /must/
--     be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-12288# If
--     @dstImage@ is not of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', and the specified
--     @layerCount@ of each region in @pImageSubresources@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the specified
--     @baseArrayLayer@ + @layerCount@ of each region in
--     @pImageSubresources@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-12289# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     and the specified @layerCount@ of each region in
--     @pImageSubresources@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', for each
--     destination region, (@imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@) /must/ be less than or equal to the
--     depth of the specified subresource
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-12290# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     and the specified @layerCount@ of each region in
--     @pImageSubresources@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', for each
--     destination region, if (@imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@) does not equal the depth of the
--     specified subresource, @imageSubresource.layerCount@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-12291# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     for each destination region, @imageSubresource.baseArrayLayer@
--     /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-12292# If
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     for each destination region, @imageSubresource.baseArrayLayer@
--     /must/ be less than or equal to the depth of the specified
--     subresource
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-07673# @dstImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-commandBuffer-07674# If the
--     queue family used to create the 'Vulkan.Core10.Handles.CommandPool'
--     which @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     region, the @aspectMask@ member of @pImageSubresources@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-10974# The format
--     features of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyAddressRange-10975# Any
--     of the source or destination memory regions specified in
--     @copyAddressRange@ /must/ not overlap with any of the specified
--     destination memory regions at the time this command is executed on
--     device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR'
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-srcCopyFlags-parameter#
--     @srcCopyFlags@ /must/ be a valid combination of
--     'AddressCopyFlagBitsKHR' values
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImage-parameter#
--     @dstImage@ /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-pImageSubresources-parameter#
--     @pImageSubresources@ /must/ be a valid pointer to an array of
--     @copyCount@ valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structures
--
-- -   #VUID-VkCopyMemoryToImageIndirectInfoKHR-copyCount-arraylength#
--     @copyCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'AddressCopyFlagsKHR', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'StridedDeviceAddressRangeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMemoryToImageIndirectKHR'
data CopyMemoryToImageIndirectInfoKHR = CopyMemoryToImageIndirectInfoKHR
  { -- | @srcCopyFlags@ is a 'AddressCopyFlagsKHR' value defining the copy flags
    -- for the source address range.
    srcCopyFlags :: AddressCopyFlagsKHR
  , -- | @copyAddressRange@ is a memory region specifying the copy parameters. It
    -- is laid out as an array of 'CopyMemoryToImageIndirectCommandKHR'
    -- structures.
    copyAddressRange :: StridedDeviceAddressRangeKHR
  , -- | @dstImage@ is the destination image.
    dstImage :: Image
  , -- | @dstImageLayout@ is the layout of the destination image subresources for
    -- the copy.
    dstImageLayout :: ImageLayout
  , -- | @pImageSubresources@ is a pointer to an array of @copyCount@
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures,
    -- specifying the image subresources of the destination image data for the
    -- copy operation.
    imageSubresources :: Vector ImageSubresourceLayers
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToImageIndirectInfoKHR)
#endif
deriving instance Show CopyMemoryToImageIndirectInfoKHR

instance ToCStruct CopyMemoryToImageIndirectInfoKHR where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToImageIndirectInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AddressCopyFlagsKHR)) (srcCopyFlags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageSubresources)) :: Word32))
    lift $ poke ((p `plusPtr` 24 :: Ptr StridedDeviceAddressRangeKHR)) (copyAddressRange)
    lift $ poke ((p `plusPtr` 48 :: Ptr Image)) (dstImage)
    lift $ poke ((p `plusPtr` 56 :: Ptr ImageLayout)) (dstImageLayout)
    pPImageSubresources' <- ContT $ allocaBytes @ImageSubresourceLayers ((Data.Vector.length (imageSubresources)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPImageSubresources' `plusPtr` (16 * (i)) :: Ptr ImageSubresourceLayers) (e)) (imageSubresources)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr ImageSubresourceLayers))) (pPImageSubresources')
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr StridedDeviceAddressRangeKHR)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Image)) (zero)
    poke ((p `plusPtr` 56 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct CopyMemoryToImageIndirectInfoKHR where
  peekCStruct p = do
    srcCopyFlags <- peek @AddressCopyFlagsKHR ((p `plusPtr` 16 :: Ptr AddressCopyFlagsKHR))
    copyAddressRange <- peekCStruct @StridedDeviceAddressRangeKHR ((p `plusPtr` 24 :: Ptr StridedDeviceAddressRangeKHR))
    dstImage <- peek @Image ((p `plusPtr` 48 :: Ptr Image))
    dstImageLayout <- peek @ImageLayout ((p `plusPtr` 56 :: Ptr ImageLayout))
    copyCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pImageSubresources <- peek @(Ptr ImageSubresourceLayers) ((p `plusPtr` 64 :: Ptr (Ptr ImageSubresourceLayers)))
    pImageSubresources' <- generateM (fromIntegral copyCount) (\i -> peekCStruct @ImageSubresourceLayers ((pImageSubresources `advancePtrBytes` (16 * (i)) :: Ptr ImageSubresourceLayers)))
    pure $ CopyMemoryToImageIndirectInfoKHR
             srcCopyFlags
             copyAddressRange
             dstImage
             dstImageLayout
             pImageSubresources'

instance Zero CopyMemoryToImageIndirectInfoKHR where
  zero = CopyMemoryToImageIndirectInfoKHR
           zero
           zero
           zero
           zero
           mempty


-- | VkPhysicalDeviceCopyMemoryIndirectFeaturesKHR - Structure describing
-- indirect copy features supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCopyMemoryIndirectFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceCopyMemoryIndirectFeaturesKHR', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCopyMemoryIndirectFeaturesKHR = PhysicalDeviceCopyMemoryIndirectFeaturesKHR
  { -- | #features-indirectMemoryCopy# @indirectMemoryCopy@ indicates whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#indirect-copies indirect memory to memory copies>
    -- are supported.
    indirectMemoryCopy :: Bool
  , -- | #features-indirectMemoryToImageCopy# @indirectMemoryToImageCopy@
    -- indicates whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#indirect-copies indirect memory to image copies>
    -- are supported.
    indirectMemoryToImageCopy :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCopyMemoryIndirectFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceCopyMemoryIndirectFeaturesKHR

instance ToCStruct PhysicalDeviceCopyMemoryIndirectFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCopyMemoryIndirectFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (indirectMemoryCopy))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (indirectMemoryToImageCopy))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCopyMemoryIndirectFeaturesKHR where
  peekCStruct p = do
    indirectMemoryCopy <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    indirectMemoryToImageCopy <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCopyMemoryIndirectFeaturesKHR
             (bool32ToBool indirectMemoryCopy)
             (bool32ToBool indirectMemoryToImageCopy)

instance Storable PhysicalDeviceCopyMemoryIndirectFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCopyMemoryIndirectFeaturesKHR where
  zero = PhysicalDeviceCopyMemoryIndirectFeaturesKHR
           zero
           zero


-- | VkPhysicalDeviceCopyMemoryIndirectPropertiesKHR - Structure describing
-- supported queues for indirect copy
--
-- = Description
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectMemoryCopy indirectMemoryCopy>
-- or
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectMemoryToImageCopy indirectMemoryToImageCopy>
-- feature is supported, @supportedQueues@ /must/ return at least one
-- supported queue type.
--
-- If the 'PhysicalDeviceCopyMemoryIndirectPropertiesKHR' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCopyMemoryIndirectPropertiesKHR = PhysicalDeviceCopyMemoryIndirectPropertiesKHR
  { -- | @supportedQueues@ is a bitmask of
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits' indicating the types
    -- of queues on which
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#indirect-copies indirect copy commands>
    -- are supported. If a queue family supports any of the bits set in
    -- @supportedQueues@, then it /must/ support at least one
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#indirect-copies indirect copy command>.
    supportedQueues :: QueueFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCopyMemoryIndirectPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceCopyMemoryIndirectPropertiesKHR

instance ToCStruct PhysicalDeviceCopyMemoryIndirectPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCopyMemoryIndirectPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (supportedQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceCopyMemoryIndirectPropertiesKHR where
  peekCStruct p = do
    supportedQueues <- peek @QueueFlags ((p `plusPtr` 16 :: Ptr QueueFlags))
    pure $ PhysicalDeviceCopyMemoryIndirectPropertiesKHR
             supportedQueues

instance Storable PhysicalDeviceCopyMemoryIndirectPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCopyMemoryIndirectPropertiesKHR where
  zero = PhysicalDeviceCopyMemoryIndirectPropertiesKHR
           zero


type AddressCopyFlagsKHR = AddressCopyFlagBitsKHR

-- | VkAddressCopyFlagBitsKHR - Bitmask specifying address copy parameters
--
-- = Description
--
-- -   'ADDRESS_COPY_DEVICE_LOCAL_BIT_KHR' specifies that the address range
--     is expected to be resident in device local memory. Specifying this
--     value is optional, but /may/ lead to improved performance if set
--     accurately.
--
-- -   'ADDRESS_COPY_PROTECTED_BIT_KHR' specifies that the address range is
--     allocated from protected memory.
--
-- -   'ADDRESS_COPY_SPARSE_BIT_KHR' specifies that the address range may
--     not be fully bound to physical memory when accessed.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>,
-- 'AddressCopyFlagsKHR'
newtype AddressCopyFlagBitsKHR = AddressCopyFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkAddressCopyFlagBitsKHR" "VK_ADDRESS_COPY_DEVICE_LOCAL_BIT_KHR"
pattern ADDRESS_COPY_DEVICE_LOCAL_BIT_KHR = AddressCopyFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkAddressCopyFlagBitsKHR" "VK_ADDRESS_COPY_SPARSE_BIT_KHR"
pattern ADDRESS_COPY_SPARSE_BIT_KHR = AddressCopyFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkAddressCopyFlagBitsKHR" "VK_ADDRESS_COPY_PROTECTED_BIT_KHR"
pattern ADDRESS_COPY_PROTECTED_BIT_KHR = AddressCopyFlagBitsKHR 0x00000004

conNameAddressCopyFlagBitsKHR :: String
conNameAddressCopyFlagBitsKHR = "AddressCopyFlagBitsKHR"

enumPrefixAddressCopyFlagBitsKHR :: String
enumPrefixAddressCopyFlagBitsKHR = "ADDRESS_COPY_"

showTableAddressCopyFlagBitsKHR :: [(AddressCopyFlagBitsKHR, String)]
showTableAddressCopyFlagBitsKHR =
  [
    ( ADDRESS_COPY_DEVICE_LOCAL_BIT_KHR
    , "DEVICE_LOCAL_BIT_KHR"
    )
  ,
    ( ADDRESS_COPY_SPARSE_BIT_KHR
    , "SPARSE_BIT_KHR"
    )
  ,
    ( ADDRESS_COPY_PROTECTED_BIT_KHR
    , "PROTECTED_BIT_KHR"
    )
  ]

instance Show AddressCopyFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixAddressCopyFlagBitsKHR
      showTableAddressCopyFlagBitsKHR
      conNameAddressCopyFlagBitsKHR
      (\(AddressCopyFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read AddressCopyFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixAddressCopyFlagBitsKHR
      showTableAddressCopyFlagBitsKHR
      conNameAddressCopyFlagBitsKHR
      AddressCopyFlagBitsKHR

type KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION"
pattern KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1


type KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_KHR_copy_memory_indirect"

-- No documentation found for TopLevel "VK_KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME"
pattern KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_KHR_copy_memory_indirect"

