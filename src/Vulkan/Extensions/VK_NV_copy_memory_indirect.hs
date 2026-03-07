{-# language CPP #-}
-- | = Name
--
-- VK_NV_copy_memory_indirect - device extension
--
-- = VK_NV_copy_memory_indirect
--
-- [__Name String__]
--     @VK_NV_copy_memory_indirect@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     427
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_memory_indirect VK_KHR_copy_memory_indirect>
--         extension
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_copy_memory_indirect] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_copy_memory_indirect extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-14
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for performing copies between memory and
-- image regions using indirect parameters that are read by the device from
-- a buffer during execution. This functionality /may/ be useful for
-- performing copies where the copy parameters are not known during the
-- command buffer creation time.
--
-- == New Commands
--
-- -   'cmdCopyMemoryIndirectNV'
--
-- -   'cmdCopyMemoryToImageIndirectNV'
--
-- == New Structures
--
-- -   'CopyMemoryIndirectCommandNV'
--
-- -   'CopyMemoryToImageIndirectCommandNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCopyMemoryIndirectFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCopyMemoryIndirectPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME'
--
-- -   'NV_COPY_MEMORY_INDIRECT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-10-14 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_copy_memory_indirect Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_copy_memory_indirect  ( cmdCopyMemoryIndirectNV
                                                     , cmdCopyMemoryToImageIndirectNV
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV
                                                     , PhysicalDeviceCopyMemoryIndirectFeaturesNV(..)
                                                     , CopyMemoryIndirectCommandNV
                                                     , CopyMemoryToImageIndirectCommandNV
                                                     , PhysicalDeviceCopyMemoryIndirectPropertiesNV
                                                     , NV_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                     , pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                     , NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME
                                                     , pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME
                                                     , CopyMemoryIndirectCommandKHR(..)
                                                     , CopyMemoryToImageIndirectCommandKHR(..)
                                                     , PhysicalDeviceCopyMemoryIndirectPropertiesKHR(..)
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
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (CopyMemoryIndirectCommandKHR)
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (CopyMemoryToImageIndirectCommandKHR)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryIndirectNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToImageIndirectNV))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (PhysicalDeviceCopyMemoryIndirectPropertiesKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR))
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (CopyMemoryIndirectCommandKHR(..))
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (CopyMemoryToImageIndirectCommandKHR(..))
import Vulkan.Extensions.VK_KHR_copy_memory_indirect (PhysicalDeviceCopyMemoryIndirectPropertiesKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryIndirectNV
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> Word32 -> Word32 -> IO ()

-- | vkCmdCopyMemoryIndirectNV - Copy data between memory regions
--
-- = Description
--
-- Each region read from @copyBufferAddress@ is copied from the source
-- region to the specified destination region. The results are undefined if
-- any of the source and destination regions overlap in memory.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-None-07653# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectCopy indirectCopy>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-copyBufferAddress-07654#
--     @copyBufferAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-stride-07655# @stride@ /must/ be a
--     multiple of @4@ and /must/ be greater than or equal to
--     sizeof('CopyMemoryIndirectCommandNV')
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-07656# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support at least one of the queue types
--     specified in
--     'Vulkan.Extensions.VK_KHR_copy_memory_indirect.PhysicalDeviceCopyMemoryIndirectPropertiesKHR'::@supportedQueues@
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-copyBufferAddress-10946# Any of the
--     source or destination memory regions specified in
--     @copyBufferAddress@ /must/ not overlap with any of the specified
--     destination memory regions
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-copyBufferAddress-parameter#
--     @copyBufferAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-videocoding# This command /must/
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
-- vkCmdCopyMemoryIndirectNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdCopyMemoryIndirectNV :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @copyBufferAddress@ is the memory address specifying the copy
                           -- parameters. It is laid out as an array of 'CopyMemoryIndirectCommandNV'
                           -- structures.
                           ("copyBufferAddress" ::: DeviceAddress)
                        -> -- | @copyCount@ is the number of copies to execute, and /can/ be zero.
                           ("copyCount" ::: Word32)
                        -> -- | @stride@ is the stride in bytes between successive sets of copy
                           -- parameters.
                           ("stride" ::: Word32)
                        -> io ()
cmdCopyMemoryIndirectNV commandBuffer
                          copyBufferAddress
                          copyCount
                          stride = liftIO $ do
  let vkCmdCopyMemoryIndirectNVPtr = pVkCmdCopyMemoryIndirectNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdCopyMemoryIndirectNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryIndirectNV is null" Nothing Nothing
  let vkCmdCopyMemoryIndirectNV' = mkVkCmdCopyMemoryIndirectNV vkCmdCopyMemoryIndirectNVPtr
  traceAroundEvent "vkCmdCopyMemoryIndirectNV" (vkCmdCopyMemoryIndirectNV'
                                                  (commandBufferHandle (commandBuffer))
                                                  (copyBufferAddress)
                                                  (copyCount)
                                                  (stride))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToImageIndirectNV
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> Word32 -> Word32 -> Image -> ImageLayout -> Ptr ImageSubresourceLayers -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> Word32 -> Word32 -> Image -> ImageLayout -> Ptr ImageSubresourceLayers -> IO ()

-- | vkCmdCopyMemoryToImageIndirectNV - Copy data from a memory region to an
-- image object
--
-- = Description
--
-- Each region in @copyBufferAddress@ is copied from the source memory
-- region to an image region in the destination image. If the destination
-- image is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
-- starting slice and number of slices to copy are specified in
-- @pImageSubresources->baseArrayLayer@ and
-- @pImageSubresources->layerCount@ respectively.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-None-07660# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-indirectCopy indirectCopy>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-offset-07676#
--     @copyBufferAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-stride-07677# @stride@ /must/
--     be a multiple of @4@ and /must/ be greater than or equal to
--     sizeof('CopyMemoryToImageIndirectCommandNV')
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-10956# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support at least one of the queue types
--     specified in
--     'Vulkan.Extensions.VK_KHR_copy_memory_indirect.PhysicalDeviceCopyMemoryIndirectPropertiesKHR'::@supportedQueues@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07661# @dstImage@
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-aspectMask-07662# The
--     @aspectMask@ member for every subresource in @pImageSubresources@
--     /must/ only have a single bit set
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-aspectMask-12287# The
--     @aspectMask@ member for every subresource in @pImageSubresources@
--     /must/ specify an aspect present in @dstImage@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07664# @dstImage@
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07665# If @dstImage@
--     is non-sparse then the image or each specified /disjoint/ plane
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07973# @dstImage@
--     /must/ have a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImageLayout-07667#
--     @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImageLayout-07669#
--     @dstImageLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-mipLevel-07670# The specified
--     @mipLevel@ of each region in @pImageSubresources@ /must/ be less
--     than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-12288# If @dstImage@
--     is not of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', and
--     the specified @layerCount@ of each region in @pImageSubresources@ is
--     not 'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', the
--     specified @baseArrayLayer@ + @layerCount@ of each region in
--     @pImageSubresources@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-12289# If @dstImage@
--     is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', and the
--     specified @layerCount@ of each region in @pImageSubresources@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', for each
--     destination region, (@imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@) /must/ be less than or equal to the
--     depth of the specified subresource
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-12290# If @dstImage@
--     is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', and the
--     specified @layerCount@ of each region in @pImageSubresources@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', for each
--     destination region, if (@imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@) does not equal the depth of the
--     specified subresource, @imageSubresource.layerCount@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-12291# If @dstImage@
--     is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each
--     destination region, @imageSubresource.baseArrayLayer@ /must/ be a
--     multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-12292# If @dstImage@
--     is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each
--     destination region, @imageSubresource.baseArrayLayer@ /must/ be less
--     than or equal to the depth of the specified subresource
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07673# @dstImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-07674# If the
--     queue family used to create the 'Vulkan.Core10.Handles.CommandPool'
--     which @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     region, the @aspectMask@ member of @pImageSubresources@ /must/ not
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-10974# The format
--     features of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-copyBufferAddress-10975# Any
--     of the source or destination memory regions specified in
--     @copyBufferAddress@ /must/ not overlap with any of the specified
--     destination memory regions at the time this command is executed on
--     device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-copyBufferAddress-parameter#
--     @copyBufferAddress@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-parameter#
--     @dstImage@ /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-pImageSubresources-parameter#
--     @pImageSubresources@ /must/ be a valid pointer to an array of
--     @copyCount@ valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structures
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-suspended# This command
--     /must/ not be called between suspended render pass instances
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-copyCount-arraylength#
--     @copyCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commonparent# Both of
--     @commandBuffer@, and @dstImage@ /must/ have been created, allocated,
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdCopyMemoryToImageIndirectNV is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
cmdCopyMemoryToImageIndirectNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @copyBufferAddress@ is the address specifying the copy parameters which
                                  -- are laid out in memory as an array of
                                  -- 'CopyMemoryToImageIndirectCommandNV' structures.
                                  ("copyBufferAddress" ::: DeviceAddress)
                               -> -- | @stride@ is the byte stride between successive sets of copy parameters.
                                  ("stride" ::: Word32)
                               -> -- | @dstImage@ is the destination image.
                                  ("dstImage" ::: Image)
                               -> -- | @dstImageLayout@ is the layout of the destination image subresources for
                                  -- the copy.
                                  ("dstImageLayout" ::: ImageLayout)
                               -> -- | @pImageSubresources@ is a pointer to an array of @copyCount@
                                  -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' structures,
                                  -- specifying the image subresources of the destination image data for the
                                  -- copy operation.
                                  ("imageSubresources" ::: Vector ImageSubresourceLayers)
                               -> io ()
cmdCopyMemoryToImageIndirectNV commandBuffer
                                 copyBufferAddress
                                 stride
                                 dstImage
                                 dstImageLayout
                                 imageSubresources = liftIO . evalContT $ do
  let vkCmdCopyMemoryToImageIndirectNVPtr = pVkCmdCopyMemoryToImageIndirectNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMemoryToImageIndirectNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryToImageIndirectNV is null" Nothing Nothing
  let vkCmdCopyMemoryToImageIndirectNV' = mkVkCmdCopyMemoryToImageIndirectNV vkCmdCopyMemoryToImageIndirectNVPtr
  pPImageSubresources <- ContT $ allocaBytes @ImageSubresourceLayers ((Data.Vector.length (imageSubresources)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPImageSubresources `plusPtr` (16 * (i)) :: Ptr ImageSubresourceLayers) (e)) (imageSubresources)
  lift $ traceAroundEvent "vkCmdCopyMemoryToImageIndirectNV" (vkCmdCopyMemoryToImageIndirectNV'
                                                                (commandBufferHandle (commandBuffer))
                                                                (copyBufferAddress)
                                                                ((fromIntegral (Data.Vector.length $ (imageSubresources)) :: Word32))
                                                                (stride)
                                                                (dstImage)
                                                                (dstImageLayout)
                                                                (pPImageSubresources))
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR


-- | VkPhysicalDeviceCopyMemoryIndirectFeaturesNV - Structure describing
-- indirect copy features supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCopyMemoryIndirectFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceCopyMemoryIndirectFeaturesNV', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCopyMemoryIndirectFeaturesNV = PhysicalDeviceCopyMemoryIndirectFeaturesNV
  { -- | #features-indirectCopy# @indirectCopy@ indicates whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#indirect-copies indirect memory to memory or, memory to image copies>
    -- are supported.
    indirectCopy :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCopyMemoryIndirectFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCopyMemoryIndirectFeaturesNV

instance ToCStruct PhysicalDeviceCopyMemoryIndirectFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCopyMemoryIndirectFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (indirectCopy))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCopyMemoryIndirectFeaturesNV where
  peekCStruct p = do
    indirectCopy <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCopyMemoryIndirectFeaturesNV
             (bool32ToBool indirectCopy)

instance Storable PhysicalDeviceCopyMemoryIndirectFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCopyMemoryIndirectFeaturesNV where
  zero = PhysicalDeviceCopyMemoryIndirectFeaturesNV
           zero


-- No documentation found for TopLevel "VkCopyMemoryIndirectCommandNV"
type CopyMemoryIndirectCommandNV = CopyMemoryIndirectCommandKHR


-- No documentation found for TopLevel "VkCopyMemoryToImageIndirectCommandNV"
type CopyMemoryToImageIndirectCommandNV = CopyMemoryToImageIndirectCommandKHR


-- No documentation found for TopLevel "VkPhysicalDeviceCopyMemoryIndirectPropertiesNV"
type PhysicalDeviceCopyMemoryIndirectPropertiesNV = PhysicalDeviceCopyMemoryIndirectPropertiesKHR


type NV_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COPY_MEMORY_INDIRECT_SPEC_VERSION"
pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1


type NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_NV_copy_memory_indirect"

-- No documentation found for TopLevel "VK_NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME"
pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_NV_copy_memory_indirect"

