{-# language CPP #-}
-- | = Name
--
-- VK_NV_copy_memory_indirect - device extension
--
-- == VK_NV_copy_memory_indirect
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
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
--     -   Requires @VK_KHR_buffer_device_address@ to be enabled for any
--         device-level functionality
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-10-14 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- 'CopyMemoryIndirectCommandNV', 'CopyMemoryToImageIndirectCommandNV',
-- 'PhysicalDeviceCopyMemoryIndirectFeaturesNV',
-- 'PhysicalDeviceCopyMemoryIndirectPropertiesNV',
-- 'cmdCopyMemoryIndirectNV', 'cmdCopyMemoryToImageIndirectNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_copy_memory_indirect Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_copy_memory_indirect  ( cmdCopyMemoryIndirectNV
                                                     , cmdCopyMemoryToImageIndirectNV
                                                     , CopyMemoryIndirectCommandNV(..)
                                                     , CopyMemoryToImageIndirectCommandNV(..)
                                                     , PhysicalDeviceCopyMemoryIndirectFeaturesNV(..)
                                                     , PhysicalDeviceCopyMemoryIndirectPropertiesNV(..)
                                                     , NV_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                     , pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION
                                                     , NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME
                                                     , pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryIndirectNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToImageIndirectNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV))
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-indirectCopy indirect copies>
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
--     allocated from /must/ support at least one of the
--     'PhysicalDeviceCopyMemoryIndirectPropertiesNV'::@supportedQueues@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyMemoryIndirectNV-renderpass# This command /must/ only
--     be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
                        -> -- | @copyBufferAddress@ is the buffer address specifying the copy
                           -- parameters. This buffer is laid out in memory as an array of
                           -- 'CopyMemoryIndirectCommandNV' structures.
                           ("copyBufferAddress" ::: DeviceAddress)
                        -> -- | @copyCount@ is the number of copies to execute, and can be zero.
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

-- | vkCmdCopyMemoryToImageIndirectNV - Copy data from a memory region into
-- an image
--
-- = Description
--
-- Each region in @copyBufferAddress@ is copied from the source memory
-- region to an image region in the destination image. If the destination
-- image is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
-- starting slice and number of slices to copy are specified in
-- @pImageSubresources@::@baseArrayLayer@ and
-- @pImageSubresources@::@layerCount@ respectively. The copy /must/ be
-- performed on a queue that supports indirect copy operations, see
-- 'PhysicalDeviceCopyMemoryIndirectPropertiesNV'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-None-07660# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-indirectCopy indirect copies>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07661# @dstImage@
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-aspectMask-07662# The
--     @aspectMask@ member for every subresource in @pImageSubresources@
--     /must/ only have a single bit set
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07663# The image
--     region specified by each element in @copyBufferAddress@ /must/ be a
--     region that is contained within @dstImage@
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07664# @dstImage@
--     /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07665# If @dstImage@
--     is non-sparse then it /must/ be bound completely and contiguously to
--     a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-dstImage-07666# @dstImage@
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
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-mipLevel-07670# The specified
--     @mipLevel@ of each region /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@
--     was created
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-baseArrayLayer-07671# The
--     specified @baseArrayLayer@ + @layerCount@ of each region /must/ be
--     less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-imageOffset-07672# The
--     @imageOffset@ and @imageExtent@ members of each region /must/
--     respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
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
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'.
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-imageOffset-07675# For each
--     region in @copyBufferAddress@, @imageOffset.y@ and
--     (@imageExtent.height@ + @imageOffset.y@) /must/ both be greater than
--     or equal to @0@ and less than or equal to the height of the
--     specified subresource
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-offset-07676# @offset@ /must/
--     be 4 byte aligned
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-stride-07677# @stride@ /must/
--     be a multiple of @4@ and /must/ be greater than or equal to
--     sizeof('CopyMemoryToImageIndirectCommandNV')
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
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
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyMemoryToImageIndirectNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
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
                               -> -- | @copyBufferAddress@ is the buffer address specifying the copy
                                  -- parameters. This buffer is laid out in memory as an array of
                                  -- 'CopyMemoryToImageIndirectCommandNV' structures.
                                  ("copyBufferAddress" ::: DeviceAddress)
                               -> -- | @stride@ is the byte stride between successive sets of copy parameters.
                                  ("stride" ::: Word32)
                               -> -- | @dstImage@ is the destination image.
                                  ("dstImage" ::: Image)
                               -> -- | @dstImageLayout@ is the layout of the destination image subresources for
                                  -- the copy.
                                  ("dstImageLayout" ::: ImageLayout)
                               -> -- | @pImageSubresources@ is a pointer to an array of size @copyCount@ of
                                  -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
                                  -- specify the specific image subresource of the destination image data for
                                  -- that copy.
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


-- | VkCopyMemoryIndirectCommandNV - Structure specifying indirect memory
-- region copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryIndirectCommandNV-srcAddress-07657# The
--     @srcAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectCommandNV-dstAddress-07658# The
--     @dstAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryIndirectCommandNV-size-07659# The @size@ /must/ be
--     4 byte aligned
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data CopyMemoryIndirectCommandNV = CopyMemoryIndirectCommandNV
  { -- | @srcAddress@ is the starting address of the source host or device memory
    -- to copy from.
    srcAddress :: DeviceAddress
  , -- | @dstAddress@ is the starting address of the destination host or device
    -- memory to copy to.
    dstAddress :: DeviceAddress
  , -- | @size@ is the size of the copy in bytes.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryIndirectCommandNV)
#endif
deriving instance Show CopyMemoryIndirectCommandNV

instance ToCStruct CopyMemoryIndirectCommandNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryIndirectCommandNV{..} f = do
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

instance FromCStruct CopyMemoryIndirectCommandNV where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    dstAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ CopyMemoryIndirectCommandNV
             srcAddress dstAddress size

instance Storable CopyMemoryIndirectCommandNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMemoryIndirectCommandNV where
  zero = CopyMemoryIndirectCommandNV
           zero
           zero
           zero


-- | VkCopyMemoryToImageIndirectCommandNV - Structure specifying indirect
-- buffer image copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-srcAddress-07678# The
--     @srcAddress@ /must/ be 4 byte aligned
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-bufferRowLength-07679#
--     @bufferRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-bufferImageHeight-07680#
--     @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-imageOffset-07681#
--     @imageOffset@ /must/ specify a valid offset in the destination image
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-imageExtent-07682#
--     @imageExtent@ /must/ specify a valid region in the destination image
--     and can be @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToImageIndirectCommandNV-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D',
-- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D'
data CopyMemoryToImageIndirectCommandNV = CopyMemoryToImageIndirectCommandNV
  { -- | @srcAddress@ is the starting address of the source host or device memory
    -- to copy from.
    srcAddress :: DeviceAddress
  , -- | @bufferRowLength@ and @bufferImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in buffer memory, and
    -- control the addressing calculations. If either of these values is zero,
    -- that aspect of the buffer memory is considered to be tightly packed
    -- according to the @imageExtent@.
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkCopyMemoryToImageIndirectCommandNV" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the
    -- destination image data, which /must/ match the values specified in
    -- @pImageSubresources@ parameter of 'cmdCopyMemoryToImageIndirectNV'
    -- during command recording.
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
deriving instance Generic (CopyMemoryToImageIndirectCommandNV)
#endif
deriving instance Show CopyMemoryToImageIndirectCommandNV

instance ToCStruct CopyMemoryToImageIndirectCommandNV where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToImageIndirectCommandNV{..} f = do
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

instance FromCStruct CopyMemoryToImageIndirectCommandNV where
  peekCStruct p = do
    srcAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 44 :: Ptr Extent3D))
    pure $ CopyMemoryToImageIndirectCommandNV
             srcAddress
             bufferRowLength
             bufferImageHeight
             imageSubresource
             imageOffset
             imageExtent

instance Storable CopyMemoryToImageIndirectCommandNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMemoryToImageIndirectCommandNV where
  zero = CopyMemoryToImageIndirectCommandNV
           zero
           zero
           zero
           zero
           zero
           zero


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
-- supported. 'PhysicalDeviceCopyMemoryIndirectFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
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
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#indirect-copies indirect copies>
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


-- | VkPhysicalDeviceCopyMemoryIndirectPropertiesNV - Structure describing
-- supported queues for indirect copy
--
-- = Description
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-indirectCopy indirect copies>
-- feature is supported, @supportedQueues@ /must/ return at least one
-- supported queue.
--
-- If the 'PhysicalDeviceCopyMemoryIndirectPropertiesNV' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_copy_memory_indirect VK_NV_copy_memory_indirect>,
-- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCopyMemoryIndirectPropertiesNV = PhysicalDeviceCopyMemoryIndirectPropertiesNV
  { -- | @supportedQueues@ is a bitmask of
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits' indicating the queues
    -- on which
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#indirect-copies indirect copy commands>
    -- are supported.
    supportedQueues :: QueueFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCopyMemoryIndirectPropertiesNV)
#endif
deriving instance Show PhysicalDeviceCopyMemoryIndirectPropertiesNV

instance ToCStruct PhysicalDeviceCopyMemoryIndirectPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCopyMemoryIndirectPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (supportedQueues)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr QueueFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceCopyMemoryIndirectPropertiesNV where
  peekCStruct p = do
    supportedQueues <- peek @QueueFlags ((p `plusPtr` 16 :: Ptr QueueFlags))
    pure $ PhysicalDeviceCopyMemoryIndirectPropertiesNV
             supportedQueues

instance Storable PhysicalDeviceCopyMemoryIndirectPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCopyMemoryIndirectPropertiesNV where
  zero = PhysicalDeviceCopyMemoryIndirectPropertiesNV
           zero


type NV_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COPY_MEMORY_INDIRECT_SPEC_VERSION"
pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COPY_MEMORY_INDIRECT_SPEC_VERSION = 1


type NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_NV_copy_memory_indirect"

-- No documentation found for TopLevel "VK_NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME"
pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME = "VK_NV_copy_memory_indirect"

