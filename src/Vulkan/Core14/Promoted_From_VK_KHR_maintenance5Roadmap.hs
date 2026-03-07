{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance5Roadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap  ( getRenderingAreaGranularity
                                                               , cmdBindIndexBuffer2
                                                               , getImageSubresourceLayout2
                                                               , getDeviceImageSubresourceLayout
                                                               , BufferUsageFlags2CreateInfo(..)
                                                               , PipelineCreateFlags2CreateInfo(..)
                                                               , PhysicalDeviceMaintenance5Features(..)
                                                               , PhysicalDeviceMaintenance5Properties(..)
                                                               , RenderingAreaInfo(..)
                                                               , ImageSubresource2(..)
                                                               , SubresourceLayout2(..)
                                                               , DeviceImageSubresourceInfo(..)
                                                               , Format(..)
                                                               , StructureType(..)
                                                               , BufferUsageFlagBits2(..)
                                                               , BufferUsageFlags2
                                                               , PipelineCreateFlagBits2(..)
                                                               , PipelineCreateFlags2
                                                               ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core14.Enums.BufferUsageFlags2 (BufferUsageFlags2)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindIndexBuffer2))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageSubresourceLayout))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout2))
import Vulkan.Dynamic (DeviceCmds(pVkGetRenderingAreaGranularity))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionPropertiesEXT)
import Vulkan.Core10.Image (ImageCreateInfo)
import Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core14.Enums.PipelineCreateFlags2 (PipelineCreateFlags2)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Core14.PromotedStreamingTransfers' (SubresourceHostMemcpySize)
import Vulkan.Core10.Image (SubresourceLayout)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_AREA_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2))
import Vulkan.Core14.Enums.BufferUsageFlags2 (BufferUsageFlagBits2(..))
import Vulkan.Core14.Enums.BufferUsageFlags2 (BufferUsageFlags2)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core14.Enums.PipelineCreateFlags2 (PipelineCreateFlagBits2(..))
import Vulkan.Core14.Enums.PipelineCreateFlags2 (PipelineCreateFlags2)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderingAreaGranularity
  :: FunPtr (Ptr Device_T -> Ptr RenderingAreaInfo -> Ptr Extent2D -> IO ()) -> Ptr Device_T -> Ptr RenderingAreaInfo -> Ptr Extent2D -> IO ()

-- | vkGetRenderingAreaGranularity - Returns the granularity for dynamic
-- rendering optimal render area
--
-- = Description
--
-- The conditions leading to an optimal @renderArea@ are:
--
-- -   the @offset.x@ member in @renderArea@ is a multiple of the @width@
--     member of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D'
--     (the horizontal granularity).
--
-- -   the @offset.y@ member in @renderArea@ is a multiple of the @height@
--     member of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D'
--     (the vertical granularity).
--
-- -   either the @extent.width@ member in @renderArea@ is a multiple of
--     the horizontal granularity or @offset.x@+@extent.width@ is equal to
--     the @width@ of each attachment used in the render pass instance.
--
-- -   either the @extent.height@ member in @renderArea@ is a multiple of
--     the vertical granularity or @offset.y@+@extent.height@ is equal to
--     the @height@ of each attachment used in the render pass instance.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRenderingAreaGranularity-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRenderingAreaGranularity-pRenderingAreaInfo-parameter#
--     @pRenderingAreaInfo@ /must/ be a valid pointer to a valid
--     'RenderingAreaInfo' structure
--
-- -   #VUID-vkGetRenderingAreaGranularity-pGranularity-parameter#
--     @pGranularity@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.Extent2D' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D', 'RenderingAreaInfo'
getRenderingAreaGranularity :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the logical device that owns the render pass instance.
                               Device
                            -> -- | @pRenderingAreaInfo@ is a pointer to a 'RenderingAreaInfo' structure
                               -- specifying details of the render pass instance to query the render area
                               -- granularity for.
                               RenderingAreaInfo
                            -> io (("granularity" ::: Extent2D))
getRenderingAreaGranularity device renderingAreaInfo = liftIO . evalContT $ do
  let vkGetRenderingAreaGranularityPtr = pVkGetRenderingAreaGranularity (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetRenderingAreaGranularityPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRenderingAreaGranularity is null" Nothing Nothing
  let vkGetRenderingAreaGranularity' = mkVkGetRenderingAreaGranularity vkGetRenderingAreaGranularityPtr
  pRenderingAreaInfo <- ContT $ withCStruct (renderingAreaInfo)
  pPGranularity <- ContT (withZeroCStruct @Extent2D)
  lift $ traceAroundEvent "vkGetRenderingAreaGranularity" (vkGetRenderingAreaGranularity'
                                                             (deviceHandle (device))
                                                             pRenderingAreaInfo
                                                             (pPGranularity))
  pGranularity <- lift $ peekCStruct @Extent2D pPGranularity
  pure $ (pGranularity)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer2
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> IndexType -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> IndexType -> IO ()

-- | vkCmdBindIndexBuffer2 - Bind an index buffer to a command buffer
--
-- = Description
--
-- @size@ specifies the bound size of the index buffer starting from
-- @offset@. If @size@ is 'Vulkan.Core10.APIConstants.WHOLE_SIZE' then the
-- bound size is from @offset@ to the end of the @buffer@.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance6 maintenance6>
-- feature is enabled, @buffer@ /can/ be
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'. If @buffer@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, every index fetched results in a value of zero.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindIndexBuffer2-offset-08782# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-vkCmdBindIndexBuffer2-offset-08783# The sum of @offset@ and
--     the base address of the range of
--     'Vulkan.Core10.Handles.DeviceMemory' object that is backing
--     @buffer@, /must/ be a multiple of the size of the type indicated by
--     @indexType@
--
-- -   #VUID-vkCmdBindIndexBuffer2-buffer-08784# @buffer@ /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdBindIndexBuffer2-buffer-08785# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindIndexBuffer2-indexType-08786# @indexType@ /must/ not
--     be 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- -   #VUID-vkCmdBindIndexBuffer2-indexType-08787# If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-indexTypeUint8 indexTypeUint8>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindIndexBuffer2-None-09493# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance6 maintenance6>
--     feature is not enabled, @buffer@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindIndexBuffer2-buffer-09494# If @buffer@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', offset /must/ be zero
--
-- -   #VUID-vkCmdBindIndexBuffer2-size-08767# If @size@ is not
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be a multiple
--     of the size of the type indicated by @indexType@
--
-- -   #VUID-vkCmdBindIndexBuffer2-size-08768# If @size@ is not
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', the sum of @offset@ and
--     @size@ /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindIndexBuffer2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer2-buffer-parameter# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer2-indexType-parameter# @indexType@ /must/
--     be a valid 'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-vkCmdBindIndexBuffer2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindIndexBuffer2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindIndexBuffer2-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- -   #VUID-vkCmdBindIndexBuffer2-commonparent# Both of @buffer@, and
--     @commandBuffer@ that are valid handles of non-ignored parameters
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindIndexBuffer2 is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
cmdBindIndexBuffer2 :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command is
                       -- recorded.
                       CommandBuffer
                    -> -- | @buffer@ is the buffer being bound.
                       Buffer
                    -> -- | @offset@ is the starting offset in bytes within @buffer@ used in index
                       -- buffer address calculations.
                       ("offset" ::: DeviceSize)
                    -> -- | @size@ is the size in bytes of index data bound from @buffer@.
                       DeviceSize
                    -> -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
                       -- specifying the size of the indices.
                       IndexType
                    -> io ()
cmdBindIndexBuffer2 commandBuffer buffer offset size indexType = liftIO $ do
  let vkCmdBindIndexBuffer2Ptr = pVkCmdBindIndexBuffer2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBindIndexBuffer2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindIndexBuffer2 is null" Nothing Nothing
  let vkCmdBindIndexBuffer2' = mkVkCmdBindIndexBuffer2 vkCmdBindIndexBuffer2Ptr
  traceAroundEvent "vkCmdBindIndexBuffer2" (vkCmdBindIndexBuffer2'
                                              (commandBufferHandle (commandBuffer))
                                              (buffer)
                                              (offset)
                                              (size)
                                              (indexType))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout2
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageSubresource2 -> Ptr (SomeStruct SubresourceLayout2) -> IO ()) -> Ptr Device_T -> Image -> Ptr ImageSubresource2 -> Ptr (SomeStruct SubresourceLayout2) -> IO ()

-- | vkGetImageSubresourceLayout2 - Retrieve information about an image
-- subresource
--
-- = Description
--
-- 'getImageSubresourceLayout2' behaves similarly to
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', with the ability to
-- specify extended inputs via chained input structures, and to return
-- extended information via chained output structures.
--
-- It is legal to call 'getImageSubresourceLayout2' with an @image@ created
-- with @tiling@ equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', but the members
-- of 'SubresourceLayout2'::@subresourceLayout@ will have undefined values
-- in this case.
--
-- Structures chained from 'ImageSubresource2'::@pNext@ will also be
-- updated when @tiling@ is equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageSubresourceLayout2-aspectMask-00997# The
--     @aspectMask@ member of @pSubresource@ /must/ only have a single bit
--     set
--
-- -   #VUID-vkGetImageSubresourceLayout2-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in @image@
--
-- -   #VUID-vkGetImageSubresourceLayout2-arrayLayer-01717# The
--     @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in @image@
--
-- -   #VUID-vkGetImageSubresourceLayout2-format-08886# If @format@ of the
--     @image@ is a color format that is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     and @tiling@ of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', the
--     @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2-format-04462# If @format@ of the
--     @image@ has a depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2-format-04463# If @format@ of the
--     @image@ has a stencil component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2-format-04464# If @format@ of the
--     @image@ does not contain a stencil or depth component, the
--     @aspectMask@ member of @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2-tiling-08717# If the @tiling@ of
--     the @image@ is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR'
--     and has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then the @aspectMask@ member of @pSubresource@ /must/ be a single
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-vkGetImageSubresourceLayout2-image-09434# If @image@ was
--     created with the
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory
--
-- -   #VUID-vkGetImageSubresourceLayout2-tiling-09435# If the @tiling@ of
--     the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index /i/ /must/ be
--     less than the
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageSubresourceLayout2-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2-image-parameter# @image@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource2' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2-pLayout-parameter# @pLayout@
--     /must/ be a valid pointer to a 'SubresourceLayout2' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2-image-parent# @image@ /must/ have
--     been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'ImageSubresource2', 'SubresourceLayout2'
getImageSubresourceLayout2 :: forall a io
                            . ( Extendss SubresourceLayout2 a
                              , PokeChain a
                              , PeekChain a
                              , MonadIO io )
                           => -- | @device@ is the logical device that owns the image.
                              Device
                           -> -- | @image@ is the image whose layout is being queried.
                              Image
                           -> -- | @pSubresource@ is a pointer to a 'ImageSubresource2' structure selecting
                              -- a specific image for the image subresource.
                              ImageSubresource2
                           -> io (SubresourceLayout2 a)
getImageSubresourceLayout2 device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayout2Ptr = pVkGetImageSubresourceLayout2 (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageSubresourceLayout2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout2 is null" Nothing Nothing
  let vkGetImageSubresourceLayout2' = mkVkGetImageSubresourceLayout2 vkGetImageSubresourceLayout2Ptr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @(SubresourceLayout2 _))
  lift $ traceAroundEvent "vkGetImageSubresourceLayout2" (vkGetImageSubresourceLayout2'
                                                            (deviceHandle (device))
                                                            (image)
                                                            pSubresource
                                                            (forgetExtensions (pPLayout)))
  pLayout <- lift $ peekCStruct @(SubresourceLayout2 _) pPLayout
  pure $ (pLayout)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageSubresourceLayout
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageSubresourceInfo -> Ptr (SomeStruct SubresourceLayout2) -> IO ()) -> Ptr Device_T -> Ptr DeviceImageSubresourceInfo -> Ptr (SomeStruct SubresourceLayout2) -> IO ()

-- | vkGetDeviceImageSubresourceLayout - Retrieve information about an image
-- subresource without an image object
--
-- = Description
--
-- 'getDeviceImageSubresourceLayout' behaves similarly to
-- 'getImageSubresourceLayout2', but uses a
-- 'Vulkan.Core10.Image.ImageCreateInfo' structure to specify the image
-- rather than a 'Vulkan.Core10.Handles.Image' object.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageSubresourceInfo',
-- 'SubresourceLayout2'
getDeviceImageSubresourceLayout :: forall a io
                                 . ( Extendss SubresourceLayout2 a
                                   , PokeChain a
                                   , PeekChain a
                                   , MonadIO io )
                                => -- | @device@ is the logical device that owns the image.
                                   --
                                   -- #VUID-vkGetDeviceImageSubresourceLayout-device-parameter# @device@
                                   -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                   Device
                                -> -- | @pInfo@ is a pointer to a 'DeviceImageSubresourceInfo' structure
                                   -- containing parameters required for the subresource layout query.
                                   --
                                   -- #VUID-vkGetDeviceImageSubresourceLayout-pInfo-parameter# @pInfo@ /must/
                                   -- be a valid pointer to a valid 'DeviceImageSubresourceInfo' structure
                                   DeviceImageSubresourceInfo
                                -> io (SubresourceLayout2 a)
getDeviceImageSubresourceLayout device info = liftIO . evalContT $ do
  let vkGetDeviceImageSubresourceLayoutPtr = pVkGetDeviceImageSubresourceLayout (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageSubresourceLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageSubresourceLayout is null" Nothing Nothing
  let vkGetDeviceImageSubresourceLayout' = mkVkGetDeviceImageSubresourceLayout vkGetDeviceImageSubresourceLayoutPtr
  pInfo <- ContT $ withCStruct (info)
  pPLayout <- ContT (withZeroCStruct @(SubresourceLayout2 _))
  lift $ traceAroundEvent "vkGetDeviceImageSubresourceLayout" (vkGetDeviceImageSubresourceLayout'
                                                                 (deviceHandle (device))
                                                                 pInfo
                                                                 (forgetExtensions (pPLayout)))
  pLayout <- lift $ peekCStruct @(SubresourceLayout2 _) pPLayout
  pure $ (pLayout)


-- | VkBufferUsageFlags2CreateInfo - Extended buffer usage flags
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of a buffer creation
-- structure, @usage@ is used instead of the corresponding @usage@ value
-- passed in that creation structure, allowing additional usage flags to be
-- specified. If this structure is included in the @pNext@ chain of a
-- buffer query structure, the usage flags of the buffer are returned in
-- @usage@ of this structure, and the usage flags representable in @usage@
-- of the buffer query structure are also returned in that field.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferUsageFlags2CreateInfo = BufferUsageFlags2CreateInfo
  { -- | @usage@ is a bitmask of
    -- 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2' specifying
    -- allowed usages of the buffer.
    --
    -- #VUID-VkBufferUsageFlags2CreateInfo-usage-parameter# @usage@ /must/ be a
    -- valid combination of
    -- 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2' values
    --
    -- #VUID-VkBufferUsageFlags2CreateInfo-usage-requiredbitmask# @usage@
    -- /must/ not be @0@
    usage :: BufferUsageFlags2 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferUsageFlags2CreateInfo)
#endif
deriving instance Show BufferUsageFlags2CreateInfo

instance ToCStruct BufferUsageFlags2CreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferUsageFlags2CreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferUsageFlags2)) (usage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferUsageFlags2)) (zero)
    f

instance FromCStruct BufferUsageFlags2CreateInfo where
  peekCStruct p = do
    usage <- peek @BufferUsageFlags2 ((p `plusPtr` 16 :: Ptr BufferUsageFlags2))
    pure $ BufferUsageFlags2CreateInfo
             usage

instance Storable BufferUsageFlags2CreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferUsageFlags2CreateInfo where
  zero = BufferUsageFlags2CreateInfo
           zero


-- | VkPipelineCreateFlags2CreateInfo - Extended pipeline create flags
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of a pipeline
-- creation structure, @flags@ is used instead of the corresponding @flags@
-- value passed in that creation structure, allowing additional creation
-- flags to be specified.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCreateFlags2CreateInfo = PipelineCreateFlags2CreateInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2'
    -- specifying how a pipeline will be generated.
    --
    -- #VUID-VkPipelineCreateFlags2CreateInfo-flags-parameter# @flags@ /must/
    -- be a valid combination of
    -- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2'
    -- values
    flags :: PipelineCreateFlags2 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreateFlags2CreateInfo)
#endif
deriving instance Show PipelineCreateFlags2CreateInfo

instance ToCStruct PipelineCreateFlags2CreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreateFlags2CreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineCreateFlags2CreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineCreateFlags2 ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2))
    pure $ PipelineCreateFlags2CreateInfo
             flags

instance Storable PipelineCreateFlags2CreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreateFlags2CreateInfo where
  zero = PipelineCreateFlags2CreateInfo
           zero


-- | VkPhysicalDeviceMaintenance5Features - Structure describing whether the
-- implementation supports maintenance5 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance5Features' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceMaintenance5Features', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance5Features = PhysicalDeviceMaintenance5Features
  { -- | #extension-features-maintenance5# @maintenance5@ indicates that the
    -- implementation supports the following:
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A1B5G5R5_UNORM_PACK16'.
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM'.
    --
    -- -   A property to indicate that multisample coverage operations are
    --     performed after sample counting in EarlyFragmentTests mode.
    --
    -- -   Creating a 'Vulkan.Core10.Handles.BufferView' with a subset of the
    --     associated 'Vulkan.Core10.Handles.Buffer' usage using
    --     'BufferUsageFlags2CreateInfo'.
    --
    -- -   A new function 'cmdBindIndexBuffer2', allowing a range of memory to
    --     be bound as an index buffer.
    --
    -- -   'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr' will return
    --     @NULL@ for function pointers of core functions for versions higher
    --     than the version requested by the application.
    --
    -- -   'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
    --     supports using 'Vulkan.Core10.APIConstants.WHOLE_SIZE' in the
    --     @pSizes@ parameter.
    --
    -- -   If @PointSize@ is not written, a default value of @1.0@ is used for
    --     the size of points.
    --
    -- -   'Vulkan.Core10.Shader.ShaderModuleCreateInfo' /can/ be added as a
    --     chained structure to pipeline creation via
    --     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
    --     rather than having to create a shader module.
    --
    -- -   A function 'getRenderingAreaGranularity' to query the optimal render
    --     area for a dynamic rendering instance.
    --
    -- -   A property to indicate that depth\/stencil texturing operations with
    --     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    --     defined behavior.
    --
    -- -   'getDeviceImageSubresourceLayout' allows an application to perform a
    --     'Vulkan.Core10.Image.getImageSubresourceLayout' query without having
    --     to create an image.
    --
    -- -   'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' as the
    --     @layerCount@ member of
    --     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'.
    --
    -- -   A property to indicate whether @PointSize@ controls the final
    --     rasterization of polygons if
    --     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-polygonmode polygon mode>
    --     is 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT'.
    --
    -- -   Two properties to indicate the non-strict line rasterization
    --     algorithm used.
    --
    -- -   Two new flags words
    --     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2'
    --     and 'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2'.
    --
    -- -   Physical-device-level functions /can/ now be called with any value
    --     in the valid range for a type beyond the defined enumerants, such
    --     that applications can avoid checking individual features,
    --     extensions, or versions before querying supported properties of a
    --     particular enumerant.
    --
    -- -   Copies between images of any type are allowed, with 1D images
    --     treated as 2D images with a height of @1@.
    maintenance5 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance5Features)
#endif
deriving instance Show PhysicalDeviceMaintenance5Features

instance ToCStruct PhysicalDeviceMaintenance5Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance5Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance5))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance5Features where
  peekCStruct p = do
    maintenance5 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance5Features
             (bool32ToBool maintenance5)

instance Storable PhysicalDeviceMaintenance5Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance5Features where
  zero = PhysicalDeviceMaintenance5Features
           zero


-- | VkPhysicalDeviceMaintenance5Properties - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance5
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance5Properties' structure is included in
-- the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance5Properties = PhysicalDeviceMaintenance5Properties
  { -- | @earlyFragmentMultisampleCoverageAfterSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-shader fragment shading>
    -- and
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-covg multisample coverage>
    -- operations are performed after
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-shader fragment shaders>
    -- with @EarlyFragmentTests@ execution mode.
    earlyFragmentMultisampleCoverageAfterSampleCounting :: Bool
  , -- | @earlyFragmentSampleMaskTestBeforeSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-samplemask sample mask test>
    -- operation is performed before
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-shader fragment shaders>
    -- using the @EarlyFragmentTests@ execution mode.
    earlyFragmentSampleMaskTestBeforeSampleCounting :: Bool
  , -- | @depthStencilSwizzleOneSupport@ is a boolean indicating that
    -- depth\/stencil texturing operations with
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    -- defined behavior.
    depthStencilSwizzleOneSupport :: Bool
  , -- | @polygonModePointSize@ is a boolean value indicating whether the point
    -- size of the final rasterization of polygons with
    -- 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT' is controlled by
    -- @PointSize@.
    polygonModePointSize :: Bool
  , -- | @nonStrictSinglePixelWideLinesUseParallelogram@ is a boolean value
    -- indicating whether non-strict lines with a width of 1.0 are rasterized
    -- as parallelograms or using Bresenham’s algorithm.
    nonStrictSinglePixelWideLinesUseParallelogram :: Bool
  , -- | @nonStrictWideLinesUseParallelogram@ is a boolean value indicating
    -- whether non-strict lines with a width greater than 1.0 are rasterized as
    -- parallelograms or using Bresenham’s algorithm.
    nonStrictWideLinesUseParallelogram :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance5Properties)
#endif
deriving instance Show PhysicalDeviceMaintenance5Properties

instance ToCStruct PhysicalDeviceMaintenance5Properties where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance5Properties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (earlyFragmentMultisampleCoverageAfterSampleCounting))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (earlyFragmentSampleMaskTestBeforeSampleCounting))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (depthStencilSwizzleOneSupport))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (polygonModePointSize))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (nonStrictSinglePixelWideLinesUseParallelogram))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (nonStrictWideLinesUseParallelogram))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance5Properties where
  peekCStruct p = do
    earlyFragmentMultisampleCoverageAfterSampleCounting <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    earlyFragmentSampleMaskTestBeforeSampleCounting <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    depthStencilSwizzleOneSupport <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    polygonModePointSize <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    nonStrictSinglePixelWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    nonStrictWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance5Properties
             (bool32ToBool earlyFragmentMultisampleCoverageAfterSampleCounting)
             (bool32ToBool earlyFragmentSampleMaskTestBeforeSampleCounting)
             (bool32ToBool depthStencilSwizzleOneSupport)
             (bool32ToBool polygonModePointSize)
             (bool32ToBool nonStrictSinglePixelWideLinesUseParallelogram)
             (bool32ToBool nonStrictWideLinesUseParallelogram)

instance Storable PhysicalDeviceMaintenance5Properties where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance5Properties where
  zero = PhysicalDeviceMaintenance5Properties
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRenderingAreaInfo - Structure describing rendering area granularity
-- query info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getRenderingAreaGranularity', 'getRenderingAreaGranularity'
data RenderingAreaInfo = RenderingAreaInfo
  { -- | @viewMask@ is the viewMask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.Format.Format' values defining the format of color
    -- attachments used in the render pass instance.
    colorAttachmentFormats :: Vector Format
  , -- | @depthAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the depth attachment used in the render pass
    -- instance.
    depthAttachmentFormat :: Format
  , -- | @stencilAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the stencil attachment used in the render pass
    -- instance.
    stencilAttachmentFormat :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAreaInfo)
#endif
deriving instance Show RenderingAreaInfo

instance ToCStruct RenderingAreaInfo where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAreaInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_AREA_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentFormats)) :: Word32))
    pPColorAttachmentFormats' <- ContT $ allocaBytes @Format ((Data.Vector.length (colorAttachmentFormats)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (colorAttachmentFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPColorAttachmentFormats')
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (depthAttachmentFormat)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (stencilAttachmentFormat)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_AREA_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    f

instance FromCStruct RenderingAreaInfo where
  peekCStruct p = do
    viewMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    pure $ RenderingAreaInfo
             viewMask
             pColorAttachmentFormats'
             depthAttachmentFormat
             stencilAttachmentFormat

instance Zero RenderingAreaInfo where
  zero = RenderingAreaInfo
           zero
           mempty
           zero
           zero


-- | VkImageSubresource2 - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'DeviceImageSubresourceInfo',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getImageSubresourceLayout2', 'getImageSubresourceLayout2',
-- 'getImageSubresourceLayout2'
data ImageSubresource2 = ImageSubresource2
  { -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure.
    --
    -- #VUID-VkImageSubresource2-imageSubresource-parameter# @imageSubresource@
    -- /must/ be a valid
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure
    imageSubresource :: ImageSubresource }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresource2)
#endif
deriving instance Show ImageSubresource2

instance ToCStruct ImageSubresource2 where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (imageSubresource)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (zero)
    f

instance FromCStruct ImageSubresource2 where
  peekCStruct p = do
    imageSubresource <- peekCStruct @ImageSubresource ((p `plusPtr` 16 :: Ptr ImageSubresource))
    pure $ ImageSubresource2
             imageSubresource

instance Storable ImageSubresource2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource2 where
  zero = ImageSubresource2
           zero


-- | VkSubresourceLayout2 - Structure specifying subresource layout
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubresourceLayout2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2'
--
-- -   #VUID-VkSubresourceLayout2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionPropertiesEXT'
--     or
--     'Vulkan.Core14.PromotedStreamingTransfers'.SubresourceHostMemcpySize'
--
-- -   #VUID-VkSubresourceLayout2-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Image.SubresourceLayout',
-- 'getDeviceImageSubresourceLayout', 'getDeviceImageSubresourceLayout',
-- 'getImageSubresourceLayout2', 'getImageSubresourceLayout2',
-- 'getImageSubresourceLayout2'
data SubresourceLayout2 (es :: [Type]) = SubresourceLayout2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @subresourceLayout@ is a 'Vulkan.Core10.Image.SubresourceLayout'
    -- structure.
    subresourceLayout :: SubresourceLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceLayout2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubresourceLayout2 es)

instance Extensible SubresourceLayout2 where
  extensibleTypeName = "SubresourceLayout2"
  setNext SubresourceLayout2{..} next' = SubresourceLayout2{next = next', ..}
  getNext SubresourceLayout2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubresourceLayout2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageCompressionPropertiesEXT = Just f
    | Just Refl <- eqT @e @SubresourceHostMemcpySize = Just f
    | otherwise = Nothing

instance ( Extendss SubresourceLayout2 es
         , PokeChain es ) => ToCStruct (SubresourceLayout2 es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceLayout2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (subresourceLayout)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (zero)
    lift $ f

instance ( Extendss SubresourceLayout2 es
         , PeekChain es ) => FromCStruct (SubresourceLayout2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    subresourceLayout <- peekCStruct @SubresourceLayout ((p `plusPtr` 16 :: Ptr SubresourceLayout))
    pure $ SubresourceLayout2
             next subresourceLayout

instance es ~ '[] => Zero (SubresourceLayout2 es) where
  zero = SubresourceLayout2
           ()
           zero


-- | VkDeviceImageSubresourceInfo - Image creation information for querying
-- subresource layout
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceImageSubresourceInfo-aspectMask-00997# The
--     @aspectMask@ member of @pSubresource@ /must/ only have a single bit
--     set
--
-- -   #VUID-VkDeviceImageSubresourceInfo-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in @pCreateInfo@
--
-- -   #VUID-VkDeviceImageSubresourceInfo-arrayLayer-01717# The
--     @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in @pCreateInfo@
--
-- -   #VUID-VkDeviceImageSubresourceInfo-format-08886# If @format@ of the
--     @image@ is a color format that is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     and @tiling@ of the @pCreateInfo@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', the
--     @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfo-format-04462# If @format@ of the
--     @pCreateInfo@ has a depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfo-format-04463# If @format@ of the
--     @pCreateInfo@ has a stencil component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfo-format-04464# If @format@ of the
--     @pCreateInfo@ does not contain a stencil or depth component, the
--     @aspectMask@ member of @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfo-tiling-08717# If the @tiling@ of
--     the @pCreateInfo@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar multi-planar format>,
--     then the @aspectMask@ member of @pSubresource@ /must/ be a single
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-multiplanar-image-aspect multi-planar aspect mask>
--     bit
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceImageSubresourceInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO'
--
-- -   #VUID-VkDeviceImageSubresourceInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDeviceImageSubresourceInfo-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Image.ImageCreateInfo' structure
--
-- -   #VUID-VkDeviceImageSubresourceInfo-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource2' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Image.ImageCreateInfo', 'ImageSubresource2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceImageSubresourceLayout', 'getDeviceImageSubresourceLayout'
data DeviceImageSubresourceInfo = DeviceImageSubresourceInfo
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure containing parameters affecting creation of the image to
    -- query.
    createInfo :: SomeStruct ImageCreateInfo
  , -- | @pSubresource@ is a pointer to a 'ImageSubresource2' structure selecting
    -- a specific image subresource for the query.
    subresource :: ImageSubresource2
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceImageSubresourceInfo)
#endif
deriving instance Show DeviceImageSubresourceInfo

instance ToCStruct DeviceImageSubresourceInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceImageSubresourceInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    pSubresource'' <- ContT $ withCStruct (subresource)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2))) pSubresource''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    pSubresource'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2))) pSubresource''
    lift $ f

instance FromCStruct DeviceImageSubresourceInfo where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _))))
    pSubresource <- peekCStruct @ImageSubresource2 =<< peek ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2)))
    pure $ DeviceImageSubresourceInfo
             pCreateInfo pSubresource

instance Zero DeviceImageSubresourceInfo where
  zero = DeviceImageSubresourceInfo
           (SomeStruct zero)
           zero

