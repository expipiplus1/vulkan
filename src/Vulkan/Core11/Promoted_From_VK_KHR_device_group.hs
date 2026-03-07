{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_group"
module Vulkan.Core11.Promoted_From_VK_KHR_device_group  ( getDeviceGroupPeerMemoryFeatures
                                                        , cmdSetDeviceMask
                                                        , cmdDispatchBase
                                                        , pattern PIPELINE_CREATE_DISPATCH_BASE
                                                        , MemoryAllocateFlagsInfo(..)
                                                        , DeviceGroupRenderPassBeginInfo(..)
                                                        , DeviceGroupCommandBufferBeginInfo(..)
                                                        , DeviceGroupSubmitInfo(..)
                                                        , DeviceGroupBindSparseInfo(..)
                                                        , StructureType(..)
                                                        , PipelineCreateFlagBits(..)
                                                        , PipelineCreateFlags
                                                        , DependencyFlagBits(..)
                                                        , DependencyFlags
                                                        , PeerMemoryFeatureFlagBits(..)
                                                        , PeerMemoryFeatureFlags
                                                        , MemoryAllocateFlagBits(..)
                                                        , MemoryAllocateFlags
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchBase))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDeviceMask))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupPeerMemoryFeatures))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_DISPATCH_BASE_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(..))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPeerMemoryFeatures
  :: FunPtr (Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()) -> Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()

-- | vkGetDeviceGroupPeerMemoryFeatures - Query supported peer memory
-- features of a device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits.PeerMemoryFeatureFlags'
getDeviceGroupPeerMemoryFeatures :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the logical device that owns the memory.
                                    --
                                    -- #VUID-vkGetDeviceGroupPeerMemoryFeatures-device-parameter# @device@
                                    -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                    Device
                                 -> -- | @heapIndex@ is the index of the memory heap from which the memory is
                                    -- allocated.
                                    --
                                    -- #VUID-vkGetDeviceGroupPeerMemoryFeatures-heapIndex-00691# @heapIndex@
                                    -- /must/ be less than @memoryHeapCount@
                                    ("heapIndex" ::: Word32)
                                 -> -- | @localDeviceIndex@ is the device index of the physical device that
                                    -- performs the memory access.
                                    --
                                    -- #VUID-vkGetDeviceGroupPeerMemoryFeatures-localDeviceIndex-00692#
                                    -- @localDeviceIndex@ /must/ be a valid device index
                                    --
                                    -- #VUID-vkGetDeviceGroupPeerMemoryFeatures-localDeviceIndex-00694#
                                    -- @localDeviceIndex@ /must/ not equal @remoteDeviceIndex@
                                    ("localDeviceIndex" ::: Word32)
                                 -> -- | @remoteDeviceIndex@ is the device index of the physical device that the
                                    -- memory is allocated for.
                                    --
                                    -- #VUID-vkGetDeviceGroupPeerMemoryFeatures-remoteDeviceIndex-00693#
                                    -- @remoteDeviceIndex@ /must/ be a valid device index
                                    ("remoteDeviceIndex" ::: Word32)
                                 -> io (("peerMemoryFeatures" ::: PeerMemoryFeatureFlags))
getDeviceGroupPeerMemoryFeatures device
                                   heapIndex
                                   localDeviceIndex
                                   remoteDeviceIndex = liftIO . evalContT $ do
  let vkGetDeviceGroupPeerMemoryFeaturesPtr = pVkGetDeviceGroupPeerMemoryFeatures (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceGroupPeerMemoryFeaturesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupPeerMemoryFeatures is null" Nothing Nothing
  let vkGetDeviceGroupPeerMemoryFeatures' = mkVkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeaturesPtr
  pPPeerMemoryFeatures <- ContT $ bracket (callocBytes @PeerMemoryFeatureFlags 4) free
  lift $ traceAroundEvent "vkGetDeviceGroupPeerMemoryFeatures" (vkGetDeviceGroupPeerMemoryFeatures'
                                                                  (deviceHandle (device))
                                                                  (heapIndex)
                                                                  (localDeviceIndex)
                                                                  (remoteDeviceIndex)
                                                                  (pPPeerMemoryFeatures))
  pPeerMemoryFeatures <- lift $ peek @PeerMemoryFeatureFlags pPPeerMemoryFeatures
  pure $ (pPeerMemoryFeatures)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDeviceMask
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetDeviceMask - Modify device mask of a command buffer
--
-- = Description
--
-- @deviceMask@ is used to filter out subsequent commands from executing on
-- all physical devices whose bit indices are not set in the mask, except
-- commands beginning a render pass instance, commands transitioning to the
-- next subpass in the render pass instance, and commands ending a render
-- pass instance, which always execute on the set of physical devices whose
-- bit indices are included in the @deviceMask@ member of the
-- 'DeviceGroupRenderPassBeginInfo' structure passed to the command
-- beginning the corresponding render pass instance.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDeviceMask-deviceMask-00108# @deviceMask@ /must/ be a
--     valid device mask value
--
-- -   #VUID-vkCmdSetDeviceMask-deviceMask-00109# @deviceMask@ /must/ not
--     be zero
--
-- -   #VUID-vkCmdSetDeviceMask-deviceMask-00110# @deviceMask@ /must/ not
--     include any set bits that were not in the
--     'DeviceGroupCommandBufferBeginInfo'::@deviceMask@ value when the
--     command buffer began recording
--
-- -   #VUID-vkCmdSetDeviceMask-deviceMask-00111# If 'cmdSetDeviceMask' is
--     called inside a render pass instance, @deviceMask@ /must/ not
--     include any set bits that were not in the
--     'DeviceGroupRenderPassBeginInfo'::@deviceMask@ value when the render
--     pass instance began recording
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDeviceMask-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDeviceMask-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDeviceMask-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSetDeviceMask is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDeviceMask :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is command buffer whose current device mask is modified.
                    CommandBuffer
                 -> -- | @deviceMask@ is the new value of the current device mask.
                    ("deviceMask" ::: Word32)
                 -> io ()
cmdSetDeviceMask commandBuffer deviceMask = liftIO $ do
  let vkCmdSetDeviceMaskPtr = pVkCmdSetDeviceMask (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDeviceMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDeviceMask is null" Nothing Nothing
  let vkCmdSetDeviceMask' = mkVkCmdSetDeviceMask vkCmdSetDeviceMaskPtr
  traceAroundEvent "vkCmdSetDeviceMask" (vkCmdSetDeviceMask'
                                           (commandBufferHandle (commandBuffer))
                                           (deviceMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDispatchBase - Dispatch compute work items with non-zero base
-- values for the workgroup IDs
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled, with @WorkgroupId@ values ranging from [@baseGroup*@,
-- @baseGroup*@ + @groupCount*@) in each component.
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatch' is equivalent to
-- @vkCmdDispatchBase(0,0,0,groupCountX,groupCountY,groupCountZ)@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchBase-magFilter-04553# If a
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
-- -   #VUID-vkCmdDispatchBase-magFilter-09598# If a
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
-- -   #VUID-vkCmdDispatchBase-mipmapMode-04770# If a
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
-- -   #VUID-vkCmdDispatchBase-mipmapMode-09599# If a
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
-- -   #VUID-vkCmdDispatchBase-unnormalizedCoordinates-09635# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @levelCount@ and @layerCount@ /must/ be 1
--
-- -   #VUID-vkCmdDispatchBase-None-08609# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdDispatchBase-None-08610# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name
--
-- -   #VUID-vkCmdDispatchBase-None-08611# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values
--
-- -   #VUID-vkCmdDispatchBase-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDispatchBase-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchBase-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdDispatchBase-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cubicRangeClamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDispatchBase-selectableCubicWeights-09214# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-selectableCubicWeights selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchBase-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchBase-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchBase-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchBase-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-08600# If a
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
-- -   #VUID-vkCmdDispatchBase-None-08601# If a
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
-- -   #VUID-vkCmdDispatchBase-None-10068# For each array of resources that
--     is used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     the indices used to access members of the array /must/ be less than
--     the descriptor count for the identified binding in the descriptor
--     sets used by this command
--
-- -   #VUID-vkCmdDispatchBase-maintenance4-08602# If a
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
-- -   #VUID-vkCmdDispatchBase-None-08114# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are accessed as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-imageLayout-00344# If an image descriptor is
--     accessed by a shader, the
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ match the
--     subresource accessible from the 'Vulkan.Core10.Handles.ImageView' as
--     defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- -   #VUID-vkCmdDispatchBase-None-08115# If the descriptors used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-None-08116# Descriptors in bound descriptor
--     buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-None-08604# Descriptors in bound descriptor
--     buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchBase-None-08117# If the descriptors used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-None-08119# If a descriptor is dynamically
--     used with a 'Vulkan.Core10.Handles.Pipeline' created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchBase-None-08605# If a descriptor is dynamically
--     used with a 'Vulkan.Extensions.Handles.ShaderEXT' created with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' that was created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchBase-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchBase-None-08608# If a pipeline is bound to the
--     pipeline bind point used by this command, there /must/ not have been
--     any calls to dynamic state setting commands for any state specified
--     statically in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdDispatchBase-uniformBuffers-06935# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer, and that stage
--     was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchBase-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchBase-storageBuffers-06936# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer, and that stage
--     was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchBase-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-02707# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchBase-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-image-dimensions ???>
--
-- -   #VUID-vkCmdDispatchBase-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDispatchBase-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDispatchBase-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDispatchBase-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchBase-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchBase-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchBase-OpImageSampleWeightedQCOM-06971# If
--     @OpImageSampleWeightedQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageSampleWeightedQCOM-06972# If
--     @OpImageSampleWeightedQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchBase-OpImageSampleWeightedQCOM-06977# If
--     @OpImageSampleWeightedQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageSampleWeightedQCOM-06978# If any
--     command other than @OpImageSampleWeightedQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchWindow-09215# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchWindow-09216# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format
--
-- -   #VUID-vkCmdDispatchBase-OpImageBlockMatchWindow-09217# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchBase-None-07288# Any shader invocation executed
--     by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatchBase-None-09600# If a descriptor with type equal
--     to any of
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
-- -   #VUID-vkCmdDispatchBase-commandBuffer-10746# The
--     'Vulkan.Core10.Handles.DeviceMemory' object allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property that is bound to a resource accessed as a result of this
--     command /must/ be the active bound
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-bind-tile-memory bound tile memory object>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdDispatchBase-None-10678# If this command is recorded
--     inside a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--     instance, the stages corresponding to the pipeline bind point used
--     by this command /must/ only include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and\/or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-10679# If this command is recorded
--     where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, there /must/ be no access to any image while the image
--     was be transitioned to the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     layout
--
-- -   #VUID-vkCmdDispatchBase-pDescription-09900# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the
--     underlying 'Vulkan.Extensions.Handles.TensorARM' object /must/ have
--     been created with the
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_SHADER_BIT_ARM' usage
--     flag set
--
-- -   #VUID-vkCmdDispatchBase-dimensionCount-09905# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the @Rank@
--     of the @OpTypeTensorARM@ of the tensor resource variable /must/ be
--     equal to the @dimensionCount@ provided via
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@pDescription@
--     when creating the underlying 'Vulkan.Extensions.Handles.TensorARM'
--     object
--
-- -   #VUID-vkCmdDispatchBase-OpTypeTensorARM-09906# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the element
--     type of the @OpTypeTensorARM@ of the tensor resource variable /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-tensor-formats compatible>
--     with the 'Vulkan.Core10.Enums.Format.Format' of the
--     'Vulkan.Extensions.Handles.TensorViewARM' used for the access
--
-- -   #VUID-vkCmdDispatchBase-None-11297# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11298# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11299# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11397# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11300# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 4
--
-- -   #VUID-vkCmdDispatchBase-None-11301# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11302# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdDispatchBase-None-11304# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 8
--
-- -   #VUID-vkCmdDispatchBase-None-11305# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdDispatchBase-None-11306# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address pointed to by the address in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdDispatchBase-None-11308# For each
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps descriptor heap>
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     either directly or via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     a valid descriptor heap /must/ be bound
--
-- -   #VUID-vkCmdDispatchBase-None-11309# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, execution of this command /must/ not result in any descriptor
--     read accessing data outside of the user range of the respective heap
--     bound by @vkCmdBind*HeapEXT@ commands
--
-- -   #VUID-vkCmdDispatchBase-None-11372# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer or uniform
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
-- -   #VUID-vkCmdDispatchBase-None-11373# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer or storage
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
-- -   #VUID-vkCmdDispatchBase-None-11374# If the
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
-- -   #VUID-vkCmdDispatchBase-pBindInfo-11375# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     uses an embedded sampler via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     the value of @pBindInfo->reservedRangeSize@ set for
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdBindSamplerHeapEXT'
--     /must/ be greater than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>
--
-- -   #VUID-vkCmdDispatchBase-None-11376# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT'
--
-- -   #VUID-vkCmdDispatchBase-None-11398# If a
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
-- -   #VUID-vkCmdDispatchBase-None-11437# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11438# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11441# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11439# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11442# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-None-11485# If a pipeline is bound to the
--     pipeline bind point used by this command, or shader is bound to a
--     shader stage used by this command, and it was created with a
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
-- -   #VUID-vkCmdDispatchBase-index-11450# If a shader uses a sampler
--     descriptor to sample an image as a result of this command, and that
--     sampler descriptor uses a custom border color with an index defined
--     by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT',
--     the value of
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT'::@index@
--     /must/ have been registered before this command was recorded, and
--     still be registered during the sampling operation, with an
--     identically defined color
--
-- -   #VUID-vkCmdDispatchBase-protectedNoFault-11455# If
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
-- -   #VUID-vkCmdDispatchBase-protectedNoFault-11456# If
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
-- -   #VUID-vkCmdDispatchBase-None-10672# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is not enabled, this command /must/ be called outside of a render
--     pass instance
--
-- -   #VUID-vkCmdDispatchBase-aspectMask-10673# If this command is
--     recorded where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, and if the 'Vulkan.Core10.Handles.Pipeline' object bound
--     to the pipeline bind point used by this command writes to a variable
--     of storage class @Storage@ @Class@ @TileAttachmentQCOM@, the
--     corresponding 'Vulkan.Core10.Handles.ImageView' using /must/ not
--     have been created with an @aspectMask@ that contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-10674# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tileShadingPerTileDispatch tileShadingPerTileDispatch>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDispatchBase-None-10675# Memory backing image
--     subresources used as
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading-attachment-access tile attachments>
--     in the current render pass /must/ not be written in any way other
--     than as a tile attachment by this command
--
-- -   #VUID-vkCmdDispatchBase-None-10676# If any recorded command in the
--     current subpass will write to an image subresource as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading-attachment-access tile attachment>,
--     this command /must/ not read from the memory backing that image
--     subresource in any other way than as a tile attachment
--
-- -   #VUID-vkCmdDispatchBase-None-10743# If there is no bound compute
--     pipeline, a valid 'Vulkan.Extensions.Handles.ShaderEXT' /must/ be
--     bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--     stage
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, pipeline stages other than the framebuffer-space
--     and compute stages in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command /must/ not
--     write to any resource
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-04617# If any of the shader
--     stages of the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point used by this command uses the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--     capability, then @commandBuffer@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-vkCmdDispatchBase-baseGroupX-00421# @baseGroupX@ /must/ be
--     less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   #VUID-vkCmdDispatchBase-baseGroupX-00422# @baseGroupY@ /must/ be
--     less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   #VUID-vkCmdDispatchBase-baseGroupZ-00423# @baseGroupZ@ /must/ be
--     less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- -   #VUID-vkCmdDispatchBase-groupCountX-00424# @groupCountX@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     minus @baseGroupX@
--
-- -   #VUID-vkCmdDispatchBase-groupCountY-00425# @groupCountY@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     minus @baseGroupY@
--
-- -   #VUID-vkCmdDispatchBase-groupCountZ-00426# @groupCountZ@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     minus @baseGroupZ@
--
-- -   #VUID-vkCmdDispatchBase-baseGroupX-00427# If any of @baseGroupX@,
--     @baseGroupY@, or @baseGroupZ@ are not zero, then the bound compute
--     pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DISPATCH_BASE_BIT'
--     flag or the bound compute shader object /must/ have been created
--     with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DISPATCH_BASE_BIT_EXT'
--     flag
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' operations
--
-- -   #VUID-vkCmdDispatchBase-suspended# This command /must/ not be called
--     between suspended render pass instances
--
-- -   #VUID-vkCmdDispatchBase-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDispatchBase is affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDispatchBase :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @baseGroupX@ is the start value for the X component of @WorkgroupId@.
                   ("baseGroupX" ::: Word32)
                -> -- | @baseGroupY@ is the start value for the Y component of @WorkgroupId@.
                   ("baseGroupY" ::: Word32)
                -> -- | @baseGroupZ@ is the start value for the Z component of @WorkgroupId@.
                   ("baseGroupZ" ::: Word32)
                -> -- | @groupCountX@ is the number of local workgroups to dispatch in the X
                   -- dimension.
                   ("groupCountX" ::: Word32)
                -> -- | @groupCountY@ is the number of local workgroups to dispatch in the Y
                   -- dimension.
                   ("groupCountY" ::: Word32)
                -> -- | @groupCountZ@ is the number of local workgroups to dispatch in the Z
                   -- dimension.
                   ("groupCountZ" ::: Word32)
                -> io ()
cmdDispatchBase commandBuffer
                  baseGroupX
                  baseGroupY
                  baseGroupZ
                  groupCountX
                  groupCountY
                  groupCountZ = liftIO $ do
  let vkCmdDispatchBasePtr = pVkCmdDispatchBase (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdDispatchBasePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchBase is null" Nothing Nothing
  let vkCmdDispatchBase' = mkVkCmdDispatchBase vkCmdDispatchBasePtr
  traceAroundEvent "vkCmdDispatchBase" (vkCmdDispatchBase'
                                          (commandBufferHandle (commandBuffer))
                                          (baseGroupX)
                                          (baseGroupY)
                                          (baseGroupZ)
                                          (groupCountX)
                                          (groupCountY)
                                          (groupCountZ))
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE"
pattern PIPELINE_CREATE_DISPATCH_BASE = PIPELINE_CREATE_DISPATCH_BASE_BIT


-- | VkMemoryAllocateFlagsInfo - Structure controlling how many instances of
-- memory will be allocated
--
-- = Description
--
-- If
-- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
-- is not set, the number of instances allocated depends on whether
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set in the memory heap. If
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set, then memory is allocated for every physical device in the
-- logical device (as if @deviceMask@ has bits set for all device indices).
-- If
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is not set, then a single instance of memory is allocated (as if
-- @deviceMask@ is set to one).
--
-- On some implementations, allocations from a multi-instance heap /may/
-- consume memory on all physical devices even if the @deviceMask@ excludes
-- some devices. If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties'::@subsetAllocation@
-- is 'Vulkan.Core10.FundamentalTypes.TRUE', then memory is only consumed
-- for the devices in the device mask.
--
-- In practice, most allocations on a multi-instance heap will be allocated
-- across all physical devices. Unicast allocation support is an optional
-- optimization for a minority of allocations.
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-deviceMask-00675# If
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ be a valid device mask
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-deviceMask-00676# If
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ not be zero
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-flags-10760# If the allocation is
--     performing a memory import operation, then @flags@ /must/ not
--     contain
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_ZERO_INITIALIZE_BIT_EXT'
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-flags-10761# If the allocation uses
--     protected memory, then @flags@ /must/ not contain
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_ZERO_INITIALIZE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO'
--
-- -   #VUID-VkMemoryAllocateFlagsInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits'
    -- controlling the allocation.
    flags :: MemoryAllocateFlags
  , -- | @deviceMask@ is a mask of physical devices in the logical device,
    -- indicating that memory /must/ be allocated on each device in the mask,
    -- if
    -- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
    -- is set in @flags@.
    deviceMask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryAllocateFlagsInfo)
#endif
deriving instance Show MemoryAllocateFlagsInfo

instance ToCStruct MemoryAllocateFlagsInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryAllocateFlagsInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryAllocateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryAllocateFlagsInfo where
  peekCStruct p = do
    flags <- peek @MemoryAllocateFlags ((p `plusPtr` 16 :: Ptr MemoryAllocateFlags))
    deviceMask <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ MemoryAllocateFlagsInfo
             flags deviceMask

instance Storable MemoryAllocateFlagsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryAllocateFlagsInfo where
  zero = MemoryAllocateFlagsInfo
           zero
           zero


-- | VkDeviceGroupRenderPassBeginInfo - Set the initial device mask and
-- render areas for a render pass instance
--
-- = Description
--
-- The @deviceMask@ serves several purposes. It is an upper bound on the
-- set of physical devices that /can/ be used during the render pass
-- instance, and the initial device mask when the render pass instance
-- begins. In addition, commands transitioning to the next subpass in a
-- render pass instance and commands ending the render pass instance, and,
-- accordingly render pass
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-load-operations load>,
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-store-operations store>,
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-resolve-operations multisample resolve>
-- operations and subpass dependencies corresponding to the render pass
-- instance, are executed on the physical devices included in the device
-- mask provided here.
--
-- If @deviceRenderAreaCount@ is not zero, then the elements of
-- @pDeviceRenderAreas@ override the value of
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- and provide a render area specific to each physical device. These render
-- areas serve the same purpose as
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- including controlling the region of attachments that are cleared by
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' and that
-- are resolved into resolve attachments.
--
-- If this structure is not present, the render pass instance’s device mask
-- is the value of 'DeviceGroupCommandBufferBeginInfo'::@deviceMask@. If
-- this structure is not present or if @deviceRenderAreaCount@ is zero,
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- is used for all physical devices.
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-deviceMask-00905#
--     @deviceMask@ /must/ be a valid device mask value
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-deviceMask-00906#
--     @deviceMask@ /must/ not be zero
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-deviceMask-00907#
--     @deviceMask@ /must/ be a subset of the command buffer’s initial
--     device mask
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-deviceRenderAreaCount-00908#
--     @deviceRenderAreaCount@ /must/ either be zero or equal to the number
--     of physical devices in the logical device
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-offset-06166# The @offset.x@
--     member of any element of @pDeviceRenderAreas@ /must/ be greater than
--     or equal to 0
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-offset-06167# The @offset.y@
--     member of any element of @pDeviceRenderAreas@ /must/ be greater than
--     or equal to 0
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-offset-06168# The sum of the
--     @offset.x@ and @extent.width@ members of any element of
--     @pDeviceRenderAreas@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxFramebufferWidth maxFramebufferWidth>
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-offset-06169# The sum of the
--     @offset.y@ and @extent.height@ members of any element of
--     @pDeviceRenderAreas@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxFramebufferHeight maxFramebufferHeight>
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-extent-08998# The
--     @extent.width@ member of any element of @pDeviceRenderAreas@ /must/
--     be greater than 0
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-extent-08999# The
--     @extent.height@ member of any element of @pDeviceRenderAreas@ /must/
--     be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO'
--
-- -   #VUID-VkDeviceGroupRenderPassBeginInfo-pDeviceRenderAreas-parameter#
--     If @deviceRenderAreaCount@ is not @0@, @pDeviceRenderAreas@ /must/
--     be a valid pointer to an array of @deviceRenderAreaCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- | @deviceMask@ is the device mask for the render pass instance.
    deviceMask :: Word32
  , -- | @pDeviceRenderAreas@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining the render
    -- area for each physical device.
    deviceRenderAreas :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupRenderPassBeginInfo)
#endif
deriving instance Show DeviceGroupRenderPassBeginInfo

instance ToCStruct DeviceGroupRenderPassBeginInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupRenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceRenderAreas)) :: Word32))
    pPDeviceRenderAreas' <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (deviceRenderAreas)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (deviceRenderAreas)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPDeviceRenderAreas')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGroupRenderPassBeginInfo where
  peekCStruct p = do
    deviceMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    deviceRenderAreaCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDeviceRenderAreas <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pDeviceRenderAreas' <- generateM (fromIntegral deviceRenderAreaCount) (\i -> peekCStruct @Rect2D ((pDeviceRenderAreas `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ DeviceGroupRenderPassBeginInfo
             deviceMask pDeviceRenderAreas'

instance Zero DeviceGroupRenderPassBeginInfo where
  zero = DeviceGroupRenderPassBeginInfo
           zero
           mempty


-- | VkDeviceGroupCommandBufferBeginInfo - Set the initial device mask for a
-- command buffer
--
-- = Description
--
-- The initial device mask also acts as an upper bound on the set of
-- devices that /can/ ever be in the device mask in the command buffer.
--
-- If this structure is not present, the initial value of a command
-- buffer’s device mask includes all physical devices in the logical device
-- when the command buffer begins recording.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- | @deviceMask@ is the initial value of the command buffer’s device mask.
    --
    -- #VUID-VkDeviceGroupCommandBufferBeginInfo-deviceMask-00106# @deviceMask@
    -- /must/ be a valid device mask value
    --
    -- #VUID-VkDeviceGroupCommandBufferBeginInfo-deviceMask-00107# @deviceMask@
    -- /must/ not be zero
    deviceMask :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupCommandBufferBeginInfo)
#endif
deriving instance Show DeviceGroupCommandBufferBeginInfo

instance ToCStruct DeviceGroupCommandBufferBeginInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupCommandBufferBeginInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGroupCommandBufferBeginInfo where
  peekCStruct p = do
    deviceMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DeviceGroupCommandBufferBeginInfo
             deviceMask

instance Storable DeviceGroupCommandBufferBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupCommandBufferBeginInfo where
  zero = DeviceGroupCommandBufferBeginInfo
           zero


-- | VkDeviceGroupSubmitInfo - Structure indicating which physical devices
-- execute semaphore operations and command buffers
--
-- = Description
--
-- If this structure is not present, semaphore operations and command
-- buffers execute on device index zero.
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceGroupSubmitInfo-waitSemaphoreCount-00082#
--     @waitSemaphoreCount@ /must/ equal
--     'Vulkan.Core10.Queue.SubmitInfo'::@waitSemaphoreCount@
--
-- -   #VUID-VkDeviceGroupSubmitInfo-commandBufferCount-00083#
--     @commandBufferCount@ /must/ equal
--     'Vulkan.Core10.Queue.SubmitInfo'::@commandBufferCount@
--
-- -   #VUID-VkDeviceGroupSubmitInfo-signalSemaphoreCount-00084#
--     @signalSemaphoreCount@ /must/ equal
--     'Vulkan.Core10.Queue.SubmitInfo'::@signalSemaphoreCount@
--
-- -   #VUID-VkDeviceGroupSubmitInfo-pWaitSemaphoreDeviceIndices-00085# All
--     elements of @pWaitSemaphoreDeviceIndices@ and
--     @pSignalSemaphoreDeviceIndices@ /must/ be valid device indices
--
-- -   #VUID-VkDeviceGroupSubmitInfo-pCommandBufferDeviceMasks-00086# All
--     elements of @pCommandBufferDeviceMasks@ /must/ be valid device masks
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceGroupSubmitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO'
--
-- -   #VUID-VkDeviceGroupSubmitInfo-pWaitSemaphoreDeviceIndices-parameter#
--     If @waitSemaphoreCount@ is not @0@, @pWaitSemaphoreDeviceIndices@
--     /must/ be a valid pointer to an array of @waitSemaphoreCount@
--     @uint32_t@ values
--
-- -   #VUID-VkDeviceGroupSubmitInfo-pCommandBufferDeviceMasks-parameter#
--     If @commandBufferCount@ is not @0@, @pCommandBufferDeviceMasks@
--     /must/ be a valid pointer to an array of @commandBufferCount@
--     @uint32_t@ values
--
-- -   #VUID-VkDeviceGroupSubmitInfo-pSignalSemaphoreDeviceIndices-parameter#
--     If @signalSemaphoreCount@ is not @0@,
--     @pSignalSemaphoreDeviceIndices@ /must/ be a valid pointer to an
--     array of @signalSemaphoreCount@ @uint32_t@ values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- | @pWaitSemaphoreDeviceIndices@ is a pointer to an array of
    -- @waitSemaphoreCount@ device indices indicating which physical device
    -- executes the semaphore wait operation in the corresponding element of
    -- 'Vulkan.Core10.Queue.SubmitInfo'::@pWaitSemaphores@.
    waitSemaphoreDeviceIndices :: Vector Word32
  , -- | @pCommandBufferDeviceMasks@ is a pointer to an array of
    -- @commandBufferCount@ device masks indicating which physical devices
    -- execute the command buffer in the corresponding element of
    -- 'Vulkan.Core10.Queue.SubmitInfo'::@pCommandBuffers@. A physical device
    -- executes the command buffer if the corresponding bit is set in the mask.
    commandBufferDeviceMasks :: Vector Word32
  , -- | @pSignalSemaphoreDeviceIndices@ is a pointer to an array of
    -- @signalSemaphoreCount@ device indices indicating which physical device
    -- executes the semaphore signal operation in the corresponding element of
    -- 'Vulkan.Core10.Queue.SubmitInfo'::@pSignalSemaphores@.
    signalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupSubmitInfo)
#endif
deriving instance Show DeviceGroupSubmitInfo

instance ToCStruct DeviceGroupSubmitInfo where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupSubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphoreDeviceIndices)) :: Word32))
    pPWaitSemaphoreDeviceIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (waitSemaphoreDeviceIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (waitSemaphoreDeviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPWaitSemaphoreDeviceIndices')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBufferDeviceMasks)) :: Word32))
    pPCommandBufferDeviceMasks' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (commandBufferDeviceMasks)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (commandBufferDeviceMasks)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPCommandBufferDeviceMasks')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphoreDeviceIndices)) :: Word32))
    pPSignalSemaphoreDeviceIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (signalSemaphoreDeviceIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (signalSemaphoreDeviceIndices)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPSignalSemaphoreDeviceIndices')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DeviceGroupSubmitInfo where
  peekCStruct p = do
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphoreDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pWaitSemaphoreDeviceIndices' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Word32 ((pWaitSemaphoreDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    commandBufferCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCommandBufferDeviceMasks <- peek @(Ptr Word32) ((p `plusPtr` 40 :: Ptr (Ptr Word32)))
    pCommandBufferDeviceMasks' <- generateM (fromIntegral commandBufferCount) (\i -> peek @Word32 ((pCommandBufferDeviceMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    signalSemaphoreCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pSignalSemaphoreDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 56 :: Ptr (Ptr Word32)))
    pSignalSemaphoreDeviceIndices' <- generateM (fromIntegral signalSemaphoreCount) (\i -> peek @Word32 ((pSignalSemaphoreDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ DeviceGroupSubmitInfo
             pWaitSemaphoreDeviceIndices'
             pCommandBufferDeviceMasks'
             pSignalSemaphoreDeviceIndices'

instance Zero DeviceGroupSubmitInfo where
  zero = DeviceGroupSubmitInfo
           mempty
           mempty
           mempty


-- | VkDeviceGroupBindSparseInfo - Structure indicating which instances are
-- bound
--
-- = Description
--
-- These device indices apply to all buffer and image memory binds included
-- in the batch pointing to this structure. The semaphore waits and signals
-- for the batch are executed only by the physical device specified by the
-- @resourceDeviceIndex@.
--
-- If this structure is not present, @resourceDeviceIndex@ and
-- @memoryDeviceIndex@ are assumed to be zero.
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceGroupBindSparseInfo-resourceDeviceIndex-01118#
--     @resourceDeviceIndex@ and @memoryDeviceIndex@ /must/ both be valid
--     device indices
--
-- -   #VUID-VkDeviceGroupBindSparseInfo-memoryDeviceIndex-01119# Each
--     memory allocation bound in this batch /must/ have allocated an
--     instance for @memoryDeviceIndex@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceGroupBindSparseInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- | @resourceDeviceIndex@ is a device index indicating which instance of the
    -- resource is bound.
    resourceDeviceIndex :: Word32
  , -- | @memoryDeviceIndex@ is a device index indicating which instance of the
    -- memory the resource instance is bound to.
    memoryDeviceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupBindSparseInfo)
#endif
deriving instance Show DeviceGroupBindSparseInfo

instance ToCStruct DeviceGroupBindSparseInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupBindSparseInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (resourceDeviceIndex)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (memoryDeviceIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceGroupBindSparseInfo where
  peekCStruct p = do
    resourceDeviceIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    memoryDeviceIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ DeviceGroupBindSparseInfo
             resourceDeviceIndex memoryDeviceIndex

instance Storable DeviceGroupBindSparseInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceGroupBindSparseInfo where
  zero = DeviceGroupBindSparseInfo
           zero
           zero

