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
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
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
getDeviceGroupPeerMemoryFeatures device heapIndex localDeviceIndex remoteDeviceIndex = liftIO . evalContT $ do
  let vkGetDeviceGroupPeerMemoryFeaturesPtr = pVkGetDeviceGroupPeerMemoryFeatures (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupPeerMemoryFeaturesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupPeerMemoryFeatures is null" Nothing Nothing
  let vkGetDeviceGroupPeerMemoryFeatures' = mkVkGetDeviceGroupPeerMemoryFeatures vkGetDeviceGroupPeerMemoryFeaturesPtr
  pPPeerMemoryFeatures <- ContT $ bracket (callocBytes @PeerMemoryFeatureFlags 4) free
  lift $ traceAroundEvent "vkGetDeviceGroupPeerMemoryFeatures" (vkGetDeviceGroupPeerMemoryFeatures' (deviceHandle (device)) (heapIndex) (localDeviceIndex) (remoteDeviceIndex) (pPPeerMemoryFeatures))
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
--     allocated from /must/ support graphics, compute, or transfer
--     operations
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
-- |                                                                                                                            |                                                                                                                        | Transfer                                                                                                              |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDeviceMask :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is command buffer whose current device mask is modified.
                    CommandBuffer
                 -> -- | @deviceMask@ is the new value of the current device mask.
                    ("deviceMask" ::: Word32)
                 -> io ()
cmdSetDeviceMask commandBuffer deviceMask = liftIO $ do
  let vkCmdSetDeviceMaskPtr = pVkCmdSetDeviceMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDeviceMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDeviceMask is null" Nothing Nothing
  let vkCmdSetDeviceMask' = mkVkCmdSetDeviceMask vkCmdSetDeviceMaskPtr
  traceAroundEvent "vkCmdSetDeviceMask" (vkCmdSetDeviceMask' (commandBufferHandle (commandBuffer)) (deviceMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDispatchBase - Dispatch compute work items
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
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchBase-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchBase-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchBase-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdDispatchBase-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchBase-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchBase-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchBase-None-02699# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDispatchBase-None-02700# A valid pipeline /must/ be bound
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDispatchBase-None-02859# There /must/ not have been any
--     calls to dynamic state setting commands for any state not specified
--     as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdDispatchBase-None-02702# If the
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
-- -   #VUID-vkCmdDispatchBase-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatchBase-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatchBase-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchBase-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchBase-commandBuffer-02707# If @commandBuffer@ is
--     an unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchBase-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatchBase-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatchBase-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatchBase-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatchBase-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatchBase-baseGroupX-00421# @baseGroupX@ /must/ be
--     less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   #VUID-vkCmdDispatchBase-baseGroupX-00422# @baseGroupX@ /must/ be
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
--     'PIPELINE_CREATE_DISPATCH_BASE' flag
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
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdDispatchBase-renderpass# This command /must/ only be
--     called outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
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
cmdDispatchBase commandBuffer baseGroupX baseGroupY baseGroupZ groupCountX groupCountY groupCountZ = liftIO $ do
  let vkCmdDispatchBasePtr = pVkCmdDispatchBase (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchBasePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchBase is null" Nothing Nothing
  let vkCmdDispatchBase' = mkVkCmdDispatchBase vkCmdDispatchBasePtr
  traceAroundEvent "vkCmdDispatchBase" (vkCmdDispatchBase' (commandBufferHandle (commandBuffer)) (baseGroupX) (baseGroupY) (baseGroupZ) (groupCountX) (groupCountY) (groupCountZ))
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
-- Note
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- begins. In addition, commands transitioning to the next subpass in the
-- render pass instance and commands ending the render pass instance, and,
-- accordingly render pass attachment load, store, and resolve operations
-- and subpass dependencies corresponding to the render pass instance, are
-- executed on the physical devices included in the device mask provided
-- here.
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
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupRenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceRenderAreas)) :: Word32))
    pPDeviceRenderAreas' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (deviceRenderAreas)) * 16) 4
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
-- buffer’s device mask is set to include all physical devices in the
-- logical device when the command buffer begins recording.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupSubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphoreDeviceIndices)) :: Word32))
    pPWaitSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (waitSemaphoreDeviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (waitSemaphoreDeviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPWaitSemaphoreDeviceIndices')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBufferDeviceMasks)) :: Word32))
    pPCommandBufferDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (commandBufferDeviceMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (commandBufferDeviceMasks)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPCommandBufferDeviceMasks')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphoreDeviceIndices)) :: Word32))
    pPSignalSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (signalSemaphoreDeviceIndices)) * 4) 4
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
             pWaitSemaphoreDeviceIndices' pCommandBufferDeviceMasks' pSignalSemaphoreDeviceIndices'

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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

