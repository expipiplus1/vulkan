{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group  ( getDeviceGroupPeerMemoryFeatures
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

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchBase))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetDeviceMask))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupPeerMemoryFeatures))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Graphics.Vulkan.Core10.CommandBufferBuilding (Rect2D)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_DISPATCH_BASE_BIT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO))
import Graphics.Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Graphics.Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(..))
import Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(..))
import Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPeerMemoryFeatures
  :: FunPtr (Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()) -> Ptr Device_T -> Word32 -> Word32 -> Word32 -> Ptr PeerMemoryFeatureFlags -> IO ()

-- | vkGetDeviceGroupPeerMemoryFeatures - Query supported peer memory
-- features of a device
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the memory.
--
-- -   @heapIndex@ is the index of the memory heap from which the memory is
--     allocated.
--
-- -   @localDeviceIndex@ is the device index of the physical device that
--     performs the memory access.
--
-- -   @remoteDeviceIndex@ is the device index of the physical device that
--     the memory is allocated for.
--
-- -   @pPeerMemoryFeatures@ is a pointer to a
--     'Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits.PeerMemoryFeatureFlags'
--     bitmask indicating which types of memory accesses are supported for
--     the combination of heap, local, and remote devices.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits.PeerMemoryFeatureFlags'
getDeviceGroupPeerMemoryFeatures :: Device -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> IO (("peerMemoryFeatures" ::: PeerMemoryFeatureFlags))
getDeviceGroupPeerMemoryFeatures device heapIndex localDeviceIndex remoteDeviceIndex = evalContT $ do
  let vkGetDeviceGroupPeerMemoryFeatures' = mkVkGetDeviceGroupPeerMemoryFeatures (pVkGetDeviceGroupPeerMemoryFeatures (deviceCmds (device :: Device)))
  pPPeerMemoryFeatures <- ContT $ bracket (callocBytes @PeerMemoryFeatureFlags 4) free
  lift $ vkGetDeviceGroupPeerMemoryFeatures' (deviceHandle (device)) (heapIndex) (localDeviceIndex) (remoteDeviceIndex) (pPPeerMemoryFeatures)
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
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is command buffer
--     whose current device mask is modified.
--
-- -   @deviceMask@ is the new value of the current device mask.
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
-- -   @deviceMask@ /must/ be a valid device mask value
--
-- -   @deviceMask@ /must/ not be zero
--
-- -   @deviceMask@ /must/ not include any set bits that were not in the
--     'DeviceGroupCommandBufferBeginInfo'::@deviceMask@ value when the
--     command buffer began recording.
--
-- -   If 'cmdSetDeviceMask' is called inside a render pass instance,
--     @deviceMask@ /must/ not include any set bits that were not in the
--     'DeviceGroupRenderPassBeginInfo'::@deviceMask@ value when the render
--     pass instance began recording.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, compute, or transfer operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetDeviceMask :: CommandBuffer -> ("deviceMask" ::: Word32) -> IO ()
cmdSetDeviceMask commandBuffer deviceMask = do
  let vkCmdSetDeviceMask' = mkVkCmdSetDeviceMask (pVkCmdSetDeviceMask (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetDeviceMask' (commandBufferHandle (commandBuffer)) (deviceMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDispatchBase - Dispatch compute work items
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @baseGroupX@ is the start value for the X component of
--     @WorkgroupId@.
--
-- -   @baseGroupY@ is the start value for the Y component of
--     @WorkgroupId@.
--
-- -   @baseGroupZ@ is the start value for the Z component of
--     @WorkgroupId@.
--
-- -   @groupCountX@ is the number of local workgroups to dispatch in the X
--     dimension.
--
-- -   @groupCountY@ is the number of local workgroups to dispatch in the Y
--     dimension.
--
-- -   @groupCountZ@ is the number of local workgroups to dispatch in the Z
--     dimension.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled, with @WorkgroupId@ values ranging from [@baseGroup*@,
-- @baseGroup*@ + @groupCount*@) in each component.
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatch' is equivalent
-- to @vkCmdDispatchBase(0,0,0,groupCountX,groupCountY,groupCountZ)@.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   @baseGroupX@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @baseGroupX@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @baseGroupZ@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- -   @groupCountX@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     minus @baseGroupX@
--
-- -   @groupCountY@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     minus @baseGroupY@
--
-- -   @groupCountZ@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     minus @baseGroupZ@
--
-- -   If any of @baseGroupX@, @baseGroupY@, or @baseGroupZ@ are not zero,
--     then the bound compute pipeline /must/ have been created with the
--     'PIPELINE_CREATE_DISPATCH_BASE' flag.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdDispatchBase :: CommandBuffer -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
cmdDispatchBase commandBuffer baseGroupX baseGroupY baseGroupZ groupCountX groupCountY groupCountZ = do
  let vkCmdDispatchBase' = mkVkCmdDispatchBase (pVkCmdDispatchBase (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDispatchBase' (commandBufferHandle (commandBuffer)) (baseGroupX) (baseGroupY) (baseGroupZ) (groupCountX) (groupCountY) (groupCountZ)
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE"
pattern PIPELINE_CREATE_DISPATCH_BASE = PIPELINE_CREATE_DISPATCH_BASE_BIT


-- | VkMemoryAllocateFlagsInfo - Structure controlling how many instances of
-- memory will be allocated
--
-- = Description
--
-- If
-- 'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
-- is not set, the number of instances allocated depends on whether
-- 'Graphics.Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set in the memory heap. If
-- 'Graphics.Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set, then memory is allocated for every physical device in the
-- logical device (as if @deviceMask@ has bits set for all device indices).
-- If
-- 'Graphics.Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is not set, then a single instance of memory is allocated (as if
-- @deviceMask@ is set to one).
--
-- On some implementations, allocations from a multi-instance heap /may/
-- consume memory on all physical devices even if the @deviceMask@ excludes
-- some devices. If
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties'::@subsetAllocation@
-- is 'Graphics.Vulkan.Core10.BaseType.TRUE', then memory is only consumed
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
-- -   If
--     'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ be a valid device mask.
--
-- -   If
--     'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ not be zero
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO'
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits'
    -- controlling the allocation.
    flags :: MemoryAllocateFlags
  , -- | @deviceMask@ is a mask of physical devices in the logical device,
    -- indicating that memory /must/ be allocated on each device in the mask,
    -- if
    -- 'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_MASK_BIT'
    -- is set in 'Graphics.Vulkan.Core10.BaseType.Flags'.
    deviceMask :: Word32
  }
  deriving (Typeable)
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
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- and provide a render area specific to each physical device. These render
-- areas serve the same purpose as
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- including controlling the region of attachments that are cleared by
-- 'Graphics.Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
-- and that are resolved into resolve attachments.
--
-- If this structure is not present, the render pass instance’s device mask
-- is the value of 'DeviceGroupCommandBufferBeginInfo'::@deviceMask@. If
-- this structure is not present or if @deviceRenderAreaCount@ is zero,
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- is used for all physical devices.
--
-- == Valid Usage
--
-- -   @deviceMask@ /must/ be a valid device mask value
--
-- -   @deviceMask@ /must/ not be zero
--
-- -   @deviceMask@ /must/ be a subset of the command buffer’s initial
--     device mask
--
-- -   @deviceRenderAreaCount@ /must/ either be zero or equal to the number
--     of physical devices in the logical device.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO'
--
-- -   If @deviceRenderAreaCount@ is not @0@, @pDeviceRenderAreas@ /must/
--     be a valid pointer to an array of @deviceRenderAreaCount@
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- | @deviceMask@ is the device mask for the render pass instance.
    deviceMask :: Word32
  , -- | @pDeviceRenderAreas@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
    -- defining the render area for each physical device.
    deviceRenderAreas :: Vector Rect2D
  }
  deriving (Typeable)
deriving instance Show DeviceGroupRenderPassBeginInfo

instance ToCStruct DeviceGroupRenderPassBeginInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupRenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (deviceMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceRenderAreas)) :: Word32))
    pPDeviceRenderAreas' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (deviceRenderAreas)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDeviceRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (deviceRenderAreas)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPDeviceRenderAreas')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pPDeviceRenderAreas' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDeviceRenderAreas' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPDeviceRenderAreas')
    lift $ f

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
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- | @deviceMask@ /must/ not be zero
    deviceMask :: Word32 }
  deriving (Typeable)
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
-- -   @waitSemaphoreCount@ /must/ equal
--     'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@waitSemaphoreCount@
--
-- -   @commandBufferCount@ /must/ equal
--     'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@commandBufferCount@
--
-- -   @signalSemaphoreCount@ /must/ equal
--     'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@signalSemaphoreCount@
--
-- -   All elements of @pWaitSemaphoreDeviceIndices@ and
--     @pSignalSemaphoreDeviceIndices@ /must/ be valid device indices
--
-- -   All elements of @pCommandBufferDeviceMasks@ /must/ be valid device
--     masks
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO'
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphoreDeviceIndices@
--     /must/ be a valid pointer to an array of @waitSemaphoreCount@
--     @uint32_t@ values
--
-- -   If @commandBufferCount@ is not @0@, @pCommandBufferDeviceMasks@
--     /must/ be a valid pointer to an array of @commandBufferCount@
--     @uint32_t@ values
--
-- -   If @signalSemaphoreCount@ is not @0@,
--     @pSignalSemaphoreDeviceIndices@ /must/ be a valid pointer to an
--     array of @signalSemaphoreCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- | @pWaitSemaphoreDeviceIndices@ is a pointer to an array of
    -- @waitSemaphoreCount@ device indices indicating which physical device
    -- executes the semaphore wait operation in the corresponding element of
    -- 'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@pWaitSemaphores@.
    waitSemaphoreDeviceIndices :: Vector Word32
  , -- | @pCommandBufferDeviceMasks@ is a pointer to an array of
    -- @commandBufferCount@ device masks indicating which physical devices
    -- execute the command buffer in the corresponding element of
    -- 'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@pCommandBuffers@. A physical
    -- device executes the command buffer if the corresponding bit is set in
    -- the mask.
    commandBufferDeviceMasks :: Vector Word32
  , -- | @pSignalSemaphoreDeviceIndices@ is a pointer to an array of
    -- @signalSemaphoreCount@ device indices indicating which physical device
    -- executes the semaphore signal operation in the corresponding element of
    -- 'Graphics.Vulkan.Core10.Queue.SubmitInfo'::@pSignalSemaphores@.
    signalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Typeable)
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPWaitSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPWaitSemaphoreDeviceIndices')
    pPCommandBufferDeviceMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferDeviceMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word32))) (pPCommandBufferDeviceMasks')
    pPSignalSemaphoreDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPSignalSemaphoreDeviceIndices')
    lift $ f

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
-- -   @resourceDeviceIndex@ and @memoryDeviceIndex@ /must/ both be valid
--     device indices.
--
-- -   Each memory allocation bound in this batch /must/ have allocated an
--     instance for @memoryDeviceIndex@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- | @resourceDeviceIndex@ is a device index indicating which instance of the
    -- resource is bound.
    resourceDeviceIndex :: Word32
  , -- | @memoryDeviceIndex@ is a device index indicating which instance of the
    -- memory the resource instance is bound to.
    memoryDeviceIndex :: Word32
  }
  deriving (Typeable)
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

