{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_mesh_shader  ( cmdDrawMeshTasksNV
                                                     , cmdDrawMeshTasksIndirectNV
                                                     , cmdDrawMeshTasksIndirectCountNV
                                                     , PhysicalDeviceMeshShaderFeaturesNV(..)
                                                     , PhysicalDeviceMeshShaderPropertiesNV(..)
                                                     , DrawMeshTasksIndirectCommandNV(..)
                                                     , NV_MESH_SHADER_SPEC_VERSION
                                                     , pattern NV_MESH_SHADER_SPEC_VERSION
                                                     , NV_MESH_SHADER_EXTENSION_NAME
                                                     , pattern NV_MESH_SHADER_EXTENSION_NAME
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectCountNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectNV))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksNV))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksNV - Draw mesh task work items
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @taskCount@ is the number of local workgroups to dispatch in the X
--     dimension. Y and Z dimension are implicitly set to one.
--
-- -   @firstTask@ is the X component of the first workgroup ID.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @taskCount@ local workgroups is assembled.
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
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   @taskCount@ /must/ be less than or equal to
--     'PhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
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
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdDrawMeshTasksNV :: CommandBuffer -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()
cmdDrawMeshTasksNV commandBuffer taskCount firstTask = do
  let vkCmdDrawMeshTasksNV' = mkVkCmdDrawMeshTasksNV (pVkCmdDrawMeshTasksNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawMeshTasksNV' (commandBufferHandle (commandBuffer)) (taskCount) (firstTask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksIndirectNV - Issue an indirect mesh tasks draw into a
-- command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer containing
--     draw parameters.
--
-- -   @offset@ is the byte offset into
--     'Graphics.Vulkan.Core10.Handles.Buffer' where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'cmdDrawMeshTasksIndirectNV' behaves similarly to 'cmdDrawMeshTasksNV'
-- except that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from 'Graphics.Vulkan.Core10.Handles.Buffer' starting
-- at @offset@ and increasing by @stride@ bytes for each successive draw.
-- The parameters of each draw are encoded in an array of
-- 'DrawMeshTasksIndirectCommandNV' structures. If @drawCount@ is less than
-- or equal to one, @stride@ is ignored.
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
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('DrawMeshTasksIndirectCommandNV')
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of 'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ + @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be
--     less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Buffer', and
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdDrawMeshTasksIndirectNV :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
cmdDrawMeshTasksIndirectNV commandBuffer buffer offset drawCount stride = do
  let vkCmdDrawMeshTasksIndirectNV' = mkVkCmdDrawMeshTasksIndirectNV (pVkCmdDrawMeshTasksIndirectNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawMeshTasksIndirectNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectCountNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawMeshTasksIndirectCountNV - Perform an indirect mesh tasks draw
-- with the draw count sourced from a buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer containing
--     draw parameters.
--
-- -   @offset@ is the byte offset into
--     'Graphics.Vulkan.Core10.Handles.Buffer' where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'cmdDrawMeshTasksIndirectCountNV' behaves similarly to
-- 'cmdDrawMeshTasksIndirectNV' except that the draw count is read by the
-- device from a buffer during execution. The command will read an unsigned
-- 32-bit integer from @countBuffer@ located at @countBufferOffset@ and use
-- this as the draw count.
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
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @countBuffer@ /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   The count stored in @countBuffer@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to @sizeof@('DrawMeshTasksIndirectCommandNV')
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of 'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If the count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of 'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If the count stored in @countBuffer@ is greater than @1@, (@stride@
--     × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('DrawMeshTasksIndirectCommandNV')) /must/ be less than or
--     equal to the size of 'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @countBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.Buffer',
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and @countBuffer@
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdDrawMeshTasksIndirectCountNV :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
cmdDrawMeshTasksIndirectCountNV commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride = do
  let vkCmdDrawMeshTasksIndirectCountNV' = mkVkCmdDrawMeshTasksIndirectCountNV (pVkCmdDrawMeshTasksIndirectCountNV (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawMeshTasksIndirectCountNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (countBuffer) (countBufferOffset) (maxDrawCount) (stride)
  pure $ ()


-- | VkPhysicalDeviceMeshShaderFeaturesNV - Structure describing mesh shading
-- features that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceMeshShaderFeaturesNV' structure is included in the
-- @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
-- 'PhysicalDeviceMeshShaderFeaturesNV' /can/ also be included in @pNext@
-- chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- | @taskShader@ indicates whether the task shader stage is supported.
    taskShader :: Bool
  , -- | @meshShader@ indicates whether the mesh shader stage is supported.
    meshShader :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceMeshShaderFeaturesNV

instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (taskShader))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (meshShader))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV where
  peekCStruct p = do
    taskShader <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    meshShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceMeshShaderFeaturesNV
             (bool32ToBool taskShader) (bool32ToBool meshShader)

instance Storable PhysicalDeviceMeshShaderFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderFeaturesNV where
  zero = PhysicalDeviceMeshShaderFeaturesNV
           zero
           zero


-- | VkPhysicalDeviceMeshShaderPropertiesNV - Structure describing mesh
-- shading properties
--
-- = Members
--
-- The members of the 'PhysicalDeviceMeshShaderPropertiesNV' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceMeshShaderPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- | @maxDrawMeshTasksCount@ is the maximum number of local workgroups that
    -- /can/ be launched by a single draw mesh tasks command. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-mesh-shading>.
    maxDrawMeshTasksCount :: Word32
  , -- | @maxTaskWorkGroupInvocations@ is the maximum total number of task shader
    -- invocations in a single local workgroup. The product of the X, Y, and Z
    -- sizes, as specified by the @LocalSize@ execution mode in shader modules
    -- or by the object decorated by the @WorkgroupSize@ decoration, /must/ be
    -- less than or equal to this limit.
    maxTaskWorkGroupInvocations :: Word32
  , -- | @maxTaskWorkGroupSize@[3] is the maximum size of a local task workgroup.
    -- These three values represent the maximum local workgroup size in the X,
    -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes, as
    -- specified by the @LocalSize@ execution mode or by the object decorated
    -- by the @WorkgroupSize@ decoration in shader modules, /must/ be less than
    -- or equal to the corresponding limit.
    maxTaskWorkGroupSize :: (Word32, Word32, Word32)
  , -- | @maxTaskTotalMemorySize@ is the maximum number of bytes that the task
    -- shader can use in total for shared and output memory combined.
    maxTaskTotalMemorySize :: Word32
  , -- | @maxTaskOutputCount@ is the maximum number of output tasks a single task
    -- shader workgroup can emit.
    maxTaskOutputCount :: Word32
  , -- | @maxMeshWorkGroupInvocations@ is the maximum total number of mesh shader
    -- invocations in a single local workgroup. The product of the X, Y, and Z
    -- sizes, as specified by the @LocalSize@ execution mode in shader modules
    -- or by the object decorated by the @WorkgroupSize@ decoration, /must/ be
    -- less than or equal to this limit.
    maxMeshWorkGroupInvocations :: Word32
  , -- | @maxMeshWorkGroupSize@[3] is the maximum size of a local mesh workgroup.
    -- These three values represent the maximum local workgroup size in the X,
    -- Y, and Z dimensions, respectively. The @x@, @y@, and @z@ sizes, as
    -- specified by the @LocalSize@ execution mode or by the object decorated
    -- by the @WorkgroupSize@ decoration in shader modules, /must/ be less than
    -- or equal to the corresponding limit.
    maxMeshWorkGroupSize :: (Word32, Word32, Word32)
  , -- | @maxMeshTotalMemorySize@ is the maximum number of bytes that the mesh
    -- shader can use in total for shared and output memory combined.
    maxMeshTotalMemorySize :: Word32
  , -- | @maxMeshOutputVertices@ is the maximum number of vertices a mesh shader
    -- output can store.
    maxMeshOutputVertices :: Word32
  , -- | @maxMeshOutputPrimitives@ is the maximum number of primitives a mesh
    -- shader output can store.
    maxMeshOutputPrimitives :: Word32
  , -- | @maxMeshMultiviewViewCount@ is the maximum number of multi-view views a
    -- mesh shader can use.
    maxMeshMultiviewViewCount :: Word32
  , -- | @meshOutputPerVertexGranularity@ is the granularity with which mesh
    -- vertex outputs are allocated. The value can be used to compute the
    -- memory size used by the mesh shader, which must be less than or equal to
    -- @maxMeshTotalMemorySize@.
    meshOutputPerVertexGranularity :: Word32
  , -- | @meshOutputPerPrimitiveGranularity@ is the granularity with which mesh
    -- outputs qualified as per-primitive are allocated. The value can be used
    -- to compute the memory size used by the mesh shader, which must be less
    -- than or equal to @maxMeshTotalMemorySize@.
    meshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceMeshShaderPropertiesNV

instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDrawMeshTasksCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTaskWorkGroupInvocations)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    case (maxTaskWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTaskTotalMemorySize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTaskOutputCount)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxMeshWorkGroupInvocations)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    case (maxMeshWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxMeshTotalMemorySize)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxMeshOutputVertices)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxMeshOutputPrimitives)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxMeshMultiviewViewCount)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (meshOutputPerVertexGranularity)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (meshOutputPerPrimitiveGranularity)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV where
  peekCStruct p = do
    maxDrawMeshTasksCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTaskWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    let pmaxTaskWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 24 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    maxTaskWorkGroupSize0 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxTaskWorkGroupSize1 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxTaskWorkGroupSize2 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxTaskTotalMemorySize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTaskOutputCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxMeshWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    let pmaxMeshWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 48 :: Ptr (Data.Vector.Storable.Sized.Vector 3 Word32)))
    maxMeshWorkGroupSize0 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxMeshWorkGroupSize1 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxMeshWorkGroupSize2 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxMeshTotalMemorySize <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxMeshOutputVertices <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxMeshOutputPrimitives <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxMeshMultiviewViewCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    meshOutputPerVertexGranularity <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    meshOutputPerPrimitiveGranularity <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pure $ PhysicalDeviceMeshShaderPropertiesNV
             maxDrawMeshTasksCount maxTaskWorkGroupInvocations ((maxTaskWorkGroupSize0, maxTaskWorkGroupSize1, maxTaskWorkGroupSize2)) maxTaskTotalMemorySize maxTaskOutputCount maxMeshWorkGroupInvocations ((maxMeshWorkGroupSize0, maxMeshWorkGroupSize1, maxMeshWorkGroupSize2)) maxMeshTotalMemorySize maxMeshOutputVertices maxMeshOutputPrimitives maxMeshMultiviewViewCount meshOutputPerVertexGranularity meshOutputPerPrimitiveGranularity

instance Storable PhysicalDeviceMeshShaderPropertiesNV where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderPropertiesNV where
  zero = PhysicalDeviceMeshShaderPropertiesNV
           zero
           zero
           (zero, zero, zero)
           zero
           zero
           zero
           (zero, zero, zero)
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDrawMeshTasksIndirectCommandNV - Structure specifying a mesh tasks
-- draw indirect command
--
-- = Description
--
-- The members of 'DrawMeshTasksIndirectCommandNV' have the same meaning as
-- the similarly named parameters of 'cmdDrawMeshTasksNV'.
--
-- == Valid Usage
--
-- = See Also
--
-- 'cmdDrawMeshTasksIndirectNV'
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- | @taskCount@ /must/ be less than or equal to
    -- 'PhysicalDeviceMeshShaderPropertiesNV'::@maxDrawMeshTasksCount@
    taskCount :: Word32
  , -- | @firstTask@ is the X component of the first workgroup ID.
    firstTask :: Word32
  }
  deriving (Typeable)
deriving instance Show DrawMeshTasksIndirectCommandNV

instance ToCStruct DrawMeshTasksIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawMeshTasksIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (taskCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (firstTask)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawMeshTasksIndirectCommandNV where
  peekCStruct p = do
    taskCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    firstTask <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DrawMeshTasksIndirectCommandNV
             taskCount firstTask

instance Storable DrawMeshTasksIndirectCommandNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawMeshTasksIndirectCommandNV where
  zero = DrawMeshTasksIndirectCommandNV
           zero
           zero


type NV_MESH_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern NV_MESH_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_MESH_SHADER_SPEC_VERSION = 1


type NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern NV_MESH_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"
