{-# language CPP #-}
-- | = Name
--
-- VK_KHR_ray_tracing_maintenance1 - device extension
--
-- == VK_KHR_ray_tracing_maintenance1
--
-- [__Name String__]
--     @VK_KHR_ray_tracing_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     387
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.1
--
--     -   Requires @VK_KHR_acceleration_structure@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_ray_tracing_maintenance1] @dgkoch%0A*Here describe the issue or question you have about the VK_KHR_ray_tracing_maintenance1 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-21
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_ray_cull_mask.html SPV_KHR_ray_cull_mask>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_ray_cull_mask.txt GLSL_EXT_ray_cull_mask>
--
--     -   Interacts with @VK_KHR_ray_tracing_pipeline@
--
--     -   Interacts with @VK_KHR_synchronization2@
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Marius Bjorge, Arm
--
--     -   Tom Olson, Arm
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Yunpeng Zhu, Huawei
--
--     -   Andrew Garrard, Imagination
--
--     -   Dae Kim, Imagination
--
--     -   Joshua Barczak, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   Daniel Koch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- @VK_KHR_ray_tracing_maintenance1@ adds a collection of minor ray tracing
-- features, none of which would warrant an entire extension of their own.
--
-- The new features are as follows:
--
-- -   Adds support for the @SPV_KHR_ray_cull_mask@ SPIR-V extension in
--     Vulkan. This extension provides access to built-in @CullMaskKHR@
--     shader variable which contains the value of the @OpTrace*@
--     @Cull Mask@ parameter. This new shader variable is accessible in the
--     intersection, any-hit, closest-hit and miss shader stages.
--
-- -   Adds support for a new pipeline stage and access mask built on top
--     of @VK_KHR_synchronization2@:
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--         to specify execution of
--         <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-copying acceleration structure copy commands>
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'
--         to specify read access to a
--         <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shader-binding-table shader binding table>
--         in any shader pipeline stage
--
-- -   Adds two new acceleration structure query parameters:
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--         to query the acceleration structure size on the device timeline
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--         to query the number of bottom level acceleration structure
--         pointers for serialization
--
-- -   Adds an optional new indirect ray tracing dispatch command,
--     'cmdTraceRaysIndirect2KHR', which sources the shader binding table
--     parameters as well as the dispatch dimensions from the device. The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingPipelineTraceRaysIndirect2 rayTracingPipelineTraceRaysIndirect2>
--     feature indicates whether this functionality is supported.
--
-- == New Commands
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   'cmdTraceRaysIndirect2KHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingMaintenance1FeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   'TraceRaysIndirectCommand2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'
--
-- == New Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-cullmask CullMaskKHR>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-RayCullMaskKHR RayCullMaskKHR>
--
-- == Issues
--
-- None Yet!
--
-- == Version History
--
-- -   Revision 1, 2022-02-21 (Members of the Vulkan Ray Tracing TSG)
--
--     -   internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceRayTracingMaintenance1FeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_ray_tracing_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1  ( cmdTraceRaysIndirect2KHR
                                                          , TraceRaysIndirectCommand2KHR(..)
                                                          , PhysicalDeviceRayTracingMaintenance1FeaturesKHR(..)
                                                          , KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION
                                                          , pattern KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION
                                                          , KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME
                                                          , pattern KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME
                                                          ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysIndirect2KHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysIndirect2KHR
  :: FunPtr (Ptr CommandBuffer_T -> DeviceAddress -> IO ()) -> Ptr CommandBuffer_T -> DeviceAddress -> IO ()

-- | vkCmdTraceRaysIndirect2KHR - Initialize an indirect ray tracing dispatch
-- with indirect shader binding tables
--
-- = Description
--
-- 'cmdTraceRaysIndirect2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
-- except that shader binding table parameters as well as dispatch
-- dimensions are read by the device from @indirectDeviceAddress@ during
-- execution.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02697# For each set /n/ that
--     is statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02699# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02700# A valid pipeline /must/
--     be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02859# There /must/ not have
--     been any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02702# If the
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
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-uniformBuffers-06935# If any stage
--     of the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-storageBuffers-06936# If any stage
--     of the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-sparseImageInt64Atomics-04474# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-sparseImageInt64Atomics-04475# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-OpImageWeightedSampleQCOM-06978# If
--     any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-None-03429# Any shader group handle
--     referenced by this call /must/ have been queried from the currently
--     bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-maxPipelineRayRecursionDepth-03679#
--     This command /must/ not cause a shader call instruction to be
--     executed from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxPipelineRayRecursionDepth@ used to
--     create the bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-03635#
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-indirectDeviceAddress-03632# If the
--     buffer from which @indirectDeviceAddress@ was queried is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-indirectDeviceAddress-03633# The
--     buffer from which @indirectDeviceAddress@ was queried /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-indirectDeviceAddress-03634#
--     @indirectDeviceAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-indirectDeviceAddress-03636# All
--     device addresses between @indirectDeviceAddress@ and
--     @indirectDeviceAddress@ + @sizeof@('TraceRaysIndirectCommand2KHR') -
--     1 /must/ be in the buffer device address range of the same buffer
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-rayTracingPipelineTraceRaysIndirect2-03637#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipelineTraceRaysIndirect2 rayTracingPipelineTraceRaysIndirect2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-rayTracingMotionBlurPipelineTraceRaysIndirect-04951#
--     If the bound ray tracing pipeline was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--     'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.PhysicalDeviceRayTracingMotionBlurFeaturesNV'::@rayTracingMotionBlurPipelineTraceRaysIndirect@
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdTraceRaysIndirect2KHR-videocoding# This command /must/
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_maintenance1 VK_KHR_ray_tracing_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
cmdTraceRaysIndirect2KHR :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @indirectDeviceAddress@ is a buffer device address which is a pointer to
                            -- a 'TraceRaysIndirectCommand2KHR' structure containing the trace ray
                            -- parameters.
                            ("indirectDeviceAddress" ::: DeviceAddress)
                         -> io ()
cmdTraceRaysIndirect2KHR commandBuffer indirectDeviceAddress = liftIO $ do
  let vkCmdTraceRaysIndirect2KHRPtr = pVkCmdTraceRaysIndirect2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdTraceRaysIndirect2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysIndirect2KHR is null" Nothing Nothing
  let vkCmdTraceRaysIndirect2KHR' = mkVkCmdTraceRaysIndirect2KHR vkCmdTraceRaysIndirect2KHRPtr
  traceAroundEvent "vkCmdTraceRaysIndirect2KHR" (vkCmdTraceRaysIndirect2KHR'
                                                   (commandBufferHandle (commandBuffer))
                                                   (indirectDeviceAddress))
  pure $ ()


-- | VkTraceRaysIndirectCommand2KHR - Structure specifying the parameters of
-- an indirect trace ray command with indirect shader binding tables
--
-- = Description
--
-- The members of 'TraceRaysIndirectCommand2KHR' have the same meaning as
-- the similarly named parameters of
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR'.
--
-- Indirect shader binding table buffer parameters must satisfy the same
-- memory alignment and binding requirements as their counterparts in
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
-- and 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR'.
--
-- == Valid Usage
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pRayGenShaderBindingTable-03680#
--     If the buffer from which @raygenShaderRecordAddress@ was queried is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pRayGenShaderBindingTable-03681#
--     The buffer from which the @raygenShaderRecordAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pRayGenShaderBindingTable-03682#
--     @raygenShaderRecordAddress@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pMissShaderBindingTable-03683#
--     If the buffer from which @missShaderBindingTableAddress@ was queried
--     is non-sparse then it /must/ be bound completely and contiguously to
--     a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pMissShaderBindingTable-03684#
--     The buffer from which the @missShaderBindingTableAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pMissShaderBindingTable-03685#
--     @missShaderBindingTableAddress@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-03686#
--     @missShaderBindingTableStride@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-04029#
--     @missShaderBindingTableStride@ /must/ be less than or equal to
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pHitShaderBindingTable-03687#
--     If the buffer from which @hitShaderBindingTableAddress@ was queried
--     is non-sparse then it /must/ be bound completely and contiguously to
--     a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pHitShaderBindingTable-03688#
--     The buffer from which the @hitShaderBindingTableAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pHitShaderBindingTable-03689#
--     @hitShaderBindingTableAddress@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-03690#
--     @hitShaderBindingTableStride@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-04035#
--     @hitShaderBindingTableStride@ /must/ be less than or equal to
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pCallableShaderBindingTable-03691#
--     If the buffer from which @callableShaderBindingTableAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pCallableShaderBindingTable-03692#
--     The buffer from which the @callableShaderBindingTableAddress@ is
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pCallableShaderBindingTable-03693#
--     @callableShaderBindingTableAddress@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-03694#
--     @callableShaderBindingTableStride@ /must/ be a multiple of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-stride-04041#
--     @callableShaderBindingTableStride@ /must/ be less than or equal to
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03696# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     @hitShaderBindingTableAddress@ /must/ not be zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03697# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     @hitShaderBindingTableAddress@ /must/ not be zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03511# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--     the shader group handle identified by
--     @missShaderBindingTableAddress@ /must/ not be set to zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03512# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     entries in the table identified by @hitShaderBindingTableAddress@
--     accessed as a result of this command in order to execute an any-hit
--     shader /must/ not be set to zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03513# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     entries in the table identified by @hitShaderBindingTableAddress@
--     accessed as a result of this command in order to execute a closest
--     hit shader /must/ not be set to zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-flags-03514# If the currently
--     bound ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     entries in the table identified by @hitShaderBindingTableAddress@
--     accessed as a result of this command in order to execute an
--     intersection shader /must/ not be set to zero
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pHitShaderBindingTable-04735#
--     Any non-zero hit shader group entries in the table identified by
--     @hitShaderBindingTableAddress@ accessed by this call from a geometry
--     with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_TRIANGLES_KHR'
--     /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR'
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-pHitShaderBindingTable-04736#
--     Any non-zero hit shader group entries in the table identified by
--     @hitShaderBindingTableAddress@ accessed by this call from a geometry
--     with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_AABBS_KHR'
--     /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR'
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-width-03638# @width@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[0]
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-height-03639# @height@ /must/
--     be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[1]
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-depth-03640# @depth@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[2]
--
-- -   #VUID-VkTraceRaysIndirectCommand2KHR-width-03641# @width@ × @height@
--     × @depth@ /must/ be less than or equal to
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxRayDispatchInvocationCount@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_maintenance1 VK_KHR_ray_tracing_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data TraceRaysIndirectCommand2KHR = TraceRaysIndirectCommand2KHR
  { -- | @raygenShaderRecordAddress@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' of the ray generation
    -- shader binding table record used by this command.
    raygenShaderRecordAddress :: DeviceAddress
  , -- | @raygenShaderRecordSize@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes
    -- corresponding to the ray generation shader binding table record at base
    -- address @raygenShaderRecordAddress@.
    raygenShaderRecordSize :: DeviceSize
  , -- | @missShaderBindingTableAddress@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' of the first record in
    -- the miss shader binding table used by this command.
    missShaderBindingTableAddress :: DeviceAddress
  , -- | @missShaderBindingTableSize@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes
    -- corresponding to the total size of the miss shader binding table at
    -- @missShaderBindingTableAddress@ that may be accessed by this command.
    missShaderBindingTableSize :: DeviceSize
  , -- | @missShaderBindingTableStride@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes between
    -- records of the miss shader binding table.
    missShaderBindingTableStride :: DeviceSize
  , -- | @hitShaderBindingTableAddress@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' of the first record in
    -- the hit shader binding table used by this command.
    hitShaderBindingTableAddress :: DeviceAddress
  , -- | @hitShaderBindingTableSize@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes
    -- corresponding to the total size of the hit shader binding table at
    -- @hitShaderBindingTableAddress@ that may be accessed by this command.
    hitShaderBindingTableSize :: DeviceSize
  , -- | @hitShaderBindingTableStride@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes between
    -- records of the hit shader binding table.
    hitShaderBindingTableStride :: DeviceSize
  , -- | @callableShaderBindingTableAddress@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' of the first record in
    -- the callable shader binding table used by this command.
    callableShaderBindingTableAddress :: DeviceAddress
  , -- | @callableShaderBindingTableSize@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes
    -- corresponding to the total size of the callable shader binding table at
    -- @callableShaderBindingTableAddress@ that may be accessed by this
    -- command.
    callableShaderBindingTableSize :: DeviceSize
  , -- | @callableShaderBindingTableStride@ is a
    -- 'Vulkan.Core10.FundamentalTypes.DeviceSize' number of bytes between
    -- records of the callable shader binding table.
    callableShaderBindingTableStride :: DeviceSize
  , -- | @width@ is the width of the ray trace query dimensions.
    width :: Word32
  , -- | @height@ is height of the ray trace query dimensions.
    height :: Word32
  , -- | @depth@ is depth of the ray trace query dimensions.
    depth :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TraceRaysIndirectCommand2KHR)
#endif
deriving instance Show TraceRaysIndirectCommand2KHR

instance ToCStruct TraceRaysIndirectCommand2KHR where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TraceRaysIndirectCommand2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (raygenShaderRecordAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (raygenShaderRecordSize)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (missShaderBindingTableAddress)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (missShaderBindingTableSize)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (missShaderBindingTableStride)
    poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (hitShaderBindingTableAddress)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (hitShaderBindingTableSize)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (hitShaderBindingTableStride)
    poke ((p `plusPtr` 64 :: Ptr DeviceAddress)) (callableShaderBindingTableAddress)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (callableShaderBindingTableSize)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (callableShaderBindingTableStride)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (depth)
    f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 92 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 96 :: Ptr Word32)) (zero)
    f

instance FromCStruct TraceRaysIndirectCommand2KHR where
  peekCStruct p = do
    raygenShaderRecordAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    raygenShaderRecordSize <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    missShaderBindingTableAddress <- peek @DeviceAddress ((p `plusPtr` 16 :: Ptr DeviceAddress))
    missShaderBindingTableSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    missShaderBindingTableStride <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    hitShaderBindingTableAddress <- peek @DeviceAddress ((p `plusPtr` 40 :: Ptr DeviceAddress))
    hitShaderBindingTableSize <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    hitShaderBindingTableStride <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    callableShaderBindingTableAddress <- peek @DeviceAddress ((p `plusPtr` 64 :: Ptr DeviceAddress))
    callableShaderBindingTableSize <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    callableShaderBindingTableStride <- peek @DeviceSize ((p `plusPtr` 80 :: Ptr DeviceSize))
    width <- peek @Word32 ((p `plusPtr` 88 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 92 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 96 :: Ptr Word32))
    pure $ TraceRaysIndirectCommand2KHR
             raygenShaderRecordAddress
             raygenShaderRecordSize
             missShaderBindingTableAddress
             missShaderBindingTableSize
             missShaderBindingTableStride
             hitShaderBindingTableAddress
             hitShaderBindingTableSize
             hitShaderBindingTableStride
             callableShaderBindingTableAddress
             callableShaderBindingTableSize
             callableShaderBindingTableStride
             width
             height
             depth

instance Storable TraceRaysIndirectCommand2KHR where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TraceRaysIndirectCommand2KHR where
  zero = TraceRaysIndirectCommand2KHR
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
           zero
           zero


-- | VkPhysicalDeviceRayTracingMaintenance1FeaturesKHR - Structure describing
-- the ray tracing maintenance features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingMaintenance1FeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayTracingMaintenance1FeaturesKHR' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_maintenance1 VK_KHR_ray_tracing_maintenance1>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingMaintenance1FeaturesKHR = PhysicalDeviceRayTracingMaintenance1FeaturesKHR
  { -- | #features-rayTracingMaintenance1# @rayTracingMaintenance1@ indicates
    -- that the implementation supports the following:
    --
    -- -   The @CullMaskKHR@ SPIR-V builtin using the @SPV_KHR_ray_cull_mask@
    --     SPIR-V extension.
    --
    -- -   Additional acceleration structure property queries:
    --     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
    --     and
    --     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'.
    --
    -- -   A new access flag
    --     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'.
    --
    -- -   A new pipeline stage flag bit
    --     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
    rayTracingMaintenance1 :: Bool
  , -- | #features-rayTracingPipelineTraceRaysIndirect2#
    -- @rayTracingPipelineTraceRaysIndirect2@ indicates whether the
    -- implementation supports the extended indirect ray tracing command
    -- 'cmdTraceRaysIndirect2KHR'.
    rayTracingPipelineTraceRaysIndirect2 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingMaintenance1FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingMaintenance1FeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingMaintenance1FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingMaintenance1FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingMaintenance1))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineTraceRaysIndirect2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingMaintenance1FeaturesKHR where
  peekCStruct p = do
    rayTracingMaintenance1 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rayTracingPipelineTraceRaysIndirect2 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingMaintenance1FeaturesKHR
             (bool32ToBool rayTracingMaintenance1)
             (bool32ToBool rayTracingPipelineTraceRaysIndirect2)

instance Storable PhysicalDeviceRayTracingMaintenance1FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingMaintenance1FeaturesKHR where
  zero = PhysicalDeviceRayTracingMaintenance1FeaturesKHR
           zero
           zero


type KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION"
pattern KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION = 1


type KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_ray_tracing_maintenance1"

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME"
pattern KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_ray_tracing_maintenance1"

