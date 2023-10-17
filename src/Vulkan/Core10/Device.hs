{-# language CPP #-}
-- No documentation found for Chapter "Device"
module Vulkan.Core10.Device  ( createDevice
                             , withDevice
                             , destroyDevice
                             , DeviceQueueCreateInfo(..)
                             , DeviceCreateInfo(..)
                             , Device(..)
                             , DeviceCreateFlags(..)
                             , DeviceQueueCreateFlagBits(..)
                             , DeviceQueueCreateFlags
                             ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
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
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Dynamic (initDeviceCmds)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDevice))
import Vulkan.Core10.Enums.DeviceCreateFlags (DeviceCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceDeviceMemoryReportCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (DeviceDiagnosticsConfigCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_memory_overallocation_behavior (DeviceMemoryOverallocationCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (DevicePrivateDataCreateInfo)
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_global_priority (DeviceQueueGlobalPriorityCreateInfoKHR)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDevice))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_4444_formats (PhysicalDevice4444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (PhysicalDeviceASTCDecodeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructureFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_address_binding_report (PhysicalDeviceAddressBindingReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_SEC_amigo_profiling (PhysicalDeviceAmigoProfilingFeaturesSEC)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout (PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_border_color_swizzle (PhysicalDeviceBorderColorSwizzleFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader (PhysicalDeviceClusterCullingShaderFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_color_write_enable (PhysicalDeviceColorWriteEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_copy_memory_indirect (PhysicalDeviceCopyMemoryIndirectFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clamp_zero_one (PhysicalDeviceDepthClampZeroOneFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_control (PhysicalDeviceDepthClipControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (PhysicalDeviceDescriptorBufferFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (PhysicalDeviceDiagnosticsConfigFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_displacement_micromap (PhysicalDeviceDisplacementMicromapFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (PhysicalDeviceExtendedDynamicState2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (PhysicalDeviceExtendedDynamicState3FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (PhysicalDeviceExternalMemoryRDMAFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (PhysicalDeviceFaultFeaturesEXT)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_global_priority (PhysicalDeviceGlobalPriorityQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_graphics_pipeline_library (PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_2d_view_of_3d (PhysicalDeviceImage2DViewOf3DFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (PhysicalDeviceImageCompressionControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control_swapchain (PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing (PhysicalDeviceImageProcessingFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d (PhysicalDeviceImageSlicedViewOf3DFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_view_min_lod (PhysicalDeviceImageViewMinLodFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_inherited_viewport_scissor (PhysicalDeviceInheritedViewportScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_invocation_mask (PhysicalDeviceInvocationMaskFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_legacy_dithering (PhysicalDeviceLegacyDitheringFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_linear_color_attachment (PhysicalDeviceLinearColorAttachmentFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (PhysicalDeviceMaintenance4Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_memory_decompression (PhysicalDeviceMemoryDecompressionFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mesh_shader (PhysicalDeviceMeshShaderFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (PhysicalDeviceMultiDrawFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas (PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports (PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_non_seamless_cube_map (PhysicalDeviceNonSeamlessCubeMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (PhysicalDeviceOpacityMicromapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (PhysicalDeviceOpticalFlowFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pageable_device_local_memory (PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_library_group_handles (PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_properties (PhysicalDevicePipelinePropertiesFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_protected_access (PhysicalDevicePipelineProtectedAccessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_robustness (PhysicalDevicePipelineRobustnessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_present_barrier (PhysicalDevicePresentBarrierFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_id (PhysicalDevicePresentIdFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_wait (PhysicalDevicePresentWaitFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_primitive_topology_list_restart (PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_primitives_generated_query (PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (PhysicalDevicePrivateDataFeatures)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PhysicalDeviceProvokingVertexFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_rgba10x6_formats (PhysicalDeviceRGBA10X6FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder (PhysicalDeviceRayTracingInvocationReorderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1 (PhysicalDeviceRayTracingMaintenance1FeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (PhysicalDeviceRayTracingMotionBlurFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float2 (PhysicalDeviceShaderAtomicFloat2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_shader_core_builtins (PhysicalDeviceShaderCoreBuiltinsFeaturesARM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests (PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (PhysicalDeviceShaderModuleIdentifierFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_object (PhysicalDeviceShaderObjectFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow (PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation (PhysicalDeviceShaderTerminateInvocationFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_tile_image (PhysicalDeviceShaderTileImageFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (PhysicalDeviceSubpassMergeFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (PhysicalDeviceSubpassShadingFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (PhysicalDeviceSwapchainMaintenance1FeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (PhysicalDeviceSynchronization2Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_tile_properties (PhysicalDeviceTilePropertiesFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (PhysicalDeviceVertexInputDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Vulkan.Core13 (PhysicalDeviceVulkan13Features)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats (PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory (PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Enums.DeviceCreateFlags (DeviceCreateFlags(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlagBits(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDevice
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct DeviceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct DeviceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result

-- | vkCreateDevice - Create a new device instance
--
-- = Description
--
-- 'createDevice' verifies that extensions and features requested in the
-- @ppEnabledExtensionNames@ and @pEnabledFeatures@ members of
-- @pCreateInfo@, respectively, are supported by the implementation. If any
-- requested extension is not supported, 'createDevice' /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'. If any
-- requested feature is not supported, 'createDevice' /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'. Support for
-- extensions /can/ be checked before creating a device by querying
-- 'Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties'.
-- Support for features /can/ similarly be checked by querying
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFeatures'.
--
-- After verifying and enabling the extensions the
-- 'Vulkan.Core10.Handles.Device' object is created and returned to the
-- application.
--
-- Multiple logical devices /can/ be created from the same physical device.
-- Logical device creation /may/ fail due to lack of device-specific
-- resources (in addition to other errors). If that occurs, 'createDevice'
-- will return 'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateDevice-ppEnabledExtensionNames-01387# All
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required device extensions>
--     for each extension in the
--     'DeviceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also be
--     present in that list
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDevice-physicalDevice-parameter# @physicalDevice@
--     /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkCreateDevice-pCreateInfo-parameter# @pCreateInfo@ /must/ be
--     a valid pointer to a valid 'DeviceCreateInfo' structure
--
-- -   #VUID-vkCreateDevice-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateDevice-pDevice-parameter# @pDevice@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Device' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'DeviceCreateInfo',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
createDevice :: forall a io
              . (Extendss DeviceCreateInfo a, PokeChain a, MonadIO io)
             => -- | @physicalDevice@ /must/ be one of the device handles returned from a
                -- call to 'Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices'
                -- (see
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-physical-device-enumeration Physical Device Enumeration>).
                PhysicalDevice
             -> -- | @pCreateInfo@ is a pointer to a 'DeviceCreateInfo' structure containing
                -- information about how to create the device.
                (DeviceCreateInfo a)
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io (Device)
createDevice physicalDevice createInfo allocator = liftIO . evalContT $ do
  let cmds = case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds
  let vkCreateDevicePtr = pVkCreateDevice cmds
  lift $ unless (vkCreateDevicePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDevice is null" Nothing Nothing
  let vkCreateDevice' = mkVkCreateDevice vkCreateDevicePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDevice <- ContT $ bracket (callocBytes @(Ptr Device_T) 8) free
  r <- lift $ traceAroundEvent "vkCreateDevice" (vkCreateDevice'
                                                   (physicalDeviceHandle (physicalDevice))
                                                   (forgetExtensions pCreateInfo)
                                                   pAllocator
                                                   (pPDevice))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDevice <- lift $ peek @(Ptr Device_T) pPDevice
  pDevice' <- lift $ (\h -> Device h <$> initDeviceCmds cmds h) pDevice
  pure $ (pDevice')

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDevice' and 'destroyDevice'
--
-- To ensure that 'destroyDevice' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDevice :: forall a io r . (Extendss DeviceCreateInfo a, PokeChain a, MonadIO io) => PhysicalDevice -> DeviceCreateInfo a -> Maybe AllocationCallbacks -> (io Device -> (Device -> io ()) -> r) -> r
withDevice physicalDevice pCreateInfo pAllocator b =
  b (createDevice physicalDevice pCreateInfo pAllocator)
    (\(o0) -> destroyDevice o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDevice
  :: FunPtr (Ptr Device_T -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDevice - Destroy a logical device
--
-- = Description
--
-- To ensure that no work is active on the device,
-- 'Vulkan.Core10.Queue.deviceWaitIdle' /can/ be used to gate the
-- destruction of the device. Prior to destroying a device, an application
-- is responsible for destroying\/freeing any Vulkan objects that were
-- created using that device as the first parameter of the corresponding
-- @vkCreate*@ or @vkAllocate*@ command.
--
-- Note
--
-- The lifetime of each of these objects is bound by the lifetime of the
-- 'Vulkan.Core10.Handles.Device' object. Therefore, to avoid resource
-- leaks, it is critical that an application explicitly free all of these
-- resources prior to calling 'destroyDevice'.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDevice-device-00378# All child objects created on
--     @device@ /must/ have been destroyed prior to destroying @device@
--
-- -   #VUID-vkDestroyDevice-device-00379# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @device@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyDevice-device-00380# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @device@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDevice-device-parameter# If @device@ is not @NULL@,
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyDevice-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- == Host Synchronization
--
-- -   Host access to @device@ /must/ be externally synchronized
--
-- -   Host access to all 'Vulkan.Core10.Handles.Queue' objects created
--     from @device@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
destroyDevice :: forall io
               . (MonadIO io)
              => -- | @device@ is the logical device to destroy.
                 Device
              -> -- | @pAllocator@ controls host memory allocation as described in the
                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                 -- chapter.
                 ("allocator" ::: Maybe AllocationCallbacks)
              -> io ()
destroyDevice device allocator = liftIO . evalContT $ do
  let vkDestroyDevicePtr = pVkDestroyDevice (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyDevicePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDevice is null" Nothing Nothing
  let vkDestroyDevice' = mkVkDestroyDevice vkDestroyDevicePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDevice" (vkDestroyDevice'
                                               (deviceHandle (device))
                                               pAllocator)
  pure $ ()


-- | VkDeviceQueueCreateInfo - Structure specifying parameters of a newly
-- created device queue
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueFamilyIndex-00381#
--     @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueCount-00382# @queueCount@ /must/
--     be less than or equal to the @queueCount@ member of the
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--     structure, as returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     in the @pQueueFamilyProperties@[queueFamilyIndex]
--
-- -   #VUID-VkDeviceQueueCreateInfo-pQueuePriorities-00383# Each element
--     of @pQueuePriorities@ /must/ be between @0.0@ and @1.0@ inclusive
--
-- -   #VUID-VkDeviceQueueCreateInfo-flags-02861# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature is not enabled, the
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
--     bit of @flags@ /must/ not be set
--
-- -   #VUID-VkDeviceQueueCreateInfo-flags-06449# If @flags@ includes
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT',
--     @queueFamilyIndex@ /must/ be the index of a queue family that
--     includes the 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_PROTECTED_BIT'
--     capability
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceQueueCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO'
--
-- -   #VUID-VkDeviceQueueCreateInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_global_priority.DeviceQueueGlobalPriorityCreateInfoKHR'
--
-- -   #VUID-VkDeviceQueueCreateInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDeviceQueueCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits'
--     values
--
-- -   #VUID-VkDeviceQueueCreateInfo-pQueuePriorities-parameter#
--     @pQueuePriorities@ /must/ be a valid pointer to an array of
--     @queueCount@ @float@ values
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueCount-arraylength# @queueCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'DeviceCreateInfo',
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueCreateInfo (es :: [Type]) = DeviceQueueCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask indicating behavior of the queues.
    flags :: DeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ is an unsigned integer indicating the index of the
    -- queue family in which to create the queues on this device. This index
    -- corresponds to the index of an element of the @pQueueFamilyProperties@
    -- array that was returned by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'.
    queueFamilyIndex :: Word32
  , -- | @pQueuePriorities@ is a pointer to an array of @queueCount@ normalized
    -- floating point values, specifying priorities of work that will be
    -- submitted to each created queue. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-priority Queue Priority>
    -- for more information.
    queuePriorities :: Vector Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance Extensible DeviceQueueCreateInfo where
  extensibleTypeName = "DeviceQueueCreateInfo"
  setNext DeviceQueueCreateInfo{..} next' = DeviceQueueCreateInfo{next = next', ..}
  getNext DeviceQueueCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceQueueCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceQueueGlobalPriorityCreateInfoKHR = Just f
    | otherwise = Nothing

instance ( Extendss DeviceQueueCreateInfo es
         , PokeChain es ) => ToCStruct (DeviceQueueCreateInfo es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queuePriorities)) :: Word32))
    pPQueuePriorities' <- ContT $ allocaBytes @CFloat ((Data.Vector.length (queuePriorities)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueuePriorities' `plusPtr` (4 * (i)) :: Ptr CFloat) (CFloat (e))) (queuePriorities)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CFloat))) (pPQueuePriorities')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss DeviceQueueCreateInfo es
         , PeekChain es ) => FromCStruct (DeviceQueueCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DeviceQueueCreateFlags ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    queueCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pQueuePriorities <- peek @(Ptr CFloat) ((p `plusPtr` 32 :: Ptr (Ptr CFloat)))
    pQueuePriorities' <- generateM (fromIntegral queueCount) (\i -> do
      pQueuePrioritiesElem <- peek @CFloat ((pQueuePriorities `advancePtrBytes` (4 * (i)) :: Ptr CFloat))
      pure $ coerce @CFloat @Float pQueuePrioritiesElem)
    pure $ DeviceQueueCreateInfo
             next flags queueFamilyIndex pQueuePriorities'

instance es ~ '[] => Zero (DeviceQueueCreateInfo es) where
  zero = DeviceQueueCreateInfo
           ()
           zero
           zero
           mempty


-- | VkDeviceCreateInfo - Structure specifying parameters of a newly created
-- device
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceCreateInfo-queueFamilyIndex-02802# The
--     @queueFamilyIndex@ member of each element of @pQueueCreateInfos@
--     /must/ be unique within @pQueueCreateInfos@, except that two members
--     can share the same @queueFamilyIndex@ if one describes
--     protected-capable queues and one describes queues that are not
--     protected-capable
--
-- -   #VUID-VkDeviceCreateInfo-pQueueCreateInfos-06755# If multiple
--     elements of @pQueueCreateInfos@ share the same @queueFamilyIndex@,
--     the sum of their @queueCount@ members /must/ be less than or equal
--     to the @queueCount@ member of the
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--     structure, as returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     in the @pQueueFamilyProperties@[queueFamilyIndex]
--
-- -   #VUID-VkDeviceCreateInfo-pQueueCreateInfos-06654# If multiple
--     elements of @pQueueCreateInfos@ share the same @queueFamilyIndex@,
--     then all of such elements /must/ have the same global priority
--     level, which /can/ be specified explicitly by the including a
--     'Vulkan.Extensions.VK_KHR_global_priority.DeviceQueueGlobalPriorityCreateInfoKHR'
--     structure in the @pNext@ chain, or by the implicit default value
--
-- -   #VUID-VkDeviceCreateInfo-pNext-00373# If the @pNext@ chain includes
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--     structure, then @pEnabledFeatures@ /must/ be @NULL@
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-01840# If
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     advertises Vulkan 1.1 or later, @ppEnabledExtensionNames@ /must/ not
--     contain @VK_AMD_negative_viewport_height@
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-00374#
--     @ppEnabledExtensionNames@ /must/ not contain both
--     @VK_KHR_maintenance1@ and @VK_AMD_negative_viewport_height@
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-03328#
--     @ppEnabledExtensionNames@ /must/ not contain both
--     @VK_KHR_buffer_device_address@ and @VK_EXT_buffer_device_address@
--
-- -   #VUID-VkDeviceCreateInfo-pNext-04748# If the @pNext@ chain includes
--     a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure and
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@bufferDeviceAddress@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', @ppEnabledExtensionNames@
--     /must/ not contain @VK_EXT_buffer_device_address@
--
-- -   #VUID-VkDeviceCreateInfo-pNext-02829# If the @pNext@ chain includes
--     a 'Vulkan.Core12.PhysicalDeviceVulkan11Features' structure, then it
--     /must/ not include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-pNext-02830# If the @pNext@ chain includes
--     a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then it
--     /must/ not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-04476# If
--     @ppEnabledExtensionNames@ contains
--     @\"VK_KHR_shader_draw_parameters\"@ and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features'::@shaderDrawParameters@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-02831# If
--     @ppEnabledExtensionNames@ contains @\"VK_KHR_draw_indirect_count\"@
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@drawIndirectCount@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-02832# If
--     @ppEnabledExtensionNames@ contains
--     @\"VK_KHR_sampler_mirror_clamp_to_edge\"@ and the @pNext@ chain
--     includes a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerMirrorClampToEdge@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-02833# If
--     @ppEnabledExtensionNames@ contains @\"VK_EXT_descriptor_indexing\"@
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@descriptorIndexing@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-02834# If
--     @ppEnabledExtensionNames@ contains
--     @\"VK_EXT_sampler_filter_minmax\"@ and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerFilterMinmax@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-02835# If
--     @ppEnabledExtensionNames@ contains
--     @\"VK_EXT_shader_viewport_index_layer\"@ and the @pNext@ chain
--     includes a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputViewportIndex@
--     and
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputLayer@
--     /must/ both be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-pNext-06532# If the @pNext@ chain includes
--     a 'Vulkan.Core13.PhysicalDeviceVulkan13Features' structure, then it
--     /must/ not include a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PhysicalDeviceDynamicRenderingFeatures',
--     'Vulkan.Core13.Promoted_From_VK_EXT_image_robustness.PhysicalDeviceImageRobustnessFeatures',
--     'Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeatures',
--     'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.PhysicalDeviceMaintenance4Features',
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeatures',
--     'Vulkan.Core13.Promoted_From_VK_EXT_private_data.PhysicalDevicePrivateDataFeatures',
--     'Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeatures',
--     'Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product.PhysicalDeviceShaderIntegerDotProductFeatures',
--     'Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation.PhysicalDeviceShaderTerminateInvocationFeatures',
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeatures',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.PhysicalDeviceSynchronization2Features',
--     'Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeatures',
--     or
--     'Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory.PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-pProperties-04451# If the
--     @VK_KHR_portability_subset@ extension is included in @pProperties@
--     of
--     'Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties',
--     @ppEnabledExtensionNames@ /must/ include
--     @\"VK_KHR_portability_subset\"@
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04478# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04479# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04480# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04481# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04482# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04483# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is enabled, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-None-04896# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     is enabled,
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderImageInt64Atomics shaderImageInt64Atomics>
--     /must/ be enabled
--
-- -   #VUID-VkDeviceCreateInfo-None-04897# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseImageFloat32Atomics sparseImageFloat32Atomics>
--     is enabled,
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderImageFloat32Atomics shaderImageFloat32Atomics>
--     /must/ be enabled
--
-- -   #VUID-VkDeviceCreateInfo-None-04898# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseImageFloat32AtomicAdd sparseImageFloat32AtomicAdd>
--     is enabled,
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderImageFloat32AtomicAdd shaderImageFloat32AtomicAdd>
--     /must/ be enabled
--
-- -   #VUID-VkDeviceCreateInfo-sparseImageFloat32AtomicMinMax-04975# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseImageFloat32AtomicMinMax sparseImageFloat32AtomicMinMax>
--     is enabled,
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderImageFloat32AtomicMinMax shaderImageFloat32AtomicMinMax>
--     /must/ be enabled
--
-- -   #VUID-VkDeviceCreateInfo-None-08095# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBuffer descriptorBuffer>
--     is enabled, @ppEnabledExtensionNames@ /must/ not contain
--     @VK_AMD_shader_fragment_mask@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_CREATE_INFO'
--
-- -   #VUID-VkDeviceCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceDeviceMemoryReportCreateInfoEXT',
--     'Vulkan.Extensions.VK_NV_device_diagnostics_config.DeviceDiagnosticsConfigCreateInfoNV',
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
--     'Vulkan.Extensions.VK_AMD_memory_overallocation_behavior.DeviceMemoryOverallocationCreateInfoAMD',
--     'Vulkan.Core13.Promoted_From_VK_EXT_private_data.DevicePrivateDataCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Vulkan.Extensions.VK_EXT_4444_formats.PhysicalDevice4444FormatsFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructureFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_device_address_binding_report.PhysicalDeviceAddressBindingReportFeaturesEXT',
--     'Vulkan.Extensions.VK_SEC_amigo_profiling.PhysicalDeviceAmigoProfilingFeaturesSEC',
--     'Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout.PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_border_color_swizzle.PhysicalDeviceBorderColorSwizzleFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     'Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
--     'Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader.PhysicalDeviceClusterCullingShaderFeaturesHUAWEI',
--     'Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
--     'Vulkan.Extensions.VK_EXT_color_write_enable.PhysicalDeviceColorWriteEnableFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
--     'Vulkan.Extensions.VK_NV_copy_memory_indirect.PhysicalDeviceCopyMemoryIndirectFeaturesNV',
--     'Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_depth_clamp_zero_one.PhysicalDeviceDepthClampZeroOneFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_depth_clip_control.PhysicalDeviceDepthClipControlFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping.PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE',
--     'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_device_memory_report.PhysicalDeviceDeviceMemoryReportFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
--     'Vulkan.Extensions.VK_NV_displacement_micromap.PhysicalDeviceDisplacementMicromapFeaturesNV',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PhysicalDeviceDynamicRenderingFeatures',
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.PhysicalDeviceExtendedDynamicState2FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.PhysicalDeviceExtendedDynamicState3FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.PhysicalDeviceExtendedDynamicStateFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_external_memory_rdma.PhysicalDeviceExternalMemoryRDMAFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_device_fault.PhysicalDeviceFaultFeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
--     'Vulkan.Extensions.VK_QCOM_fragment_density_map_offset.PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM',
--     'Vulkan.Extensions.VK_KHR_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateFeaturesKHR',
--     'Vulkan.Extensions.VK_KHR_global_priority.PhysicalDeviceGlobalPriorityQueryFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Vulkan.Extensions.VK_EXT_image_2d_view_of_3d.PhysicalDeviceImage2DViewOf3DFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_image_compression_control.PhysicalDeviceImageCompressionControlFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_image_compression_control_swapchain.PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT',
--     'Vulkan.Extensions.VK_QCOM_image_processing.PhysicalDeviceImageProcessingFeaturesQCOM',
--     'Vulkan.Core13.Promoted_From_VK_EXT_image_robustness.PhysicalDeviceImageRobustnessFeatures',
--     'Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d.PhysicalDeviceImageSlicedViewOf3DFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_image_view_min_lod.PhysicalDeviceImageViewMinLodFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.PhysicalDeviceInheritedViewportScissorFeaturesNV',
--     'Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeatures',
--     'Vulkan.Extensions.VK_HUAWEI_invocation_mask.PhysicalDeviceInvocationMaskFeaturesHUAWEI',
--     'Vulkan.Extensions.VK_EXT_legacy_dithering.PhysicalDeviceLegacyDitheringFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_linear_color_attachment.PhysicalDeviceLinearColorAttachmentFeaturesNV',
--     'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.PhysicalDeviceMaintenance4Features',
--     'Vulkan.Extensions.VK_NV_memory_decompression.PhysicalDeviceMemoryDecompressionFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_mesh_shader.PhysicalDeviceMeshShaderFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_multi_draw.PhysicalDeviceMultiDrawFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas.PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM',
--     'Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports.PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM',
--     'Vulkan.Extensions.VK_EXT_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_non_seamless_cube_map.PhysicalDeviceNonSeamlessCubeMapFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.PhysicalDeviceOpacityMicromapFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_optical_flow.PhysicalDeviceOpticalFlowFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_pageable_device_local_memory.PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeatures',
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_pipeline_library_group_handles.PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_pipeline_properties.PhysicalDevicePipelinePropertiesFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_pipeline_protected_access.PhysicalDevicePipelineProtectedAccessFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PhysicalDevicePipelineRobustnessFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR',
--     'Vulkan.Extensions.VK_NV_present_barrier.PhysicalDevicePresentBarrierFeaturesNV',
--     'Vulkan.Extensions.VK_KHR_present_id.PhysicalDevicePresentIdFeaturesKHR',
--     'Vulkan.Extensions.VK_KHR_present_wait.PhysicalDevicePresentWaitFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_primitive_topology_list_restart.PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_primitives_generated_query.PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT',
--     'Vulkan.Core13.Promoted_From_VK_EXT_private_data.PhysicalDevicePrivateDataFeatures',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_rgba10x6_formats.PhysicalDeviceRGBA10X6FormatsFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access.PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_ray_query.PhysicalDeviceRayQueryFeaturesKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder.PhysicalDeviceRayTracingInvocationReorderFeaturesNV',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1.PhysicalDeviceRayTracingMaintenance1FeaturesKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.PhysicalDeviceRayTracingMotionBlurFeaturesNV',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelineFeaturesKHR',
--     'Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2FeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Vulkan.Extensions.VK_EXT_shader_atomic_float2.PhysicalDeviceShaderAtomicFloat2FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_shader_atomic_float.PhysicalDeviceShaderAtomicFloatFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
--     'Vulkan.Extensions.VK_ARM_shader_core_builtins.PhysicalDeviceShaderCoreBuiltinsFeaturesARM',
--     'Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
--     'Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests.PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Vulkan.Extensions.VK_EXT_shader_image_atomic_int64.PhysicalDeviceShaderImageAtomicInt64FeaturesEXT',
--     'Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
--     'Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product.PhysicalDeviceShaderIntegerDotProductFeatures',
--     'Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
--     'Vulkan.Extensions.VK_EXT_shader_module_identifier.PhysicalDeviceShaderModuleIdentifierFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_shader_object.PhysicalDeviceShaderObjectFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow.PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR',
--     'Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation.PhysicalDeviceShaderTerminateInvocationFeatures',
--     'Vulkan.Extensions.VK_EXT_shader_tile_image.PhysicalDeviceShaderTileImageFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeatures',
--     'Vulkan.Extensions.VK_EXT_subpass_merge_feedback.PhysicalDeviceSubpassMergeFeedbackFeaturesEXT',
--     'Vulkan.Extensions.VK_HUAWEI_subpass_shading.PhysicalDeviceSubpassShadingFeaturesHUAWEI',
--     'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.PhysicalDeviceSwapchainMaintenance1FeaturesEXT',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.PhysicalDeviceSynchronization2Features',
--     'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
--     'Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeatures',
--     'Vulkan.Extensions.VK_QCOM_tile_properties.PhysicalDeviceTilePropertiesFeaturesQCOM',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.PhysicalDeviceVertexInputDynamicStateFeaturesEXT',
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features',
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features',
--     'Vulkan.Core13.PhysicalDeviceVulkan13Features',
--     'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
--     'Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout.PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats.PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT',
--     or
--     'Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory.PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures'
--
-- -   #VUID-VkDeviceCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceDeviceMemoryReportCreateInfoEXT'
--     or
--     'Vulkan.Core13.Promoted_From_VK_EXT_private_data.DevicePrivateDataCreateInfo'
--
-- -   #VUID-VkDeviceCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-VkDeviceCreateInfo-pQueueCreateInfos-parameter#
--     @pQueueCreateInfos@ /must/ be a valid pointer to an array of
--     @queueCreateInfoCount@ valid 'DeviceQueueCreateInfo' structures
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledLayerNames-parameter# If
--     @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-parameter# If
--     @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@ /must/
--     be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- -   #VUID-VkDeviceCreateInfo-pEnabledFeatures-parameter# If
--     @pEnabledFeatures@ is not @NULL@, @pEnabledFeatures@ /must/ be a
--     valid pointer to a valid
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-queueCreateInfoCount-arraylength#
--     @queueCreateInfoCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.DeviceCreateFlags.DeviceCreateFlags',
-- 'DeviceQueueCreateInfo',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createDevice'
data DeviceCreateInfo (es :: [Type]) = DeviceCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: DeviceCreateFlags
  , -- | @pQueueCreateInfos@ is a pointer to an array of 'DeviceQueueCreateInfo'
    -- structures describing the queues that are requested to be created along
    -- with the logical device. Refer to the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-queue-creation Queue Creation>
    -- section below for further details.
    queueCreateInfos :: Vector (SomeStruct DeviceQueueCreateInfo)
  , -- | @ppEnabledLayerNames@ is deprecated and ignored. See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#extendingvulkan-layers-devicelayerdeprecation>.
    enabledLayerNames :: Vector ByteString
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
    -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
    -- names of extensions to enable for the created device. See the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#extendingvulkan-extensions>
    -- section for further details.
    enabledExtensionNames :: Vector ByteString
  , -- | @pEnabledFeatures@ is @NULL@ or a pointer to a
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures' structure
    -- containing boolean indicators of all the features to be enabled. Refer
    -- to the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features Features>
    -- section for further details.
    enabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceCreateInfo es)

instance Extensible DeviceCreateInfo where
  extensibleTypeName = "DeviceCreateInfo"
  setNext DeviceCreateInfo{..} next' = DeviceCreateInfo{next = next', ..}
  getNext DeviceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceShaderTileImageFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderObjectFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingInvocationReorderFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSwapchainMaintenance1FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderCoreBuiltinsFeaturesARM = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFaultFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceOpticalFlowFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAddressBindingReportFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthClampZeroOneFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAmigoProfilingFeaturesSEC = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTilePropertiesFeaturesQCOM = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageProcessingFeaturesQCOM = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineRobustnessFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceNonSeamlessCubeMapFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelinePropertiesFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDisplacementMicromapFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceOpacityMicromapFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubpassMergeFeedbackFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageCompressionControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderModuleIdentifierFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE = Just f
    | Just Refl <- eqT @e @PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLinearColorAttachmentFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageViewMinLodFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDynamicRenderingFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRGBA10X6FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingMotionBlurFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShaderBarycentricFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderIntegerDotProductFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorBufferFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProvokingVertexFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInheritedViewportScissorFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineProtectedAccessFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLegacyDitheringFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSynchronization2Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceColorWriteEnableFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExternalMemoryRDMAFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVertexInputDynamicStateFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthClipControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMutableDescriptorTypeFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageSlicedViewOf3DFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImage2DViewOf3DFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderTerminateInvocationFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceClusterCullingShaderFeaturesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubpassShadingFeaturesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDevice4444FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevicePortabilitySubsetFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageRobustnessFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRobustness2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures = Just f
    | Just Refl <- eqT @e @DeviceDiagnosticsConfigCreateInfoNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDiagnosticsConfigFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicState3FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicState2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicStateFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBorderColorSwizzleFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCustomBorderColorFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCoherentMemoryFeaturesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan13Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan12Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan11Features = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineCreationCacheControlFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLineRasterizationFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubgroupSizeControlFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTexelBufferAlignmentFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDemoteToHelperInvocationFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSeparateDepthStencilLayoutsFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShaderInterlockFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSMBuiltinsFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceIndexTypeUint8FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderClockFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCoverageReductionModeFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDevicePerformanceQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDevicePresentBarrierFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceYcbcrImageArraysFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCooperativeMatrixFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTextureCompressionASTCHDRFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImagelessFramebufferFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMemoryPriorityFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthClipEnableFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceUniformBufferStandardLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceScalarBlockLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMap2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapFeaturesEXT = Just f
    | Just Refl <- eqT @e @DeviceMemoryOverallocationCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingMaintenance1FeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingPipelineFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAccelerationStructureFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMeshShaderFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMeshShaderFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInvocationMaskFeaturesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShadingRateImageFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMemoryDecompressionFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCopyMemoryIndirectFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageFootprintFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceComputeShaderDerivativesFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCornerSampledImageFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExclusiveScissorFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRepresentativeFragmentTestFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTransformFeedbackFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceASTCDecodeFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVertexAttributeDivisorFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicFloat2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicFloatFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicInt64Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkanMemoryModelFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceConditionalRenderingFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevice8BitStorageFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTimelineSemaphoreFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorIndexingFeatures = Just f
    | Just Refl <- eqT @e @DeviceDeviceMemoryReportCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceMemoryReportFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceGlobalPriorityQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceHostQueryResetFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderFloat16Int8Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDrawParametersFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMaintenance4Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInlineUniformBlockFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiDrawFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBlendOperationAdvancedFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProtectedMemoryFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSamplerYcbcrConversionFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSubgroupExtendedTypesFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevice16BitStorageFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePresentWaitFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDevicePresentIdFeaturesKHR = Just f
    | Just Refl <- eqT @e @DeviceGroupDeviceCreateInfo = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVariablePointersFeatures = Just f
    | Just Refl <- eqT @e @(PhysicalDeviceFeatures2 '[]) = Just f
    | Just Refl <- eqT @e @PhysicalDevicePrivateDataFeatures = Just f
    | Just Refl <- eqT @e @DevicePrivateDataCreateInfo = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = Just f
    | otherwise = Nothing

instance ( Extendss DeviceCreateInfo es
         , PokeChain es ) => ToCStruct (DeviceCreateInfo es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueCreateInfos)) :: Word32))
    pPQueueCreateInfos' <- ContT $ allocaBytes @(DeviceQueueCreateInfo _) ((Data.Vector.length (queueCreateInfos)) * 40)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPQueueCreateInfos' `plusPtr` (40 * (i)) :: Ptr (DeviceQueueCreateInfo _))) (e) . ($ ())) (queueCreateInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo _)))) (pPQueueCreateInfos')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledLayerNames)) :: Word32))
    pPpEnabledLayerNames' <- ContT $ allocaBytes @(Ptr CChar) ((Data.Vector.length (enabledLayerNames)) * 8)
    Data.Vector.imapM_ (\i e -> do
      ppEnabledLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledLayerNames'') (enabledLayerNames)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledLayerNames')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledExtensionNames)) :: Word32))
    pPpEnabledExtensionNames' <- ContT $ allocaBytes @(Ptr CChar) ((Data.Vector.length (enabledExtensionNames)) * 8)
    Data.Vector.imapM_ (\i e -> do
      ppEnabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledExtensionNames'') (enabledExtensionNames)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledExtensionNames')
    pEnabledFeatures'' <- case (enabledFeatures) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr PhysicalDeviceFeatures))) pEnabledFeatures''
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss DeviceCreateInfo es
         , PeekChain es ) => FromCStruct (DeviceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DeviceCreateFlags ((p `plusPtr` 16 :: Ptr DeviceCreateFlags))
    queueCreateInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pQueueCreateInfos <- peek @(Ptr (DeviceQueueCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo _))))
    pQueueCreateInfos' <- generateM (fromIntegral queueCreateInfoCount) (\i -> peekSomeCStruct (forgetExtensions ((pQueueCreateInfos `advancePtrBytes` (40 * (i)) :: Ptr (DeviceQueueCreateInfo _)))))
    enabledLayerCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    ppEnabledLayerNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledLayerNames' <- generateM (fromIntegral enabledLayerCount) (\i -> packCString =<< peek ((ppEnabledLayerNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    enabledExtensionCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    ppEnabledExtensionNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledExtensionNames' <- generateM (fromIntegral enabledExtensionCount) (\i -> packCString =<< peek ((ppEnabledExtensionNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    pEnabledFeatures <- peek @(Ptr PhysicalDeviceFeatures) ((p `plusPtr` 64 :: Ptr (Ptr PhysicalDeviceFeatures)))
    pEnabledFeatures' <- maybePeek (\j -> peekCStruct @PhysicalDeviceFeatures (j)) pEnabledFeatures
    pure $ DeviceCreateInfo
             next
             flags
             pQueueCreateInfos'
             ppEnabledLayerNames'
             ppEnabledExtensionNames'
             pEnabledFeatures'

instance es ~ '[] => Zero (DeviceCreateInfo es) where
  zero = DeviceCreateInfo
           ()
           zero
           mempty
           mempty
           mempty
           Nothing

