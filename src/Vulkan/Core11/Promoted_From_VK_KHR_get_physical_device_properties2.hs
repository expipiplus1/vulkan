{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_get_physical_device_properties2"
module Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2  ( getPhysicalDeviceFeatures2
                                                                           , getPhysicalDeviceProperties2
                                                                           , getPhysicalDeviceFormatProperties2
                                                                           , getPhysicalDeviceImageFormatProperties2
                                                                           , getPhysicalDeviceQueueFamilyProperties2
                                                                           , getPhysicalDeviceMemoryProperties2
                                                                           , getPhysicalDeviceSparseImageFormatProperties2
                                                                           , PhysicalDeviceFeatures2(..)
                                                                           , PhysicalDeviceProperties2(..)
                                                                           , FormatProperties2(..)
                                                                           , ImageFormatProperties2(..)
                                                                           , PhysicalDeviceImageFormatInfo2(..)
                                                                           , QueueFamilyProperties2(..)
                                                                           , PhysicalDeviceMemoryProperties2(..)
                                                                           , SparseImageFormatProperties2(..)
                                                                           , PhysicalDeviceSparseImageFormatInfo2(..)
                                                                           , StructureType(..)
                                                                           ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferUsageANDROID)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesList2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesListEXT)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalImageFormatProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_filter_cubic (FilterCubicImageViewImageFormatPropertiesEXT)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.DeviceInitialization (FormatProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_format_feature_flags2 (FormatProperties3KHR)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import Vulkan.Core10.Enums.ImageType (ImageType)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFeatures2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFormatProperties2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceImageFormatProperties2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceMemoryProperties2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceProperties2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyProperties2))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSparseImageFormatProperties2))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_4444_formats (PhysicalDevice4444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (PhysicalDeviceASTCDecodeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructureFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructurePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_border_color_swizzle (PhysicalDeviceBorderColorSwizzleFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_color_write_enable (PhysicalDeviceColorWriteEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PhysicalDeviceConservativeRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (PhysicalDeviceDepthStencilResolveProperties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (PhysicalDeviceDiagnosticsConfigFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PhysicalDeviceDiscardRectanglePropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (PhysicalDeviceDriverProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_physical_device_drm (PhysicalDeviceDrmPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (PhysicalDeviceExtendedDynamicState2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalImageFormatInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (PhysicalDeviceExternalMemoryHostPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (PhysicalDeviceExternalMemoryRDMAFeaturesNV)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls (PhysicalDeviceFloatControlsProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRatePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_global_priority_query (PhysicalDeviceGlobalPriorityQueryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceIDProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (PhysicalDeviceImageDrmFormatModifierInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_filter_cubic (PhysicalDeviceImageViewImageFormatInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_inherited_viewport_scissor (PhysicalDeviceInheritedViewportScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_invocation_mask (PhysicalDeviceInvocationMaskFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (PhysicalDeviceMaintenance3Properties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance4 (PhysicalDeviceMaintenance4FeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance4 (PhysicalDeviceMaintenance4PropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_budget (PhysicalDeviceMemoryBudgetPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceMemoryProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (PhysicalDeviceMultiDrawFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (PhysicalDeviceMultiDrawPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesVALVE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pci_bus_info (PhysicalDevicePCIBusInfoPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pageable_device_local_memory (PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PhysicalDevicePointClippingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_id (PhysicalDevicePresentIdFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_wait (PhysicalDevicePresentWaitFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_primitive_topology_list_restart (PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (PhysicalDevicePrivateDataFeaturesEXT)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceProperties)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PhysicalDeviceProvokingVertexFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PhysicalDeviceProvokingVertexPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_push_descriptor (PhysicalDevicePushDescriptorPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_rgba10x6_formats (PhysicalDeviceRGBA10X6FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (PhysicalDeviceRayTracingMotionBlurFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelinePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (PhysicalDeviceRayTracingPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PhysicalDeviceSampleLocationsPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (PhysicalDeviceSamplerFilterMinmaxProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float2 (PhysicalDeviceShaderAtomicFloat2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties2 (PhysicalDeviceShaderCoreProperties2AMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties (PhysicalDeviceShaderCorePropertiesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsPropertiesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow (PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_terminate_invocation (PhysicalDeviceShaderTerminateInvocationFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImagePropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup (PhysicalDeviceSubgroupProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (PhysicalDeviceSubpassShadingFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (PhysicalDeviceSubpassShadingPropertiesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_synchronization2 (PhysicalDeviceSynchronization2FeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (PhysicalDeviceVertexInputDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Properties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats (PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_zero_initialize_workgroup_memory (PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_synchronization2 (QueueFamilyCheckpointProperties2NV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (QueueFamilyCheckpointPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_global_priority_query (QueueFamilyGlobalPriorityPropertiesEXT)
import Vulkan.Core10.DeviceInitialization (QueueFamilyProperties)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionImageFormatProperties)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.SparseResourceMemoryManagement (SparseImageFormatProperties)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_texture_gather_bias_lod (TextureLODGatherFormatPropertiesAMD)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceFeatures2) -> IO ()) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceFeatures2) -> IO ()

-- | vkGetPhysicalDeviceFeatures2 - Reports capabilities of a physical device
--
-- = Description
--
-- Each structure in @pFeatures@ and its @pNext@ chain contains members
-- corresponding to fine-grained features. 'getPhysicalDeviceFeatures2'
-- writes each member to a boolean value indicating whether that feature is
-- supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceFeatures2'
getPhysicalDeviceFeatures2 :: forall a io
                            . (Extendss PhysicalDeviceFeatures2 a, PokeChain a, PeekChain a, MonadIO io)
                           => -- | @physicalDevice@ is the physical device from which to query the
                              -- supported features.
                              --
                              -- #VUID-vkGetPhysicalDeviceFeatures2-physicalDevice-parameter#
                              -- @physicalDevice@ /must/ be a valid
                              -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                              PhysicalDevice
                           -> io (PhysicalDeviceFeatures2 a)
getPhysicalDeviceFeatures2 physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFeatures2Ptr = pVkGetPhysicalDeviceFeatures2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFeatures2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFeatures2 is null" Nothing Nothing
  let vkGetPhysicalDeviceFeatures2' = mkVkGetPhysicalDeviceFeatures2 vkGetPhysicalDeviceFeatures2Ptr
  pPFeatures <- ContT (withZeroCStruct @(PhysicalDeviceFeatures2 _))
  lift $ traceAroundEvent "vkGetPhysicalDeviceFeatures2" (vkGetPhysicalDeviceFeatures2' (physicalDeviceHandle (physicalDevice)) (forgetExtensions (pPFeatures)))
  pFeatures <- lift $ peekCStruct @(PhysicalDeviceFeatures2 _) pPFeatures
  pure $ (pFeatures)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceProperties2) -> IO ()) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceProperties2) -> IO ()

-- | vkGetPhysicalDeviceProperties2 - Returns properties of a physical device
--
-- = Description
--
-- Each structure in @pProperties@ and its @pNext@ chain contains members
-- corresponding to implementation-dependent properties, behaviors, or
-- limits. 'getPhysicalDeviceProperties2' fills in each member to specify
-- the corresponding value for the implementation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceProperties2'
getPhysicalDeviceProperties2 :: forall a io
                              . (Extendss PhysicalDeviceProperties2 a, PokeChain a, PeekChain a, MonadIO io)
                             => -- | @physicalDevice@ is the handle to the physical device whose properties
                                -- will be queried.
                                --
                                -- #VUID-vkGetPhysicalDeviceProperties2-physicalDevice-parameter#
                                -- @physicalDevice@ /must/ be a valid
                                -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                PhysicalDevice
                             -> io (PhysicalDeviceProperties2 a)
getPhysicalDeviceProperties2 physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceProperties2Ptr = pVkGetPhysicalDeviceProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceProperties2' = mkVkGetPhysicalDeviceProperties2 vkGetPhysicalDeviceProperties2Ptr
  pPProperties <- ContT (withZeroCStruct @(PhysicalDeviceProperties2 _))
  lift $ traceAroundEvent "vkGetPhysicalDeviceProperties2" (vkGetPhysicalDeviceProperties2' (physicalDeviceHandle (physicalDevice)) (forgetExtensions (pPProperties)))
  pProperties <- lift $ peekCStruct @(PhysicalDeviceProperties2 _) pPProperties
  pure $ (pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Format -> Ptr (SomeStruct FormatProperties2) -> IO ()) -> Ptr PhysicalDevice_T -> Format -> Ptr (SomeStruct FormatProperties2) -> IO ()

-- | vkGetPhysicalDeviceFormatProperties2 - Lists physical device’s format
-- capabilities
--
-- = Description
--
-- 'getPhysicalDeviceFormatProperties2' behaves similarly to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.Format.Format', 'FormatProperties2',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceFormatProperties2 :: forall a io
                                    . (Extendss FormatProperties2 a, PokeChain a, PeekChain a, MonadIO io)
                                   => -- | @physicalDevice@ is the physical device from which to query the format
                                      -- properties.
                                      --
                                      -- #VUID-vkGetPhysicalDeviceFormatProperties2-physicalDevice-parameter#
                                      -- @physicalDevice@ /must/ be a valid
                                      -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                      PhysicalDevice
                                   -> -- | @format@ is the format whose properties are queried.
                                      --
                                      -- #VUID-vkGetPhysicalDeviceFormatProperties2-format-parameter# @format@
                                      -- /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
                                      Format
                                   -> io (FormatProperties2 a)
getPhysicalDeviceFormatProperties2 physicalDevice format = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFormatProperties2Ptr = pVkGetPhysicalDeviceFormatProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFormatProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFormatProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceFormatProperties2' = mkVkGetPhysicalDeviceFormatProperties2 vkGetPhysicalDeviceFormatProperties2Ptr
  pPFormatProperties <- ContT (withZeroCStruct @(FormatProperties2 _))
  lift $ traceAroundEvent "vkGetPhysicalDeviceFormatProperties2" (vkGetPhysicalDeviceFormatProperties2' (physicalDeviceHandle (physicalDevice)) (format) (forgetExtensions (pPFormatProperties)))
  pFormatProperties <- lift $ peekCStruct @(FormatProperties2 _) pPFormatProperties
  pure $ (pFormatProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceImageFormatInfo2) -> Ptr (SomeStruct ImageFormatProperties2) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceImageFormatInfo2) -> Ptr (SomeStruct ImageFormatProperties2) -> IO Result

-- | vkGetPhysicalDeviceImageFormatProperties2 - Lists physical device’s
-- image format capabilities
--
-- = Description
--
-- 'getPhysicalDeviceImageFormatProperties2' behaves similarly to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceImageFormatProperties2-pNext-01868# If the
--     @pNext@ chain of @pImageFormatProperties@ includes a
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferUsageANDROID'
--     structure, the @pNext@ chain of @pImageFormatInfo@ /must/ include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo'
--     structure with @handleType@ set to
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceImageFormatProperties2-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceImageFormatProperties2-pImageFormatInfo-parameter#
--     @pImageFormatInfo@ /must/ be a valid pointer to a valid
--     'PhysicalDeviceImageFormatInfo2' structure
--
-- -   #VUID-vkGetPhysicalDeviceImageFormatProperties2-pImageFormatProperties-parameter#
--     @pImageFormatProperties@ /must/ be a valid pointer to a
--     'ImageFormatProperties2' structure
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'ImageFormatProperties2', 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceImageFormatInfo2'
getPhysicalDeviceImageFormatProperties2 :: forall a b io
                                         . (Extendss PhysicalDeviceImageFormatInfo2 a, PokeChain a, Extendss ImageFormatProperties2 b, PokeChain b, PeekChain b, MonadIO io)
                                        => -- | @physicalDevice@ is the physical device from which to query the image
                                           -- capabilities.
                                           PhysicalDevice
                                        -> -- | @pImageFormatInfo@ is a pointer to a 'PhysicalDeviceImageFormatInfo2'
                                           -- structure describing the parameters that would be consumed by
                                           -- 'Vulkan.Core10.Image.createImage'.
                                           (PhysicalDeviceImageFormatInfo2 a)
                                        -> io (ImageFormatProperties2 b)
getPhysicalDeviceImageFormatProperties2 physicalDevice imageFormatInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceImageFormatProperties2Ptr = pVkGetPhysicalDeviceImageFormatProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceImageFormatProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceImageFormatProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceImageFormatProperties2' = mkVkGetPhysicalDeviceImageFormatProperties2 vkGetPhysicalDeviceImageFormatProperties2Ptr
  pImageFormatInfo <- ContT $ withCStruct (imageFormatInfo)
  pPImageFormatProperties <- ContT (withZeroCStruct @(ImageFormatProperties2 _))
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceImageFormatProperties2" (vkGetPhysicalDeviceImageFormatProperties2' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pImageFormatInfo) (forgetExtensions (pPImageFormatProperties)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pImageFormatProperties <- lift $ peekCStruct @(ImageFormatProperties2 _) pPImageFormatProperties
  pure $ (pImageFormatProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr (SomeStruct QueueFamilyProperties2) -> IO ()) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr (SomeStruct QueueFamilyProperties2) -> IO ()

-- | vkGetPhysicalDeviceQueueFamilyProperties2 - Reports properties of the
-- queues of the specified physical device
--
-- = Description
--
-- 'getPhysicalDeviceQueueFamilyProperties2' behaves similarly to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties2-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties2-pQueueFamilyPropertyCount-parameter#
--     @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceQueueFamilyProperties2-pQueueFamilyProperties-parameter#
--     If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     'QueueFamilyProperties2' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'QueueFamilyProperties2'
getPhysicalDeviceQueueFamilyProperties2 :: forall a io
                                         . (Extendss QueueFamilyProperties2 a, PokeChain a, PeekChain a, MonadIO io)
                                        => -- | @physicalDevice@ is the handle to the physical device whose properties
                                           -- will be queried.
                                           PhysicalDevice
                                        -> io (("queueFamilyProperties" ::: Vector (QueueFamilyProperties2 a)))
getPhysicalDeviceQueueFamilyProperties2 physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyProperties2Ptr = pVkGetPhysicalDeviceQueueFamilyProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceQueueFamilyProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyProperties2' = mkVkGetPhysicalDeviceQueueFamilyProperties2 vkGetPhysicalDeviceQueueFamilyProperties2Ptr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPQueueFamilyPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyProperties2" (vkGetPhysicalDeviceQueueFamilyProperties2' physicalDevice' (pPQueueFamilyPropertyCount) (forgetExtensions (nullPtr)))
  pQueueFamilyPropertyCount <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pPQueueFamilyProperties <- ContT $ bracket (callocBytes @(QueueFamilyProperties2 _) ((fromIntegral (pQueueFamilyPropertyCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPQueueFamilyProperties `advancePtrBytes` (i * 40) :: Ptr (QueueFamilyProperties2 _)) . ($ ())) [0..(fromIntegral (pQueueFamilyPropertyCount)) - 1]
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyProperties2" (vkGetPhysicalDeviceQueueFamilyProperties2' physicalDevice' (pPQueueFamilyPropertyCount) (forgetExtensions ((pPQueueFamilyProperties))))
  pQueueFamilyPropertyCount' <- lift $ peek @Word32 pPQueueFamilyPropertyCount
  pQueueFamilyProperties' <- lift $ generateM (fromIntegral (pQueueFamilyPropertyCount')) (\i -> peekCStruct @(QueueFamilyProperties2 _) (((pPQueueFamilyProperties) `advancePtrBytes` (40 * (i)) :: Ptr (QueueFamilyProperties2 _))))
  pure $ (pQueueFamilyProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceMemoryProperties2) -> IO ()) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceMemoryProperties2) -> IO ()

-- | vkGetPhysicalDeviceMemoryProperties2 - Reports memory information for
-- the specified physical device
--
-- = Description
--
-- 'getPhysicalDeviceMemoryProperties2' behaves similarly to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceMemoryProperties',
-- with the ability to return extended information in a @pNext@ chain of
-- output structures.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceMemoryProperties2'
getPhysicalDeviceMemoryProperties2 :: forall a io
                                    . (Extendss PhysicalDeviceMemoryProperties2 a, PokeChain a, PeekChain a, MonadIO io)
                                   => -- | @physicalDevice@ is the handle to the device to query.
                                      --
                                      -- #VUID-vkGetPhysicalDeviceMemoryProperties2-physicalDevice-parameter#
                                      -- @physicalDevice@ /must/ be a valid
                                      -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                      PhysicalDevice
                                   -> io (PhysicalDeviceMemoryProperties2 a)
getPhysicalDeviceMemoryProperties2 physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceMemoryProperties2Ptr = pVkGetPhysicalDeviceMemoryProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceMemoryProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceMemoryProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceMemoryProperties2' = mkVkGetPhysicalDeviceMemoryProperties2 vkGetPhysicalDeviceMemoryProperties2Ptr
  pPMemoryProperties <- ContT (withZeroCStruct @(PhysicalDeviceMemoryProperties2 _))
  lift $ traceAroundEvent "vkGetPhysicalDeviceMemoryProperties2" (vkGetPhysicalDeviceMemoryProperties2' (physicalDeviceHandle (physicalDevice)) (forgetExtensions (pPMemoryProperties)))
  pMemoryProperties <- lift $ peekCStruct @(PhysicalDeviceMemoryProperties2 _) pPMemoryProperties
  pure $ (pMemoryProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties2
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceSparseImageFormatInfo2 -> Ptr Word32 -> Ptr SparseImageFormatProperties2 -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceSparseImageFormatInfo2 -> Ptr Word32 -> Ptr SparseImageFormatProperties2 -> IO ()

-- | vkGetPhysicalDeviceSparseImageFormatProperties2 - Retrieve properties of
-- an image format applied to sparse images
--
-- = Description
--
-- 'getPhysicalDeviceSparseImageFormatProperties2' behaves identically to
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties',
-- with the ability to return extended information by adding extending
-- structures to the @pNext@ chain of its @pProperties@ parameter.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties2-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties2-pFormatInfo-parameter#
--     @pFormatInfo@ /must/ be a valid pointer to a valid
--     'PhysicalDeviceSparseImageFormatInfo2' structure
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties2-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSparseImageFormatProperties2-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'SparseImageFormatProperties2'
--     structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'PhysicalDeviceSparseImageFormatInfo2', 'SparseImageFormatProperties2'
getPhysicalDeviceSparseImageFormatProperties2 :: forall io
                                               . (MonadIO io)
                                              => -- | @physicalDevice@ is the physical device from which to query the sparse
                                                 -- image format properties.
                                                 PhysicalDevice
                                              -> -- | @pFormatInfo@ is a pointer to a 'PhysicalDeviceSparseImageFormatInfo2'
                                                 -- structure containing input parameters to the command.
                                                 PhysicalDeviceSparseImageFormatInfo2
                                              -> io (("properties" ::: Vector SparseImageFormatProperties2))
getPhysicalDeviceSparseImageFormatProperties2 physicalDevice formatInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSparseImageFormatProperties2Ptr = pVkGetPhysicalDeviceSparseImageFormatProperties2 (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSparseImageFormatProperties2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSparseImageFormatProperties2 is null" Nothing Nothing
  let vkGetPhysicalDeviceSparseImageFormatProperties2' = mkVkGetPhysicalDeviceSparseImageFormatProperties2 vkGetPhysicalDeviceSparseImageFormatProperties2Ptr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pFormatInfo <- ContT $ withCStruct (formatInfo)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetPhysicalDeviceSparseImageFormatProperties2" (vkGetPhysicalDeviceSparseImageFormatProperties2' physicalDevice' pFormatInfo (pPPropertyCount) (nullPtr))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @SparseImageFormatProperties2 ((fromIntegral (pPropertyCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 40) :: Ptr SparseImageFormatProperties2) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  lift $ traceAroundEvent "vkGetPhysicalDeviceSparseImageFormatProperties2" (vkGetPhysicalDeviceSparseImageFormatProperties2' physicalDevice' pFormatInfo (pPPropertyCount) ((pPProperties)))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @SparseImageFormatProperties2 (((pPProperties) `advancePtrBytes` (40 * (i)) :: Ptr SparseImageFormatProperties2)))
  pure $ (pProperties')


-- | VkPhysicalDeviceFeatures2 - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- features defined by extensions. This structure /can/ be used in
-- 'getPhysicalDeviceFeatures2' or /can/ be included in the @pNext@ chain
-- of a 'Vulkan.Core10.Device.DeviceCreateInfo' structure, in which case it
-- controls which features are enabled in the device in lieu of
-- @pEnabledFeatures@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceFeatures2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2KHR'
data PhysicalDeviceFeatures2 (es :: [Type]) = PhysicalDeviceFeatures2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @features@ is a
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures' structure
    -- describing the fine-grained features of the Vulkan 1.0 API.
    features :: PhysicalDeviceFeatures
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFeatures2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceFeatures2 es)

instance Extensible PhysicalDeviceFeatures2 where
  extensibleTypeName = "PhysicalDeviceFeatures2"
  setNext x next = x{next = next}
  getNext PhysicalDeviceFeatures2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceFeatures2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceDynamicRenderingFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRGBA10X6FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingMotionBlurFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderIntegerDotProductFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProvokingVertexFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInheritedViewportScissorFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSynchronization2FeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceColorWriteEnableFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExternalMemoryRDMAFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVertexInputDynamicStateFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMutableDescriptorTypeFeaturesVALVE = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderTerminateInvocationFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubpassShadingFeaturesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDevice4444FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevicePortabilitySubsetFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageRobustnessFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRobustness2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDiagnosticsConfigFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicState2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicStateFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBorderColorSwizzleFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCustomBorderColorFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCoherentMemoryFeaturesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan12Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan11Features = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineCreationCacheControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLineRasterizationFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubgroupSizeControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTexelBufferAlignmentFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = Just f
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
    | Just Refl <- eqT @e @PhysicalDeviceYcbcrImageArraysFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCooperativeMatrixFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImagelessFramebufferFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMemoryPriorityFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthClipEnableFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceUniformBufferStandardLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceScalarBlockLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMap2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingPipelineFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAccelerationStructureFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMeshShaderFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInvocationMaskFeaturesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShadingRateImageFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageFootprintFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShaderBarycentricFeaturesNV = Just f
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
    | Just Refl <- eqT @e @PhysicalDeviceDeviceMemoryReportFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceGlobalPriorityQueryFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceHostQueryResetFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderFloat16Int8Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDrawParametersFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMaintenance4FeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInlineUniformBlockFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiDrawFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBlendOperationAdvancedFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProtectedMemoryFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSamplerYcbcrConversionFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSubgroupExtendedTypesFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevice16BitStorageFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePresentWaitFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDevicePresentIdFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVariablePointersFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevicePrivateDataFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceFeatures2 es, PokeChain es) => ToCStruct (PhysicalDeviceFeatures2 es) where
  withCStruct x f = allocaBytes 240 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFeatures2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceFeatures)) (features)
    lift $ f
  cStructSize = 240
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceFeatures)) (zero)
    lift $ f

instance (Extendss PhysicalDeviceFeatures2 es, PeekChain es) => FromCStruct (PhysicalDeviceFeatures2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    features <- peekCStruct @PhysicalDeviceFeatures ((p `plusPtr` 16 :: Ptr PhysicalDeviceFeatures))
    pure $ PhysicalDeviceFeatures2
             next features

instance es ~ '[] => Zero (PhysicalDeviceFeatures2 es) where
  zero = PhysicalDeviceFeatures2
           ()
           zero


-- | VkPhysicalDeviceProperties2 - Structure specifying physical device
-- properties
--
-- = Description
--
-- The @pNext@ chain of this structure is used to extend the structure with
-- properties defined by extensions.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceProperties2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2'
--
-- -   #VUID-VkPhysicalDeviceProperties2-pNext-pNext# Each @pNext@ member
--     of any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructurePropertiesKHR',
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT',
--     'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixPropertiesNV',
--     'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorPropertiesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
--     'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsPropertiesNV',
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PhysicalDeviceDiscardRectanglePropertiesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
--     'Vulkan.Extensions.VK_EXT_physical_device_drm.PhysicalDeviceDrmPropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2PropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsPropertiesNV',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockPropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationPropertiesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
--     'Vulkan.Extensions.VK_KHR_maintenance4.PhysicalDeviceMaintenance4PropertiesKHR',
--     'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV',
--     'Vulkan.Extensions.VK_EXT_multi_draw.PhysicalDeviceMultiDrawPropertiesEXT',
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties',
--     'Vulkan.Extensions.VK_EXT_pci_bus_info.PhysicalDevicePCIBusInfoPropertiesEXT',
--     'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PhysicalDevicePointClippingProperties',
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetPropertiesKHR',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.PhysicalDeviceProvokingVertexPropertiesEXT',
--     'Vulkan.Extensions.VK_KHR_push_descriptor.PhysicalDevicePushDescriptorPropertiesKHR',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV',
--     'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2PropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
--     'Vulkan.Extensions.VK_AMD_shader_core_properties2.PhysicalDeviceShaderCoreProperties2AMD',
--     'Vulkan.Extensions.VK_AMD_shader_core_properties.PhysicalDeviceShaderCorePropertiesAMD',
--     'Vulkan.Extensions.VK_KHR_shader_integer_dot_product.PhysicalDeviceShaderIntegerDotProductPropertiesKHR',
--     'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsPropertiesNV',
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImagePropertiesNV',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlPropertiesEXT',
--     'Vulkan.Extensions.VK_HUAWEI_subpass_shading.PhysicalDeviceSubpassShadingPropertiesHUAWEI',
--     'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreProperties',
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
--     'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorPropertiesEXT',
--     'Vulkan.Core12.PhysicalDeviceVulkan11Properties', or
--     'Vulkan.Core12.PhysicalDeviceVulkan12Properties'
--
-- -   #VUID-VkPhysicalDeviceProperties2-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2KHR'
data PhysicalDeviceProperties2 (es :: [Type]) = PhysicalDeviceProperties2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @properties@ is a
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties' structure
    -- describing properties of the physical device. This structure is written
    -- with the same values as if it were written by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceProperties'.
    properties :: PhysicalDeviceProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProperties2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceProperties2 es)

instance Extensible PhysicalDeviceProperties2 where
  extensibleTypeName = "PhysicalDeviceProperties2"
  setNext x next = x{next = next}
  getNext PhysicalDeviceProperties2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceProperties2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceDrmPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderIntegerDotProductPropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProvokingVertexPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateEnumsPropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRatePropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDevicePortabilitySubsetPropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRobustness2PropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCustomBorderColorPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan12Properties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan11Properties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLineRasterizationPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubpassShadingPropertiesHUAWEI = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubgroupSizeControlPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTexelBufferAlignmentPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSMBuiltinsPropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDevicePerformanceQueryPropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCooperativeMatrixPropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMap2PropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingPropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingPipelinePropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAccelerationStructurePropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMeshShaderPropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShadingRateImagePropertiesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTransformFeedbackPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthStencilResolveProperties = Just f
    | Just Refl <- eqT @e @PhysicalDevicePCIBusInfoPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVertexAttributeDivisorPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTimelineSemaphoreProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorIndexingProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderCoreProperties2AMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderCorePropertiesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceConservativeRasterizationPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExternalMemoryHostPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFloatControlsProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMaintenance4PropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMaintenance3Properties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInlineUniformBlockPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBlendOperationAdvancedPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSampleLocationsPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSamplerFilterMinmaxProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProtectedMemoryProperties = Just f
    | Just Refl <- eqT @e @PhysicalDevicePointClippingProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubgroupProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDiscardRectanglePropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceIDProperties = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDriverProperties = Just f
    | Just Refl <- eqT @e @PhysicalDevicePushDescriptorPropertiesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiDrawPropertiesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceProperties2 es, PokeChain es) => ToCStruct (PhysicalDeviceProperties2 es) where
  withCStruct x f = allocaBytes 840 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProperties2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceProperties)) (properties)
    lift $ f
  cStructSize = 840
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceProperties)) (zero)
    lift $ f

instance (Extendss PhysicalDeviceProperties2 es, PeekChain es) => FromCStruct (PhysicalDeviceProperties2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    properties <- peekCStruct @PhysicalDeviceProperties ((p `plusPtr` 16 :: Ptr PhysicalDeviceProperties))
    pure $ PhysicalDeviceProperties2
             next properties

instance es ~ '[] => Zero (PhysicalDeviceProperties2 es) where
  zero = PhysicalDeviceProperties2
           ()
           zero


-- | VkFormatProperties2 - Structure specifying image format properties
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFormatProperties2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FORMAT_PROPERTIES_2'
--
-- -   #VUID-VkFormatProperties2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesList2EXT',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT',
--     'Vulkan.Extensions.VK_KHR_format_feature_flags2.FormatProperties3KHR',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH264ProfileEXT VkVideoDecodeH264ProfileEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH265ProfileEXT VkVideoDecodeH265ProfileEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264ProfileEXT VkVideoEncodeH264ProfileEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265ProfileEXT VkVideoEncodeH265ProfileEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileKHR VkVideoProfileKHR>,
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfilesKHR VkVideoProfilesKHR>
--
-- -   #VUID-VkFormatProperties2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.FormatProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2KHR'
data FormatProperties2 (es :: [Type]) = FormatProperties2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @formatProperties@ is a
    -- 'Vulkan.Core10.DeviceInitialization.FormatProperties' structure
    -- describing features supported by the requested format.
    formatProperties :: FormatProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FormatProperties2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FormatProperties2 es)

instance Extensible FormatProperties2 where
  extensibleTypeName = "FormatProperties2"
  setNext x next = x{next = next}
  getNext FormatProperties2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FormatProperties2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DrmFormatModifierPropertiesList2EXT = Just f
    | Just Refl <- eqT @e @FormatProperties3KHR = Just f
    | Just Refl <- eqT @e @DrmFormatModifierPropertiesListEXT = Just f
    | otherwise = Nothing

instance (Extendss FormatProperties2 es, PokeChain es) => ToCStruct (FormatProperties2 es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FormatProperties2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr FormatProperties)) (formatProperties)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FORMAT_PROPERTIES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr FormatProperties)) (zero)
    lift $ f

instance (Extendss FormatProperties2 es, PeekChain es) => FromCStruct (FormatProperties2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    formatProperties <- peekCStruct @FormatProperties ((p `plusPtr` 16 :: Ptr FormatProperties))
    pure $ FormatProperties2
             next formatProperties

instance es ~ '[] => Zero (FormatProperties2 es) where
  zero = FormatProperties2
           ()
           zero


-- | VkImageFormatProperties2 - Structure specifying an image format
-- properties
--
-- = Description
--
-- If the combination of parameters to
-- 'getPhysicalDeviceImageFormatProperties2' is not supported by the
-- implementation for use in 'Vulkan.Core10.Image.createImage', then all
-- members of @imageFormatProperties@ will be filled with zero.
--
-- Note
--
-- Filling @imageFormatProperties@ with zero for unsupported formats is an
-- exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility. This exeption only applies to
-- @imageFormatProperties@, not @sType@, @pNext@, or any structures chained
-- from @pNext@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageFormatProperties2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2'
--
-- -   #VUID-VkImageFormatProperties2-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferUsageANDROID',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties',
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionImageFormatProperties',
--     or
--     'Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.TextureLODGatherFormatPropertiesAMD'
--
-- -   #VUID-VkImageFormatProperties2-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2KHR'
data ImageFormatProperties2 (es :: [Type]) = ImageFormatProperties2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    -- The @pNext@ chain of 'ImageFormatProperties2' is used to allow the
    -- specification of additional capabilities to be returned from
    -- 'getPhysicalDeviceImageFormatProperties2'.
    next :: Chain es
  , -- | @imageFormatProperties@ is a
    -- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties' structure in
    -- which capabilities are returned.
    imageFormatProperties :: ImageFormatProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageFormatProperties2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageFormatProperties2 es)

instance Extensible ImageFormatProperties2 where
  extensibleTypeName = "ImageFormatProperties2"
  setNext x next = x{next = next}
  getNext ImageFormatProperties2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageFormatProperties2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @FilterCubicImageViewImageFormatPropertiesEXT = Just f
    | Just Refl <- eqT @e @AndroidHardwareBufferUsageANDROID = Just f
    | Just Refl <- eqT @e @TextureLODGatherFormatPropertiesAMD = Just f
    | Just Refl <- eqT @e @SamplerYcbcrConversionImageFormatProperties = Just f
    | Just Refl <- eqT @e @ExternalImageFormatProperties = Just f
    | otherwise = Nothing

instance (Extendss ImageFormatProperties2 es, PokeChain es) => ToCStruct (ImageFormatProperties2 es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageFormatProperties2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageFormatProperties)) (imageFormatProperties)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ImageFormatProperties)) (zero)
    lift $ f

instance (Extendss ImageFormatProperties2 es, PeekChain es) => FromCStruct (ImageFormatProperties2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    imageFormatProperties <- peekCStruct @ImageFormatProperties ((p `plusPtr` 16 :: Ptr ImageFormatProperties))
    pure $ ImageFormatProperties2
             next imageFormatProperties

instance es ~ '[] => Zero (ImageFormatProperties2 es) where
  zero = ImageFormatProperties2
           ()
           zero


-- | VkPhysicalDeviceImageFormatInfo2 - Structure specifying image creation
-- parameters
--
-- = Description
--
-- The members of 'PhysicalDeviceImageFormatInfo2' correspond to the
-- arguments to
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- with @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-tiling-02249# @tiling@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     if and only if the @pNext@ chain includes
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT'
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-tiling-02313# If @tiling@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT'
--     and @flags@ contains
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     then the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'
--     structure with non-zero @viewFormatCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2'
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
--     or
--     'Vulkan.Extensions.VK_EXT_filter_cubic.PhysicalDeviceImageViewImageFormatInfoEXT'
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-format-parameter# @format@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-type-parameter# @type@ /must/
--     be a valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-tiling-parameter# @tiling@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageTiling.ImageTiling'
--     value
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-usage-parameter# @usage@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-usage-requiredbitmask#
--     @usage@ /must/ not be @0@
--
-- -   #VUID-VkPhysicalDeviceImageFormatInfo2-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlags',
-- 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2KHR'
data PhysicalDeviceImageFormatInfo2 (es :: [Type]) = PhysicalDeviceImageFormatInfo2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    -- The @pNext@ chain of 'PhysicalDeviceImageFormatInfo2' is used to provide
    -- additional image parameters to
    -- 'getPhysicalDeviceImageFormatProperties2'.
    next :: Chain es
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' value indicating the
    -- image format, corresponding to
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@format@.
    format :: Format
  , -- | @type@ is a 'Vulkan.Core10.Enums.ImageType.ImageType' value indicating
    -- the image type, corresponding to
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@imageType@.
    type' :: ImageType
  , -- | @tiling@ is a 'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
    -- indicating the image tiling, corresponding to
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@tiling@.
    tiling :: ImageTiling
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' indicating
    -- the intended usage of the image, corresponding to
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@.
    usage :: ImageUsageFlags
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits' indicating
    -- additional parameters of the image, corresponding to
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@.
    flags :: ImageCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageFormatInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceImageFormatInfo2 es)

instance Extensible PhysicalDeviceImageFormatInfo2 where
  extensibleTypeName = "PhysicalDeviceImageFormatInfo2"
  setNext x next = x{next = next}
  getNext PhysicalDeviceImageFormatInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceImageFormatInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceImageViewImageFormatInfoEXT = Just f
    | Just Refl <- eqT @e @ImageStencilUsageCreateInfo = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageDrmFormatModifierInfoEXT = Just f
    | Just Refl <- eqT @e @ImageFormatListCreateInfo = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExternalImageFormatInfo = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceImageFormatInfo2 es, PokeChain es) => ToCStruct (PhysicalDeviceImageFormatInfo2 es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageFormatInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (type')
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageTiling)) (tiling)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 32 :: Ptr ImageCreateFlags)) (flags)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr ImageType)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageTiling)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageUsageFlags)) (zero)
    lift $ f

instance (Extendss PhysicalDeviceImageFormatInfo2 es, PeekChain es) => FromCStruct (PhysicalDeviceImageFormatInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    type' <- peek @ImageType ((p `plusPtr` 20 :: Ptr ImageType))
    tiling <- peek @ImageTiling ((p `plusPtr` 24 :: Ptr ImageTiling))
    usage <- peek @ImageUsageFlags ((p `plusPtr` 28 :: Ptr ImageUsageFlags))
    flags <- peek @ImageCreateFlags ((p `plusPtr` 32 :: Ptr ImageCreateFlags))
    pure $ PhysicalDeviceImageFormatInfo2
             next format type' tiling usage flags

instance es ~ '[] => Zero (PhysicalDeviceImageFormatInfo2 es) where
  zero = PhysicalDeviceImageFormatInfo2
           ()
           zero
           zero
           zero
           zero
           zero


-- | VkQueueFamilyProperties2 - Structure providing information about a queue
-- family
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueueFamilyProperties2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2'
--
-- -   #VUID-VkQueueFamilyProperties2-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_synchronization2.QueueFamilyCheckpointProperties2NV',
--     'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.QueueFamilyCheckpointPropertiesNV',
--     'Vulkan.Extensions.VK_EXT_global_priority_query.QueueFamilyGlobalPriorityPropertiesEXT',
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoQueueFamilyProperties2KHR VkVideoQueueFamilyProperties2KHR>
--
-- -   #VUID-VkQueueFamilyProperties2-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2KHR'
data QueueFamilyProperties2 (es :: [Type]) = QueueFamilyProperties2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @queueFamilyProperties@ is a
    -- 'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties' structure
    -- which is populated with the same values as in
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'.
    queueFamilyProperties :: QueueFamilyProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyProperties2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (QueueFamilyProperties2 es)

instance Extensible QueueFamilyProperties2 where
  extensibleTypeName = "QueueFamilyProperties2"
  setNext x next = x{next = next}
  getNext QueueFamilyProperties2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends QueueFamilyProperties2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @QueueFamilyCheckpointProperties2NV = Just f
    | Just Refl <- eqT @e @QueueFamilyCheckpointPropertiesNV = Just f
    | Just Refl <- eqT @e @QueueFamilyGlobalPriorityPropertiesEXT = Just f
    | otherwise = Nothing

instance (Extendss QueueFamilyProperties2 es, PokeChain es) => ToCStruct (QueueFamilyProperties2 es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyProperties2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr QueueFamilyProperties)) (queueFamilyProperties)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr QueueFamilyProperties)) (zero)
    lift $ f

instance (Extendss QueueFamilyProperties2 es, PeekChain es) => FromCStruct (QueueFamilyProperties2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    queueFamilyProperties <- peekCStruct @QueueFamilyProperties ((p `plusPtr` 16 :: Ptr QueueFamilyProperties))
    pure $ QueueFamilyProperties2
             next queueFamilyProperties

instance es ~ '[] => Zero (QueueFamilyProperties2 es) where
  zero = QueueFamilyProperties2
           ()
           zero


-- | VkPhysicalDeviceMemoryProperties2 - Structure specifying physical device
-- memory properties
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceMemoryProperties2-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2'
--
-- -   #VUID-VkPhysicalDeviceMemoryProperties2-pNext-pNext# @pNext@ /must/
--     be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- -   #VUID-VkPhysicalDeviceMemoryProperties2-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceMemoryProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceMemoryProperties2KHR'
data PhysicalDeviceMemoryProperties2 (es :: [Type]) = PhysicalDeviceMemoryProperties2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @memoryProperties@ is a
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'
    -- structure which is populated with the same values as in
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceMemoryProperties'.
    memoryProperties :: PhysicalDeviceMemoryProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryProperties2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceMemoryProperties2 es)

instance Extensible PhysicalDeviceMemoryProperties2 where
  extensibleTypeName = "PhysicalDeviceMemoryProperties2"
  setNext x next = x{next = next}
  getNext PhysicalDeviceMemoryProperties2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceMemoryProperties2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceMemoryBudgetPropertiesEXT = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceMemoryProperties2 es, PokeChain es) => ToCStruct (PhysicalDeviceMemoryProperties2 es) where
  withCStruct x f = allocaBytes 536 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryProperties2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceMemoryProperties)) (memoryProperties)
    lift $ f
  cStructSize = 536
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr PhysicalDeviceMemoryProperties)) (zero)
    lift $ f

instance (Extendss PhysicalDeviceMemoryProperties2 es, PeekChain es) => FromCStruct (PhysicalDeviceMemoryProperties2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    memoryProperties <- peekCStruct @PhysicalDeviceMemoryProperties ((p `plusPtr` 16 :: Ptr PhysicalDeviceMemoryProperties))
    pure $ PhysicalDeviceMemoryProperties2
             next memoryProperties

instance es ~ '[] => Zero (PhysicalDeviceMemoryProperties2 es) where
  zero = PhysicalDeviceMemoryProperties2
           ()
           zero


-- | VkSparseImageFormatProperties2 - Structure specifying sparse image
-- format properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceSparseImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2KHR'
data SparseImageFormatProperties2 = SparseImageFormatProperties2
  { -- | @properties@ is a
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageFormatProperties'
    -- structure which is populated with the same values as in
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties'.
    properties :: SparseImageFormatProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SparseImageFormatProperties2)
#endif
deriving instance Show SparseImageFormatProperties2

instance ToCStruct SparseImageFormatProperties2 where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SparseImageFormatProperties2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SparseImageFormatProperties)) (properties)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SparseImageFormatProperties)) (zero)
    f

instance FromCStruct SparseImageFormatProperties2 where
  peekCStruct p = do
    properties <- peekCStruct @SparseImageFormatProperties ((p `plusPtr` 16 :: Ptr SparseImageFormatProperties))
    pure $ SparseImageFormatProperties2
             properties

instance Storable SparseImageFormatProperties2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SparseImageFormatProperties2 where
  zero = SparseImageFormatProperties2
           zero


-- | VkPhysicalDeviceSparseImageFormatInfo2 - Structure specifying sparse
-- image format inputs
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.ImageTiling.ImageTiling',
-- 'Vulkan.Core10.Enums.ImageType.ImageType',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceSparseImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2KHR'
data PhysicalDeviceSparseImageFormatInfo2 = PhysicalDeviceSparseImageFormatInfo2
  { -- | @format@ is the image format.
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-format-parameter# @format@
    -- /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
    format :: Format
  , -- | @type@ is the dimensionality of image.
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-type-parameter# @type@
    -- /must/ be a valid 'Vulkan.Core10.Enums.ImageType.ImageType' value
    type' :: ImageType
  , -- | @samples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- specifying the number of samples per texel.
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-samples-01095# @samples@
    -- /must/ be a bit value that is set in
    -- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties'::@sampleCounts@
    -- returned by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
    -- with @format@, @type@, @tiling@, and @usage@ equal to those in this
    -- command and @flags@ equal to the value that is set in
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ when the image is created
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-samples-parameter#
    -- @samples@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    samples :: SampleCountFlagBits
  , -- | @usage@ is a bitmask describing the intended usage of the image.
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-usage-parameter# @usage@
    -- /must/ be a valid combination of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' values
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-usage-requiredbitmask#
    -- @usage@ /must/ not be @0@
    usage :: ImageUsageFlags
  , -- | @tiling@ is the tiling arrangement of the texel blocks in memory.
    --
    -- #VUID-VkPhysicalDeviceSparseImageFormatInfo2-tiling-parameter# @tiling@
    -- /must/ be a valid 'Vulkan.Core10.Enums.ImageTiling.ImageTiling' value
    tiling :: ImageTiling
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSparseImageFormatInfo2)
#endif
deriving instance Show PhysicalDeviceSparseImageFormatInfo2

instance ToCStruct PhysicalDeviceSparseImageFormatInfo2 where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSparseImageFormatInfo2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    poke ((p `plusPtr` 20 :: Ptr ImageType)) (type')
    poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (samples)
    poke ((p `plusPtr` 28 :: Ptr ImageUsageFlags)) (usage)
    poke ((p `plusPtr` 32 :: Ptr ImageTiling)) (tiling)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ImageType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr SampleCountFlagBits)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageUsageFlags)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ImageTiling)) (zero)
    f

instance FromCStruct PhysicalDeviceSparseImageFormatInfo2 where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    type' <- peek @ImageType ((p `plusPtr` 20 :: Ptr ImageType))
    samples <- peek @SampleCountFlagBits ((p `plusPtr` 24 :: Ptr SampleCountFlagBits))
    usage <- peek @ImageUsageFlags ((p `plusPtr` 28 :: Ptr ImageUsageFlags))
    tiling <- peek @ImageTiling ((p `plusPtr` 32 :: Ptr ImageTiling))
    pure $ PhysicalDeviceSparseImageFormatInfo2
             format type' samples usage tiling

instance Storable PhysicalDeviceSparseImageFormatInfo2 where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSparseImageFormatInfo2 where
  zero = PhysicalDeviceSparseImageFormatInfo2
           zero
           zero
           zero
           zero
           zero

