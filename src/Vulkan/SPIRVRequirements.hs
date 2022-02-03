{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language TupleSections #-}
-- No documentation found for Chapter "SPIRVRequirements"
module Vulkan.SPIRVRequirements  ( spirvExtensionRequirements
                                 , spirvCapabilityRequirements
                                 ) where

import Vulkan.Requirement (DeviceRequirement(..))
import Vulkan.Requirement (InstanceRequirement(..))
import Data.Bits ((.&.))
import Data.Bits (zeroBits)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures(..))
import Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT(..))
import Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV(..))
import Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV(..))
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures(..))
import Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT(..))
import Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..))
import Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT(..))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR(..))
import Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures(..))
import Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR(..))
import Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (PhysicalDeviceRayTracingMotionBlurFeaturesNV(..))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR(..))
import Vulkan.Extensions.VK_EXT_shader_atomic_float2 (PhysicalDeviceShaderAtomicFloat2FeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeatures(..))
import Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures(..))
import Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT(..))
import Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV(..))
import Vulkan.Extensions.VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeaturesKHR)
import Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeatures(..))
import Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL(..))
import Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV(..))
import Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV(..))
import Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT(..))
import Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Features(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Features(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Properties)
import Vulkan.Core12 (PhysicalDeviceVulkan12Properties(..))
import Vulkan.Core13 (PhysicalDeviceVulkan13Features(..))
import Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR(..))
import Vulkan.Extensions.VK_AMD_gcn_shader (pattern AMD_GCN_SHADER_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_gpu_shader_half_float (pattern AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_gpu_shader_int16 (pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_shader_ballot (pattern AMD_SHADER_BALLOT_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_shader_explicit_vertex_parameter (pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_shader_fragment_mask (pattern AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_shader_image_load_store_lod (pattern AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_shader_trinary_minmax (pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME)
import Vulkan.Extensions.VK_AMD_texture_gather_bias_lod (pattern AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_buffer_device_address (pattern EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_descriptor_indexing (pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_fragment_density_map (pattern EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_fragment_shader_interlock (pattern EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_post_depth_coverage (pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_atomic_float2 (pattern EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_atomic_float (pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_stencil_export (pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_subgroup_ballot (pattern EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_subgroup_vote (pattern EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_viewport_index_layer (pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_transform_feedback (pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME)
import Vulkan.Extensions.VK_GOOGLE_decorate_string (pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME)
import Vulkan.Extensions.VK_GOOGLE_hlsl_functionality1 (pattern GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME)
import Vulkan.Extensions.VK_GOOGLE_user_type (pattern GOOGLE_USER_TYPE_EXTENSION_NAME)
import Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (pattern INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_16bit_storage (pattern KHR_16BIT_STORAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_8bit_storage (pattern KHR_8BIT_STORAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_acceleration_structure (pattern KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_buffer_device_address (pattern KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_create_renderpass2 (pattern KHR_CREATE_RENDERPASS_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_deferred_host_operations (pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_device_group_creation (pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_device_group (pattern KHR_DEVICE_GROUP_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_format_feature_flags2 (pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2 (pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_maintenance2 (pattern KHR_MAINTENANCE_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_maintenance3 (pattern KHR_MAINTENANCE_3_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_multiview (pattern KHR_MULTIVIEW_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_ray_query (pattern KHR_RAY_QUERY_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_atomic_int64 (pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_clock (pattern KHR_SHADER_CLOCK_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_draw_parameters (pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float16_int8 (pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float_controls (pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_integer_dot_product (pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info (pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow (pattern KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_terminate_invocation (pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_spirv_1_4 (pattern KHR_SPIRV_1_4_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_storage_buffer_storage_class (pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_variable_pointers (pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_vulkan_memory_model (pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout (pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME)
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_compute_shader_derivatives (pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_cooperative_matrix (pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_fragment_shader_barycentric (pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_geometry_shader_passthrough (pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_mesh_shader (pattern NV_MESH_SHADER_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_ray_tracing (pattern NV_RAY_TRACING_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (pattern NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_sample_mask_override_coverage (pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_image_footprint (pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_sm_builtins (pattern NV_SHADER_SM_BUILTINS_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_subgroup_partitioned (pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shading_rate_image (pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_viewport_array2 (pattern NV_VIEWPORT_ARRAY_2_EXTENSION_NAME)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_ARITHMETIC_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_BALLOT_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_BASIC_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_CLUSTERED_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_PARTITIONED_BIT_NV))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_QUAD_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_SHUFFLE_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_VOTE_BIT))
-- | Check if the intersection of bits is non-zero
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (x .&. y) /= zeroBits

spirvExtensionRequirements :: ByteString -> ([InstanceRequirement], [DeviceRequirement])
spirvExtensionRequirements = \case
  "SPV_KHR_variable_pointers" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_AMD_shader_explicit_vertex_parameter" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_gcn_shader" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_GCN_SHADER_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_gpu_shader_half_float" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_gpu_shader_int16" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_GPU_SHADER_INT16_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_shader_ballot" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_BALLOT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_shader_fragment_mask" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_shader_image_load_store_lod" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_shader_trinary_minmax" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_AMD_texture_gather_bias_lod" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_shader_draw_parameters" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_8bit_storage" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_16bit_storage" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_shader_clock" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_CLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_float_controls" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_storage_buffer_storage_class" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_post_depth_coverage" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_shader_stencil_export" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_shader_ballot" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_subgroup_vote" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_sample_mask_override_coverage" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_geometry_shader_passthrough" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_mesh_shader" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_MESH_SHADER_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_viewport_array2" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_shader_subgroup_partitioned" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_shader_viewport_index_layer" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_NVX_multiview_per_view_attributes" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MULTIVIEW_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_descriptor_indexing" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_vulkan_memory_model" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_NV_compute_shader_derivatives" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_fragment_shader_barycentric" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_shader_image_footprint" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_shading_rate" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_ray_tracing" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_RAY_TRACING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_ray_tracing" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_ray_query" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_QUERY_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_GOOGLE_hlsl_functionality1" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_GOOGLE_user_type" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = GOOGLE_USER_TYPE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_GOOGLE_decorate_string" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = GOOGLE_DECORATE_STRING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_fragment_invocation_density" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_physical_storage_buffer" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_EXT_physical_storage_buffer" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_cooperative_matrix" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_NV_shader_sm_builtins" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_SM_BUILTINS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_fragment_shader_interlock" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_demote_to_helper_invocation" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_KHR_fragment_shading_rate" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MULTIVIEW_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_non_semantic_info" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_EXT_shader_image_int64" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_terminate_invocation" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_KHR_multiview" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_workgroup_memory_explicit_layout" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_shader_atomic_float_add" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_subgroup_uniform_control_flow" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_EXT_shader_atomic_float_min_max" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_EXT_shader_atomic_float16_add" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_integer_dot_product" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  _ -> ([], [])

spirvCapabilityRequirements :: ByteString -> ([InstanceRequirement], [DeviceRequirement])
spirvCapabilityRequirements = \case
  "Matrix" -> (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Shader" -> (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "InputAttachment" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Sampled1D" -> (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Image1D"   -> (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "SampledBuffer" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "ImageBuffer" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "ImageQuery" -> (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "DerivativeControl" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Geometry" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "geometryShader"
      , checkFeature  = \PhysicalDeviceFeatures { geometryShader } -> geometryShader
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { geometryShader = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "Tessellation" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "tessellationShader"
      , checkFeature  = \PhysicalDeviceFeatures { tessellationShader } -> tessellationShader
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { tessellationShader = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "Float64" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderFloat64"
      , checkFeature  = \PhysicalDeviceFeatures { shaderFloat64 } -> shaderFloat64
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderFloat64 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "Int64" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderInt64"
      , checkFeature  = \PhysicalDeviceFeatures { shaderInt64 } -> shaderInt64
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderInt64 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "Int64Atomics" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferInt64Atomics"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderBufferInt64Atomics } -> shaderBufferInt64Atomics
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderBufferInt64Atomics = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat16AddEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat16AtomicAdd"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat16AtomicAdd } ->
                          shaderBufferFloat16AtomicAdd
      , enableFeature = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat16AtomicAdd = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat32AddEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat32AtomicAdd"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloatFeaturesEXT { shaderBufferFloat32AtomicAdd } ->
                          shaderBufferFloat32AtomicAdd
      , enableFeature = \PhysicalDeviceShaderAtomicFloatFeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloatFeaturesEXT { shaderBufferFloat32AtomicAdd = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat64AddEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat64AtomicAdd"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloatFeaturesEXT { shaderBufferFloat64AtomicAdd } ->
                          shaderBufferFloat64AtomicAdd
      , enableFeature = \PhysicalDeviceShaderAtomicFloatFeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloatFeaturesEXT { shaderBufferFloat64AtomicAdd = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat16MinMaxEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat16AtomicMinMax"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat16AtomicMinMax } ->
                          shaderBufferFloat16AtomicMinMax
      , enableFeature = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat16AtomicMinMax = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat32MinMaxEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat32AtomicMinMax"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat32AtomicMinMax } ->
                          shaderBufferFloat32AtomicMinMax
      , enableFeature = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat32AtomicMinMax = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "AtomicFloat64MinMaxEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferFloat64AtomicMinMax"
      , checkFeature  = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat64AtomicMinMax } ->
                          shaderBufferFloat64AtomicMinMax
      , enableFeature = \PhysicalDeviceShaderAtomicFloat2FeaturesEXT {..} ->
                          PhysicalDeviceShaderAtomicFloat2FeaturesEXT { shaderBufferFloat64AtomicMinMax = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Int64ImageEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderImageInt64Atomics"
      , checkFeature  = \PhysicalDeviceShaderImageAtomicInt64FeaturesEXT { shaderImageInt64Atomics } ->
                          shaderImageInt64Atomics
      , enableFeature = \PhysicalDeviceShaderImageAtomicInt64FeaturesEXT {..} ->
                          PhysicalDeviceShaderImageAtomicInt64FeaturesEXT { shaderImageInt64Atomics = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Int16" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderInt16"
      , checkFeature  = \PhysicalDeviceFeatures { shaderInt16 } -> shaderInt16
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderInt16 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "TessellationPointSize" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderTessellationAndGeometryPointSize"
      , checkFeature  = \PhysicalDeviceFeatures { shaderTessellationAndGeometryPointSize } ->
                          shaderTessellationAndGeometryPointSize
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderTessellationAndGeometryPointSize = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "GeometryPointSize" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderTessellationAndGeometryPointSize"
      , checkFeature  = \PhysicalDeviceFeatures { shaderTessellationAndGeometryPointSize } ->
                          shaderTessellationAndGeometryPointSize
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderTessellationAndGeometryPointSize = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "ImageGatherExtended" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderImageGatherExtended"
      , checkFeature  = \PhysicalDeviceFeatures { shaderImageGatherExtended } -> shaderImageGatherExtended
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderImageGatherExtended = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageImageMultisample" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageMultisample"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageImageMultisample } -> shaderStorageImageMultisample
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageImageMultisample = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "UniformBufferArrayDynamicIndexing" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformBufferArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceFeatures { shaderUniformBufferArrayDynamicIndexing } ->
                          shaderUniformBufferArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderUniformBufferArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "SampledImageArrayDynamicIndexing" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderSampledImageArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceFeatures { shaderSampledImageArrayDynamicIndexing } ->
                          shaderSampledImageArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderSampledImageArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageBufferArrayDynamicIndexing" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageBufferArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageBufferArrayDynamicIndexing } ->
                          shaderStorageBufferArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageBufferArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageImageArrayDynamicIndexing" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageImageArrayDynamicIndexing } ->
                          shaderStorageImageArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageImageArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "ClipDistance" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderClipDistance"
      , checkFeature  = \PhysicalDeviceFeatures { shaderClipDistance } -> shaderClipDistance
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderClipDistance = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "CullDistance" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderCullDistance"
      , checkFeature  = \PhysicalDeviceFeatures { shaderCullDistance } -> shaderCullDistance
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderCullDistance = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "ImageCubeArray" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "imageCubeArray"
      , checkFeature  = \PhysicalDeviceFeatures { imageCubeArray } -> imageCubeArray
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { imageCubeArray = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "SampleRateShading" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "sampleRateShading"
      , checkFeature  = \PhysicalDeviceFeatures { sampleRateShading } -> sampleRateShading
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { sampleRateShading = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "SparseResidency" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderResourceResidency"
      , checkFeature  = \PhysicalDeviceFeatures { shaderResourceResidency } -> shaderResourceResidency
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderResourceResidency = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "MinLod" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderResourceMinLod"
      , checkFeature  = \PhysicalDeviceFeatures { shaderResourceMinLod } -> shaderResourceMinLod
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { shaderResourceMinLod = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "SampledCubeArray" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "imageCubeArray"
      , checkFeature  = \PhysicalDeviceFeatures { imageCubeArray } -> imageCubeArray
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { imageCubeArray = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "ImageMSArray" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageMultisample"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageImageMultisample } -> shaderStorageImageMultisample
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageImageMultisample = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageImageExtendedFormats" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "InterpolationFunction" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "sampleRateShading"
      , checkFeature  = \PhysicalDeviceFeatures { sampleRateShading } -> sampleRateShading
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { sampleRateShading = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageImageReadWithoutFormat" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageReadWithoutFormat"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageImageReadWithoutFormat } ->
                          shaderStorageImageReadWithoutFormat
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageImageReadWithoutFormat = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "StorageImageWriteWithoutFormat" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageWriteWithoutFormat"
      , checkFeature  = \PhysicalDeviceFeatures { shaderStorageImageWriteWithoutFormat } ->
                          shaderStorageImageWriteWithoutFormat
      , enableFeature = \PhysicalDeviceFeatures {..} ->
                          PhysicalDeviceFeatures { shaderStorageImageWriteWithoutFormat = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "MultiViewport" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
    [ RequireDeviceFeature
      { featureName   = "multiViewport"
      , checkFeature  = \PhysicalDeviceFeatures { multiViewport } -> multiViewport
      , enableFeature = \PhysicalDeviceFeatures {..} -> PhysicalDeviceFeatures { multiViewport = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
    ]
  "DrawParameters" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "shaderDrawParameters"
      , checkFeature  = \PhysicalDeviceVulkan11Features { shaderDrawParameters } -> shaderDrawParameters
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { shaderDrawParameters = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "MultiView" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "multiview"
      , checkFeature  = \PhysicalDeviceVulkan11Features { multiview } -> multiview
      , enableFeature = \PhysicalDeviceVulkan11Features {..} -> PhysicalDeviceVulkan11Features { multiview = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "DeviceGroup" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "VariablePointersStorageBuffer" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "variablePointersStorageBuffer"
      , checkFeature  = \PhysicalDeviceVulkan11Features { variablePointersStorageBuffer } ->
                          variablePointersStorageBuffer
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { variablePointersStorageBuffer = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "VariablePointers" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "variablePointers"
      , checkFeature  = \PhysicalDeviceVulkan11Features { variablePointers } -> variablePointers
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { variablePointers = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "ShaderClockKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_CLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StencilExportEXT" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SubgroupBallotKHR" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SubgroupVoteKHR" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ImageReadWriteLodAMD" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ImageGatherBiasLodAMD" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentMaskAMD" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SampleMaskOverrideCoverageNV" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "GeometryShaderPassthroughNV" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderViewportIndex" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "shaderOutputViewportIndex"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderOutputViewportIndex } -> shaderOutputViewportIndex
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderOutputViewportIndex = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "ShaderLayer" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "shaderOutputLayer"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderOutputLayer } -> shaderOutputLayer
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderOutputLayer = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "ShaderViewportIndexLayerEXT" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderViewportIndexLayerNV" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderViewportMaskNV" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "PerViewAttributesNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MULTIVIEW_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageBuffer16BitAccess" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "storageBuffer16BitAccess"
      , checkFeature  = \PhysicalDeviceVulkan11Features { storageBuffer16BitAccess } -> storageBuffer16BitAccess
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { storageBuffer16BitAccess = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "UniformAndStorageBuffer16BitAccess" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "uniformAndStorageBuffer16BitAccess"
      , checkFeature  = \PhysicalDeviceVulkan11Features { uniformAndStorageBuffer16BitAccess } ->
                          uniformAndStorageBuffer16BitAccess
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { uniformAndStorageBuffer16BitAccess = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "StoragePushConstant16" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "storagePushConstant16"
      , checkFeature  = \PhysicalDeviceVulkan11Features { storagePushConstant16 } -> storagePushConstant16
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { storagePushConstant16 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "StorageInputOutput16" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "storageInputOutput16"
      , checkFeature  = \PhysicalDeviceVulkan11Features { storageInputOutput16 } -> storageInputOutput16
      , enableFeature = \PhysicalDeviceVulkan11Features {..} ->
                          PhysicalDeviceVulkan11Features { storageInputOutput16 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    ]
  "GroupNonUniform" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_BASIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformVote" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_VOTE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformArithmetic" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_ARITHMETIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformBallot" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_BALLOT_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformShuffle" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_SHUFFLE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformShuffleRelative" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformClustered" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_CLUSTERED_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformQuad" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_QUAD_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
    ]
  "GroupNonUniformPartitionedNV" -> (,)
    []
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_PARTITIONED_BIT_NV .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SampleMaskPostDepthCoverage" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderNonUniform" ->
    (,) [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "RuntimeDescriptorArray" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "runtimeDescriptorArray"
      , checkFeature  = \PhysicalDeviceVulkan12Features { runtimeDescriptorArray } -> runtimeDescriptorArray
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { runtimeDescriptorArray = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "InputAttachmentArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderInputAttachmentArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderInputAttachmentArrayDynamicIndexing } ->
                          shaderInputAttachmentArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderInputAttachmentArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformTexelBufferArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformTexelBufferArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderUniformTexelBufferArrayDynamicIndexing } ->
                          shaderUniformTexelBufferArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderUniformTexelBufferArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageTexelBufferArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageTexelBufferArrayDynamicIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderStorageTexelBufferArrayDynamicIndexing } ->
                          shaderStorageTexelBufferArrayDynamicIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderStorageTexelBufferArrayDynamicIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformBufferArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderUniformBufferArrayNonUniformIndexing } ->
                          shaderUniformBufferArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderUniformBufferArrayNonUniformIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SampledImageArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderSampledImageArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderSampledImageArrayNonUniformIndexing } ->
                          shaderSampledImageArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderSampledImageArrayNonUniformIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageBufferArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderStorageBufferArrayNonUniformIndexing } ->
                          shaderStorageBufferArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderStorageBufferArrayNonUniformIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageImageArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderStorageImageArrayNonUniformIndexing } ->
                          shaderStorageImageArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderStorageImageArrayNonUniformIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "InputAttachmentArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderInputAttachmentArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderInputAttachmentArrayNonUniformIndexing } ->
                          shaderInputAttachmentArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderInputAttachmentArrayNonUniformIndexing = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformTexelBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformTexelBufferArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderUniformTexelBufferArrayNonUniformIndexing } ->
                          shaderUniformTexelBufferArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} -> PhysicalDeviceVulkan12Features
                          { shaderUniformTexelBufferArrayNonUniformIndexing = True
                          , ..
                          }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageTexelBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageTexelBufferArrayNonUniformIndexing"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderStorageTexelBufferArrayNonUniformIndexing } ->
                          shaderStorageTexelBufferArrayNonUniformIndexing
      , enableFeature = \PhysicalDeviceVulkan12Features {..} -> PhysicalDeviceVulkan12Features
                          { shaderStorageTexelBufferArrayNonUniformIndexing = True
                          , ..
                          }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Float16" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderFloat16"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderFloat16 } -> shaderFloat16
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { shaderFloat16 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Int8" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderInt8"
      , checkFeature  = \PhysicalDeviceVulkan12Features { shaderInt8 } -> shaderInt8
      , enableFeature = \PhysicalDeviceVulkan12Features {..} -> PhysicalDeviceVulkan12Features { shaderInt8 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageBuffer8BitAccess" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "storageBuffer8BitAccess"
      , checkFeature  = \PhysicalDeviceVulkan12Features { storageBuffer8BitAccess } -> storageBuffer8BitAccess
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { storageBuffer8BitAccess = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_8BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformAndStorageBuffer8BitAccess" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "uniformAndStorageBuffer8BitAccess"
      , checkFeature  = \PhysicalDeviceVulkan12Features { uniformAndStorageBuffer8BitAccess } ->
                          uniformAndStorageBuffer8BitAccess
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { uniformAndStorageBuffer8BitAccess = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_8BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StoragePushConstant8" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "storagePushConstant8"
      , checkFeature  = \PhysicalDeviceVulkan12Features { storagePushConstant8 } -> storagePushConstant8
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { storagePushConstant8 = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_8BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "VulkanMemoryModel" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "vulkanMemoryModel"
      , checkFeature  = \PhysicalDeviceVulkan12Features { vulkanMemoryModel } -> vulkanMemoryModel
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { vulkanMemoryModel = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "VulkanMemoryModelDeviceScope" -> (,)
    [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "vulkanMemoryModelDeviceScope"
      , checkFeature  = \PhysicalDeviceVulkan12Features { vulkanMemoryModelDeviceScope } -> vulkanMemoryModelDeviceScope
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { vulkanMemoryModelDeviceScope = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DenormPreserve" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderDenormPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DenormFlushToZero" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty = \p -> shaderDenormFlushToZeroFloat16 (p :: PhysicalDeviceVulkan12Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SignedZeroInfNanPreserve" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty = \p -> shaderSignedZeroInfNanPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RoundingModeRTE" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderRoundingModeRTEFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RoundingModeRTZ" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderRoundingModeRTZFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ComputeDerivativeGroupQuadsNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "computeDerivativeGroupQuads"
      , checkFeature  = \PhysicalDeviceComputeShaderDerivativesFeaturesNV { computeDerivativeGroupQuads } ->
                          computeDerivativeGroupQuads
      , enableFeature = \PhysicalDeviceComputeShaderDerivativesFeaturesNV {..} ->
                          PhysicalDeviceComputeShaderDerivativesFeaturesNV { computeDerivativeGroupQuads = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ComputeDerivativeGroupLinearNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "computeDerivativeGroupLinear"
      , checkFeature  = \PhysicalDeviceComputeShaderDerivativesFeaturesNV { computeDerivativeGroupLinear } ->
                          computeDerivativeGroupLinear
      , enableFeature = \PhysicalDeviceComputeShaderDerivativesFeaturesNV {..} ->
                          PhysicalDeviceComputeShaderDerivativesFeaturesNV { computeDerivativeGroupLinear = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentBarycentricNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "fragmentShaderBarycentric"
      , checkFeature  = \PhysicalDeviceFragmentShaderBarycentricFeaturesNV { fragmentShaderBarycentric } ->
                          fragmentShaderBarycentric
      , enableFeature = \_ -> PhysicalDeviceFragmentShaderBarycentricFeaturesNV { fragmentShaderBarycentric = True }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ImageFootprintNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "imageFootprint"
      , checkFeature  = \PhysicalDeviceShaderImageFootprintFeaturesNV { imageFootprint } -> imageFootprint
      , enableFeature = \_ -> PhysicalDeviceShaderImageFootprintFeaturesNV { imageFootprint = True }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShadingRateNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shadingRateImage"
      , checkFeature  = \PhysicalDeviceShadingRateImageFeaturesNV { shadingRateImage } -> shadingRateImage
      , enableFeature = \PhysicalDeviceShadingRateImageFeaturesNV {..} ->
                          PhysicalDeviceShadingRateImageFeaturesNV { shadingRateImage = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADING_RATE_IMAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "MeshShadingNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_MESH_SHADER_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RayTracingKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "rayTracingPipeline"
      , checkFeature  = \PhysicalDeviceRayTracingPipelineFeaturesKHR { rayTracingPipeline } -> rayTracingPipeline
      , enableFeature = \PhysicalDeviceRayTracingPipelineFeaturesKHR {..} ->
                          PhysicalDeviceRayTracingPipelineFeaturesKHR { rayTracingPipeline = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RayQueryKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "rayQuery"
                           , checkFeature  = \PhysicalDeviceRayQueryFeaturesKHR { rayQuery } -> rayQuery
                           , enableFeature = \_ -> PhysicalDeviceRayQueryFeaturesKHR { rayQuery = True }
                           }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_QUERY_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RayTraversalPrimitiveCullingKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "rayTraversalPrimitiveCulling"
      , checkFeature  = \PhysicalDeviceRayTracingPipelineFeaturesKHR { rayTraversalPrimitiveCulling } ->
                          rayTraversalPrimitiveCulling
      , enableFeature = \PhysicalDeviceRayTracingPipelineFeaturesKHR {..} ->
                          PhysicalDeviceRayTracingPipelineFeaturesKHR { rayTraversalPrimitiveCulling = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RayTracingNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_RAY_TRACING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RayTracingMotionBlurNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "rayTracingMotionBlur"
      , checkFeature  = \PhysicalDeviceRayTracingMotionBlurFeaturesNV { rayTracingMotionBlur } -> rayTracingMotionBlur
      , enableFeature = \PhysicalDeviceRayTracingMotionBlurFeaturesNV {..} ->
                          PhysicalDeviceRayTracingMotionBlurFeaturesNV { rayTracingMotionBlur = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SPIRV_1_4_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "TransformFeedback" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "transformFeedback"
      , checkFeature  = \PhysicalDeviceTransformFeedbackFeaturesEXT { transformFeedback } -> transformFeedback
      , enableFeature = \PhysicalDeviceTransformFeedbackFeaturesEXT {..} ->
                          PhysicalDeviceTransformFeedbackFeaturesEXT { transformFeedback = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "GeometryStreams" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "geometryStreams"
      , checkFeature  = \PhysicalDeviceTransformFeedbackFeaturesEXT { geometryStreams } -> geometryStreams
      , enableFeature = \PhysicalDeviceTransformFeedbackFeaturesEXT {..} ->
                          PhysicalDeviceTransformFeedbackFeaturesEXT { geometryStreams = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentDensityEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "fragmentDensityMap"
      , checkFeature  = \PhysicalDeviceFragmentDensityMapFeaturesEXT { fragmentDensityMap } -> fragmentDensityMap
      , enableFeature = \PhysicalDeviceFragmentDensityMapFeaturesEXT {..} ->
                          PhysicalDeviceFragmentDensityMapFeaturesEXT { fragmentDensityMap = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "PhysicalStorageBufferAddresses" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "bufferDeviceAddress"
      , checkFeature  = \PhysicalDeviceVulkan12Features { bufferDeviceAddress } -> bufferDeviceAddress
      , enableFeature = \PhysicalDeviceVulkan12Features {..} ->
                          PhysicalDeviceVulkan12Features { bufferDeviceAddress = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "CooperativeMatrixNV" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "cooperativeMatrix"
      , checkFeature  = \PhysicalDeviceCooperativeMatrixFeaturesNV { cooperativeMatrix } -> cooperativeMatrix
      , enableFeature = \PhysicalDeviceCooperativeMatrixFeaturesNV {..} ->
                          PhysicalDeviceCooperativeMatrixFeaturesNV { cooperativeMatrix = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "IntegerFunctions2INTEL" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderIntegerFunctions2"
      , checkFeature  = \PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL { shaderIntegerFunctions2 } ->
                          shaderIntegerFunctions2
      , enableFeature = \_ -> PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL { shaderIntegerFunctions2 = True }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderSMBuiltinsNV" -> (,)
    []
    [ RequireDeviceFeature
      { featureName   = "shaderSMBuiltins"
      , checkFeature  = \PhysicalDeviceShaderSMBuiltinsFeaturesNV { shaderSMBuiltins } -> shaderSMBuiltins
      , enableFeature = \_ -> PhysicalDeviceShaderSMBuiltinsFeaturesNV { shaderSMBuiltins = True }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_SHADER_SM_BUILTINS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentShaderSampleInterlockEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "fragmentShaderSampleInterlock"
      , checkFeature  = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderSampleInterlock } ->
                          fragmentShaderSampleInterlock
      , enableFeature = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT {..} ->
                          PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderSampleInterlock = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentShaderPixelInterlockEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "fragmentShaderPixelInterlock"
      , checkFeature  = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderPixelInterlock } ->
                          fragmentShaderPixelInterlock
      , enableFeature = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT {..} ->
                          PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderPixelInterlock = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentShaderShadingRateInterlockEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "fragmentShaderShadingRateInterlock"
      , checkFeature  = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderShadingRateInterlock } ->
                          fragmentShaderShadingRateInterlock
      , enableFeature = \PhysicalDeviceFragmentShaderInterlockFeaturesEXT {..} ->
                          PhysicalDeviceFragmentShaderInterlockFeaturesEXT { fragmentShaderShadingRateInterlock = True
                                                                           , ..
                                                                           }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DemoteToHelperInvocationEXT" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 3 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderDemoteToHelperInvocation"
      , checkFeature  = \PhysicalDeviceVulkan13Features { shaderDemoteToHelperInvocation } ->
                          shaderDemoteToHelperInvocation
      , enableFeature = \PhysicalDeviceVulkan13Features {..} ->
                          PhysicalDeviceVulkan13Features { shaderDemoteToHelperInvocation = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 3 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "FragmentShadingRateKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "pipelineFragmentShadingRate"
      , checkFeature  = \PhysicalDeviceFragmentShadingRateFeaturesKHR { pipelineFragmentShadingRate } ->
                          pipelineFragmentShadingRate
      , enableFeature = \PhysicalDeviceFragmentShadingRateFeaturesKHR {..} ->
                          PhysicalDeviceFragmentShadingRateFeaturesKHR { pipelineFragmentShadingRate = True, .. }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_CREATE_RENDERPASS_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MULTIVIEW_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE_2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "WorkgroupMemoryExplicitLayoutKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "workgroupMemoryExplicitLayout"
      , checkFeature  = \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR { workgroupMemoryExplicitLayout } ->
                          workgroupMemoryExplicitLayout
      , enableFeature = \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR {..} ->
                          PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR { workgroupMemoryExplicitLayout = True
                                                                                 , ..
                                                                                 }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "WorkgroupMemoryExplicitLayout8BitAccessKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "workgroupMemoryExplicitLayout8BitAccess"
      , checkFeature  =
        \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR { workgroupMemoryExplicitLayout8BitAccess } ->
          workgroupMemoryExplicitLayout8BitAccess
      , enableFeature = \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR {..} ->
                          PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
                            { workgroupMemoryExplicitLayout8BitAccess = True
                            , ..
                            }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "WorkgroupMemoryExplicitLayout16BitAccessKHR" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "workgroupMemoryExplicitLayout16BitAccess"
      , checkFeature  =
        \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR { workgroupMemoryExplicitLayout16BitAccess } ->
          workgroupMemoryExplicitLayout16BitAccess
      , enableFeature = \PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR {..} ->
                          PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
                            { workgroupMemoryExplicitLayout16BitAccess = True
                            , ..
                            }
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DotProductInputAllKHR" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 3 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderIntegerDotProduct"
      , checkFeature  = \PhysicalDeviceVulkan13Features { shaderIntegerDotProduct } -> shaderIntegerDotProduct
      , enableFeature = \PhysicalDeviceVulkan13Features {..} ->
                          PhysicalDeviceVulkan13Features { shaderIntegerDotProduct = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 3 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DotProductInput4x8BitKHR" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 3 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderIntegerDotProduct"
      , checkFeature  = \PhysicalDeviceVulkan13Features { shaderIntegerDotProduct } -> shaderIntegerDotProduct
      , enableFeature = \PhysicalDeviceVulkan13Features {..} ->
                          PhysicalDeviceVulkan13Features { shaderIntegerDotProduct = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 3 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DotProductInput4x8BitPackedKHR" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 3 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderIntegerDotProduct"
      , checkFeature  = \PhysicalDeviceVulkan13Features { shaderIntegerDotProduct } -> shaderIntegerDotProduct
      , enableFeature = \PhysicalDeviceVulkan13Features {..} ->
                          PhysicalDeviceVulkan13Features { shaderIntegerDotProduct = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 3 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DotProductKHR" -> (,)
    [ RequireInstanceVersion $ MAKE_API_VERSION 1 3 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderIntegerDotProduct"
      , checkFeature  = \PhysicalDeviceVulkan13Features { shaderIntegerDotProduct } -> shaderIntegerDotProduct
      , enableFeature = \PhysicalDeviceVulkan13Features {..} ->
                          PhysicalDeviceVulkan13Features { shaderIntegerDotProduct = True, .. }
      }
    , RequireDeviceVersion $ MAKE_API_VERSION 1 3 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  _ -> ([], [])

