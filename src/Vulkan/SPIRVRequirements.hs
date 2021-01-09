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
import Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT(..))
import Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV(..))
import Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV(..))
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures(..))
import Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT(..))
import Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
import Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..))
import Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT(..))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR)
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR)
import Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR(..))
import Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT(..))
import Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV(..))
import Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL(..))
import Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV(..))
import Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV(..))
import Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import Vulkan.Core12 (PhysicalDeviceVulkan11Features(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import Vulkan.Core12 (PhysicalDeviceVulkan12Features(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Properties)
import Vulkan.Core12 (PhysicalDeviceVulkan12Properties(..))
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
import Vulkan.Extensions.VK_EXT_shader_atomic_float (pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (pattern EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_stencil_export (pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_subgroup_ballot (pattern EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_subgroup_vote (pattern EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_shader_viewport_index_layer (pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME)
import Vulkan.Extensions.VK_EXT_transform_feedback (pattern EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME)
import Vulkan.Extensions.VK_GOOGLE_decorate_string (pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME)
import Vulkan.Extensions.VK_GOOGLE_hlsl_functionality1 (pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME)
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
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2 (pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_maintenance2 (pattern KHR_MAINTENANCE2_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_maintenance3 (pattern KHR_MAINTENANCE3_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_multiview (pattern KHR_MULTIVIEW_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_ray_query (pattern KHR_RAY_QUERY_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_atomic_int64 (pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_clock (pattern KHR_SHADER_CLOCK_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_draw_parameters (pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float16_int8 (pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float_controls (pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info (pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_terminate_invocation (pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_spirv_1_4 (pattern KHR_SPIRV_1_4_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_storage_buffer_storage_class (pattern KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_variable_pointers (pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_vulkan_memory_model (pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME)
import Vulkan.Version (pattern MAKE_VERSION)
import Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_compute_shader_derivatives (pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_cooperative_matrix (pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_fragment_shader_barycentric (pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_geometry_shader_passthrough (pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_mesh_shader (pattern NV_MESH_SHADER_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_ray_tracing (pattern NV_RAY_TRACING_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_sample_mask_override_coverage (pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_image_footprint (pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_sm_builtins (pattern NV_SHADER_SM_BUILTINS_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shader_subgroup_partitioned (pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_shading_rate_image (pattern NV_SHADING_RATE_IMAGE_EXTENSION_NAME)
import Vulkan.Extensions.VK_NV_viewport_array2 (pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME)
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_VERSION 1 1 0]
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_VERSION 1 1 0]
  "SPV_KHR_8bit_storage" ->
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
  "SPV_KHR_16bit_storage" ->
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_VERSION 1 1 0]
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
  "SPV_KHR_storage_buffer_storage_class" ->
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_VERSION 1 1 0]
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
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY2_EXTENSION_NAME
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
  "SPV_KHR_vulkan_memory_model" ->
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
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
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
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
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_GOOGLE_hlsl_functionality1" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
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
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
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
  "SPV_EXT_demote_to_helper_invocation" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
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
                             , deviceExtensionName       = KHR_MAINTENANCE2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SPV_KHR_non_semantic_info" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
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
  "SPV_KHR_terminate_invocation" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  _ -> ([], [])

spirvCapabilityRequirements :: ByteString -> ([InstanceRequirement], [DeviceRequirement])
spirvCapabilityRequirements = \case
  "Matrix"            -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "Shader"            -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "InputAttachment"   -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "Sampled1D"         -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "Image1D"           -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "SampledBuffer"     -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "ImageBuffer"       -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "ImageQuery"        -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "DerivativeControl" -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "Geometry"          -> (,)
    []
    [ RequireDeviceFeature { featureName   = "geometryShader"
                           , checkFeature  = geometryShader :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { geometryShader = True } :: PhysicalDeviceFeatures
                           }
    ]
  "Tessellation" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "tessellationShader"
                           , checkFeature  = tessellationShader :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { tessellationShader = True } :: PhysicalDeviceFeatures
                           }
    ]
  "Float64" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderFloat64"
                           , checkFeature  = shaderFloat64 :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderFloat64 = True } :: PhysicalDeviceFeatures
                           }
    ]
  "Int64" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderInt64"
                           , checkFeature  = shaderInt64 :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderInt64 = True } :: PhysicalDeviceFeatures
                           }
    ]
  "Int64Atomics" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderBufferInt64Atomics"
      , checkFeature  = shaderBufferInt64Atomics :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderBufferInt64Atomics = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
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
      , checkFeature  = shaderBufferFloat32AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderBufferFloat32AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
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
      , checkFeature  = shaderBufferFloat64AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderBufferFloat64AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
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
      , checkFeature  = shaderImageInt64Atomics :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderImageInt64Atomics = True } :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Int16" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderInt16"
                           , checkFeature  = shaderInt16 :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderInt16 = True } :: PhysicalDeviceFeatures
                           }
    ]
  "TessellationPointSize" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderTessellationAndGeometryPointSize"
        , checkFeature  = shaderTessellationAndGeometryPointSize :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderTessellationAndGeometryPointSize = True } :: PhysicalDeviceFeatures
        }
    ]
  "GeometryPointSize" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderTessellationAndGeometryPointSize"
        , checkFeature  = shaderTessellationAndGeometryPointSize :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderTessellationAndGeometryPointSize = True } :: PhysicalDeviceFeatures
        }
    ]
  "ImageGatherExtended" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderImageGatherExtended"
                           , checkFeature  = shaderImageGatherExtended :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderImageGatherExtended = True } :: PhysicalDeviceFeatures
                           }
    ]
  "StorageImageMultisample" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderStorageImageMultisample"
                           , checkFeature  = shaderStorageImageMultisample :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderStorageImageMultisample = True } :: PhysicalDeviceFeatures
                           }
    ]
  "UniformBufferArrayDynamicIndexing" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderUniformBufferArrayDynamicIndexing"
        , checkFeature  = shaderUniformBufferArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderUniformBufferArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        }
    ]
  "SampledImageArrayDynamicIndexing" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderSampledImageArrayDynamicIndexing"
        , checkFeature  = shaderSampledImageArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderSampledImageArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        }
    ]
  "StorageBufferArrayDynamicIndexing" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderStorageBufferArrayDynamicIndexing"
        , checkFeature  = shaderStorageBufferArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderStorageBufferArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        }
    ]
  "StorageImageArrayDynamicIndexing" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderStorageImageArrayDynamicIndexing"
        , checkFeature  = shaderStorageImageArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderStorageImageArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        }
    ]
  "ClipDistance" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderClipDistance"
                           , checkFeature  = shaderClipDistance :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderClipDistance = True } :: PhysicalDeviceFeatures
                           }
    ]
  "CullDistance" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderCullDistance"
                           , checkFeature  = shaderCullDistance :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderCullDistance = True } :: PhysicalDeviceFeatures
                           }
    ]
  "ImageCubeArray" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "imageCubeArray"
                           , checkFeature  = imageCubeArray :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { imageCubeArray = True } :: PhysicalDeviceFeatures
                           }
    ]
  "SampleRateShading" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "sampleRateShading"
                           , checkFeature  = sampleRateShading :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { sampleRateShading = True } :: PhysicalDeviceFeatures
                           }
    ]
  "SparseResidency" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderResourceResidency"
                           , checkFeature  = shaderResourceResidency :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderResourceResidency = True } :: PhysicalDeviceFeatures
                           }
    ]
  "MinLod" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderResourceMinLod"
                           , checkFeature  = shaderResourceMinLod :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderResourceMinLod = True } :: PhysicalDeviceFeatures
                           }
    ]
  "SampledCubeArray" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "imageCubeArray"
                           , checkFeature  = imageCubeArray :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { imageCubeArray = True } :: PhysicalDeviceFeatures
                           }
    ]
  "ImageMSArray" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "shaderStorageImageMultisample"
                           , checkFeature  = shaderStorageImageMultisample :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { shaderStorageImageMultisample = True } :: PhysicalDeviceFeatures
                           }
    ]
  "StorageImageExtendedFormats" ->
    (,) [RequireInstanceVersion $ MAKE_VERSION 1 0 0] [RequireDeviceVersion $ MAKE_VERSION 1 0 0]
  "InterpolationFunction" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "sampleRateShading"
                           , checkFeature  = sampleRateShading :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { sampleRateShading = True } :: PhysicalDeviceFeatures
                           }
    ]
  "StorageImageReadWithoutFormat" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderStorageImageReadWithoutFormat"
        , checkFeature  = shaderStorageImageReadWithoutFormat :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderStorageImageReadWithoutFormat = True } :: PhysicalDeviceFeatures
        }
    ]
  "StorageImageWriteWithoutFormat" -> (,)
    []
    [ RequireDeviceFeature
        { featureName   = "shaderStorageImageWriteWithoutFormat"
        , checkFeature  = shaderStorageImageWriteWithoutFormat :: PhysicalDeviceFeatures -> Bool
        , enableFeature = \f -> f { shaderStorageImageWriteWithoutFormat = True } :: PhysicalDeviceFeatures
        }
    ]
  "MultiViewport" -> (,)
    []
    [ RequireDeviceFeature { featureName   = "multiViewport"
                           , checkFeature  = multiViewport :: PhysicalDeviceFeatures -> Bool
                           , enableFeature = \f -> f { multiViewport = True } :: PhysicalDeviceFeatures
                           }
    ]
  "DrawParameters" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceFeature { featureName   = "shaderDrawParameters"
                           , checkFeature  = shaderDrawParameters :: PhysicalDeviceVulkan11Features -> Bool
                           , enableFeature = \f -> f { shaderDrawParameters = True } :: PhysicalDeviceVulkan11Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "MultiView" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "multiview"
                           , checkFeature  = multiview :: PhysicalDeviceVulkan11Features -> Bool
                           , enableFeature = \f -> f { multiview = True } :: PhysicalDeviceVulkan11Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MULTIVIEW_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DeviceGroup" -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 1 0] [RequireDeviceVersion $ MAKE_VERSION 1 1 0]
  "VariablePointersStorageBuffer" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "variablePointersStorageBuffer"
      , checkFeature  = variablePointersStorageBuffer :: PhysicalDeviceVulkan11Features -> Bool
      , enableFeature = \f -> f { variablePointersStorageBuffer = True } :: PhysicalDeviceVulkan11Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VARIABLE_POINTERS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "VariablePointers" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "variablePointers"
                           , checkFeature  = variablePointers :: PhysicalDeviceVulkan11Features -> Bool
                           , enableFeature = \f -> f { variablePointers = True } :: PhysicalDeviceVulkan11Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VARIABLE_POINTERS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
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
    [RequireInstanceVersion $ MAKE_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "shaderOutputViewportIndex"
      , checkFeature  = shaderOutputViewportIndex :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderOutputViewportIndex = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    ]
  "ShaderLayer" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 2 0]
    [ RequireDeviceFeature { featureName   = "shaderOutputLayer"
                           , checkFeature  = shaderOutputLayer :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { shaderOutputLayer = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "ShaderViewportMaskNV" -> (,)
    []
    [ RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = NV_VIEWPORT_ARRAY2_EXTENSION_NAME
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
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "storageBuffer16BitAccess"
      , checkFeature  = storageBuffer16BitAccess :: PhysicalDeviceVulkan11Features -> Bool
      , enableFeature = \f -> f { storageBuffer16BitAccess = True } :: PhysicalDeviceVulkan11Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_16BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformAndStorageBuffer16BitAccess" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "uniformAndStorageBuffer16BitAccess"
      , checkFeature  = uniformAndStorageBuffer16BitAccess :: PhysicalDeviceVulkan11Features -> Bool
      , enableFeature = \f -> f { uniformAndStorageBuffer16BitAccess = True } :: PhysicalDeviceVulkan11Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_16BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StoragePushConstant16" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "storagePushConstant16"
                           , checkFeature  = storagePushConstant16 :: PhysicalDeviceVulkan11Features -> Bool
                           , enableFeature = \f -> f { storagePushConstant16 = True } :: PhysicalDeviceVulkan11Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_16BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageInputOutput16" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 1 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "storageInputOutput16"
                           , checkFeature  = storageInputOutput16 :: PhysicalDeviceVulkan11Features -> Bool
                           , enableFeature = \f -> f { storageInputOutput16 = True } :: PhysicalDeviceVulkan11Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_16BIT_STORAGE_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "GroupNonUniform" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_BASIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformVote" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_VOTE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformArithmetic" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_ARITHMETIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformBallot" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_BALLOT_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformShuffle" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_SHUFFLE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformShuffleRelative" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformClustered" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_CLUSTERED_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
    ]
  "GroupNonUniformQuad" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 1 0]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan11Properties"
      , checkProperty = \p ->
        SUBGROUP_FEATURE_QUAD_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 1 0
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
  "ShaderNonUniform" -> (,) [RequireInstanceVersion $ MAKE_VERSION 1 2 0] [RequireDeviceVersion $ MAKE_VERSION 1 2 0]
  "RuntimeDescriptorArray" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "runtimeDescriptorArray"
                           , checkFeature  = runtimeDescriptorArray :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { runtimeDescriptorArray = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "InputAttachmentArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderInputAttachmentArrayDynamicIndexing"
      , checkFeature  = shaderInputAttachmentArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderInputAttachmentArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformTexelBufferArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformTexelBufferArrayDynamicIndexing"
      , checkFeature  = shaderUniformTexelBufferArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f ->
                          f { shaderUniformTexelBufferArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageTexelBufferArrayDynamicIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageTexelBufferArrayDynamicIndexing"
      , checkFeature  = shaderStorageTexelBufferArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f ->
                          f { shaderStorageTexelBufferArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformBufferArrayNonUniformIndexing"
      , checkFeature  = shaderUniformBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderUniformBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SampledImageArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderSampledImageArrayNonUniformIndexing"
      , checkFeature  = shaderSampledImageArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderSampledImageArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageBufferArrayNonUniformIndexing"
      , checkFeature  = shaderStorageBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderStorageBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageImageArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageImageArrayNonUniformIndexing"
      , checkFeature  = shaderStorageImageArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { shaderStorageImageArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "InputAttachmentArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderInputAttachmentArrayNonUniformIndexing"
      , checkFeature  = shaderInputAttachmentArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f ->
                          f { shaderInputAttachmentArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "UniformTexelBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderUniformTexelBufferArrayNonUniformIndexing"
      , checkFeature  = shaderUniformTexelBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f ->
                          f { shaderUniformTexelBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageTexelBufferArrayNonUniformIndexing" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "shaderStorageTexelBufferArrayNonUniformIndexing"
      , checkFeature  = shaderStorageTexelBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f ->
                          f { shaderStorageTexelBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Float16" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "shaderFloat16"
                           , checkFeature  = shaderFloat16 :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { shaderFloat16 = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "Int8" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "shaderInt8"
                           , checkFeature  = shaderInt8 :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { shaderInt8 = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "StorageBuffer8BitAccess" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "storageBuffer8BitAccess"
      , checkFeature  = storageBuffer8BitAccess :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { storageBuffer8BitAccess = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "uniformAndStorageBuffer8BitAccess"
      , checkFeature  = uniformAndStorageBuffer8BitAccess :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { uniformAndStorageBuffer8BitAccess = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "storagePushConstant8"
                           , checkFeature  = storagePushConstant8 :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { storagePushConstant8 = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
    [RequireInstanceVersion $ MAKE_VERSION 1 2 0]
    [ RequireDeviceFeature { featureName   = "vulkanMemoryModel"
                           , checkFeature  = vulkanMemoryModel :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { vulkanMemoryModel = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "VulkanMemoryModelDeviceScope" -> (,)
    [RequireInstanceVersion $ MAKE_VERSION 1 2 0]
    [ RequireDeviceFeature
      { featureName   = "vulkanMemoryModelDeviceScope"
      , checkFeature  = vulkanMemoryModelDeviceScope :: PhysicalDeviceVulkan12Features -> Bool
      , enableFeature = \f -> f { vulkanMemoryModelDeviceScope = True } :: PhysicalDeviceVulkan12Features
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DenormPreserve" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderDenormPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DenormFlushToZero" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty = \p -> shaderDenormFlushToZeroFloat16 (p :: PhysicalDeviceVulkan12Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "SignedZeroInfNanPreserve" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty
      { propertyName  = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty = \p -> shaderSignedZeroInfNanPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
      }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RoundingModeRTE" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderRoundingModeRTEFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "RoundingModeRTZ" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceProperty { propertyName  = "VkPhysicalDeviceVulkan12Properties"
                            , checkProperty = \p -> shaderRoundingModeRTZFloat16 (p :: PhysicalDeviceVulkan12Properties)
                            }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
      , checkFeature  = computeDerivativeGroupQuads :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> Bool
      , enableFeature = \f ->
                          f { computeDerivativeGroupQuads = True } :: PhysicalDeviceComputeShaderDerivativesFeaturesNV
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
      , checkFeature  = computeDerivativeGroupLinear :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> Bool
      , enableFeature = \f ->
                          f { computeDerivativeGroupLinear = True } :: PhysicalDeviceComputeShaderDerivativesFeaturesNV
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
      , checkFeature  = fragmentShaderBarycentric :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV -> Bool
      , enableFeature = \f ->
                          f { fragmentShaderBarycentric = True } :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV
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
      , checkFeature  = imageFootprint :: PhysicalDeviceShaderImageFootprintFeaturesNV -> Bool
      , enableFeature = \f -> f { imageFootprint = True } :: PhysicalDeviceShaderImageFootprintFeaturesNV
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
      , checkFeature  = shadingRateImage :: PhysicalDeviceShadingRateImageFeaturesNV -> Bool
      , enableFeature = \f -> f { shadingRateImage = True } :: PhysicalDeviceShadingRateImageFeaturesNV
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
      , checkFeature  = rayTracingPipeline :: PhysicalDeviceRayTracingPipelineFeaturesKHR -> Bool
      , enableFeature = \f -> f { rayTracingPipeline = True } :: PhysicalDeviceRayTracingPipelineFeaturesKHR
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
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
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
                           , checkFeature  = rayQuery :: PhysicalDeviceRayQueryFeaturesKHR -> Bool
                           , enableFeature = \f -> f { rayQuery = True } :: PhysicalDeviceRayQueryFeaturesKHR
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
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
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
      , checkFeature  = rayTraversalPrimitiveCulling :: PhysicalDeviceRayTracingPipelineFeaturesKHR -> Bool
      , enableFeature = \f -> f { rayTraversalPrimitiveCulling = True } :: PhysicalDeviceRayTracingPipelineFeaturesKHR
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
                             , deviceExtensionName       = KHR_MAINTENANCE3_EXTENSION_NAME
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
  "TransformFeedback" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName   = "transformFeedback"
      , checkFeature  = transformFeedback :: PhysicalDeviceTransformFeedbackFeaturesEXT -> Bool
      , enableFeature = \f -> f { transformFeedback = True } :: PhysicalDeviceTransformFeedbackFeaturesEXT
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
      , checkFeature  = geometryStreams :: PhysicalDeviceTransformFeedbackFeaturesEXT -> Bool
      , enableFeature = \f -> f { geometryStreams = True } :: PhysicalDeviceTransformFeedbackFeaturesEXT
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
      , checkFeature  = fragmentDensityMap :: PhysicalDeviceFragmentDensityMapFeaturesEXT -> Bool
      , enableFeature = \f -> f { fragmentDensityMap = True } :: PhysicalDeviceFragmentDensityMapFeaturesEXT
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "PhysicalStorageBufferAddresses" -> (,)
    [ RequireInstanceVersion $ MAKE_VERSION 1 2 0
    , RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature { featureName   = "bufferDeviceAddress"
                           , checkFeature  = bufferDeviceAddress :: PhysicalDeviceVulkan12Features -> Bool
                           , enableFeature = \f -> f { bufferDeviceAddress = True } :: PhysicalDeviceVulkan12Features
                           }
    , RequireDeviceVersion $ MAKE_VERSION 1 2 0
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
      , checkFeature  = cooperativeMatrix :: PhysicalDeviceCooperativeMatrixFeaturesNV -> Bool
      , enableFeature = \f -> f { cooperativeMatrix = True } :: PhysicalDeviceCooperativeMatrixFeaturesNV
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
      , checkFeature  = shaderIntegerFunctions2 :: PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL -> Bool
      , enableFeature = \f -> f { shaderIntegerFunctions2 = True } :: PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
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
      , checkFeature  = shaderSMBuiltins :: PhysicalDeviceShaderSMBuiltinsFeaturesNV -> Bool
      , enableFeature = \f -> f { shaderSMBuiltins = True } :: PhysicalDeviceShaderSMBuiltinsFeaturesNV
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
      , checkFeature  = fragmentShaderSampleInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
      , enableFeature = \f ->
                          f { fragmentShaderSampleInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
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
      , checkFeature  = fragmentShaderPixelInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
      , enableFeature = \f ->
                          f { fragmentShaderPixelInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
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
      , checkFeature  = fragmentShaderShadingRateInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
      , enableFeature = \f ->
        f { fragmentShaderShadingRateInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
      }
    , RequireDeviceExtension { deviceExtensionLayerName  = Nothing
                             , deviceExtensionName       = EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  "DemoteToHelperInvocationEXT" -> (,)
    [ RequireInstanceExtension { instanceExtensionLayerName  = Nothing
                               , instanceExtensionName       = KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                               , instanceExtensionMinVersion = 0
                               }
    ]
    [ RequireDeviceFeature
      { featureName = "shaderDemoteToHelperInvocation"
      , checkFeature = shaderDemoteToHelperInvocation :: PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT -> Bool
      , enableFeature = \f ->
        f { shaderDemoteToHelperInvocation = True } :: PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
      }
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
      , checkFeature  = pipelineFragmentShadingRate :: PhysicalDeviceFragmentShadingRateFeaturesKHR -> Bool
      , enableFeature = \f -> f { pipelineFragmentShadingRate = True } :: PhysicalDeviceFragmentShadingRateFeaturesKHR
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
                             , deviceExtensionName       = KHR_MAINTENANCE2_EXTENSION_NAME
                             , deviceExtensionMinVersion = 0
                             }
    ]
  _ -> ([], [])

