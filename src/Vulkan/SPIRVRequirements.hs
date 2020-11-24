{-# language CPP #-}
{-# language OverloadedLists #-}
module Vulkan.SPIRVRequirements  ( spirvExtensionRequirements
                                 , spirvCapabilityRequirements
                                 ) where

import Vulkan.Requirements (Requirement)
import Vulkan.Requirements (Requirement(..))
import Data.Bits ((.&.))
import Data.Bits (zeroBits)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
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
import Vulkan.Extensions.VK_KHR_ray_tracing (PhysicalDeviceRayTracingFeaturesKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (PhysicalDeviceRayTracingFeaturesKHR(..))
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
import Vulkan.Extensions.VK_KHR_buffer_device_address (pattern KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_device_group (pattern KHR_DEVICE_GROUP_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_multiview (pattern KHR_MULTIVIEW_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_ray_tracing (pattern KHR_RAY_TRACING_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_atomic_int64 (pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_clock (pattern KHR_SHADER_CLOCK_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_draw_parameters (pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float16_int8 (pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_float_controls (pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_non_semantic_info (pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME)
import Vulkan.Extensions.VK_KHR_shader_terminate_invocation (pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME)
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

spirvExtensionRequirements :: ByteString -> [Requirement]
spirvExtensionRequirements = \case
  "SPV_KHR_variable_pointers" ->
    [RequireVersion $ MAKE_VERSION 1 1 0, RequireExtension KHR_VARIABLE_POINTERS_EXTENSION_NAME]
  "SPV_AMD_shader_explicit_vertex_parameter" -> [RequireExtension AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME]
  "SPV_AMD_gcn_shader"                       -> [RequireExtension AMD_GCN_SHADER_EXTENSION_NAME]
  "SPV_AMD_gpu_shader_half_float"            -> [RequireExtension AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME]
  "SPV_AMD_gpu_shader_int16"                 -> [RequireExtension AMD_GPU_SHADER_INT16_EXTENSION_NAME]
  "SPV_AMD_shader_ballot"                    -> [RequireExtension AMD_SHADER_BALLOT_EXTENSION_NAME]
  "SPV_AMD_shader_fragment_mask"             -> [RequireExtension AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME]
  "SPV_AMD_shader_image_load_store_lod"      -> [RequireExtension AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME]
  "SPV_AMD_shader_trinary_minmax"            -> [RequireExtension AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME]
  "SPV_AMD_texture_gather_bias_lod"          -> [RequireExtension AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME]
  "SPV_KHR_shader_draw_parameters" ->
    [RequireVersion $ MAKE_VERSION 1 1 0, RequireExtension KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME]
  "SPV_KHR_8bit_storage"  -> [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension KHR_8BIT_STORAGE_EXTENSION_NAME]
  "SPV_KHR_16bit_storage" -> [RequireVersion $ MAKE_VERSION 1 1 0, RequireExtension KHR_16BIT_STORAGE_EXTENSION_NAME]
  "SPV_KHR_shader_clock"  -> [RequireExtension KHR_SHADER_CLOCK_EXTENSION_NAME]
  "SPV_KHR_float_controls" ->
    [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
  "SPV_KHR_storage_buffer_storage_class" ->
    [RequireVersion $ MAKE_VERSION 1 1 0, RequireExtension KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME]
  "SPV_KHR_post_depth_coverage"          -> [RequireExtension EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME]
  "SPV_EXT_shader_stencil_export"        -> [RequireExtension EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME]
  "SPV_KHR_shader_ballot"                -> [RequireExtension EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME]
  "SPV_KHR_subgroup_vote"                -> [RequireExtension EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME]
  "SPV_NV_sample_mask_override_coverage" -> [RequireExtension NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME]
  "SPV_NV_geometry_shader_passthrough"   -> [RequireExtension NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME]
  "SPV_NV_mesh_shader"                   -> [RequireExtension NV_MESH_SHADER_EXTENSION_NAME]
  "SPV_NV_viewport_array2"               -> [RequireExtension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "SPV_NV_shader_subgroup_partitioned"   -> [RequireExtension NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME]
  "SPV_EXT_shader_viewport_index_layer" ->
    [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME]
  "SPV_NVX_multiview_per_view_attributes" -> [RequireExtension NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME]
  "SPV_EXT_descriptor_indexing" ->
    [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
  "SPV_KHR_vulkan_memory_model" ->
    [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
  "SPV_NV_compute_shader_derivatives"   -> [RequireExtension NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
  "SPV_NV_fragment_shader_barycentric"  -> [RequireExtension NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME]
  "SPV_NV_shader_image_footprint"       -> [RequireExtension NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME]
  "SPV_NV_shading_rate"                 -> [RequireExtension NV_SHADING_RATE_IMAGE_EXTENSION_NAME]
  "SPV_NV_ray_tracing"                  -> [RequireExtension NV_RAY_TRACING_EXTENSION_NAME]
  "SPV_KHR_ray_tracing"                 -> [RequireExtension KHR_RAY_TRACING_EXTENSION_NAME]
  "SPV_KHR_ray_query"                   -> [RequireExtension KHR_RAY_TRACING_EXTENSION_NAME]
  "SPV_GOOGLE_hlsl_functionality1"      -> [RequireExtension GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME]
  "SPV_GOOGLE_user_type"                -> [RequireExtension GOOGLE_USER_TYPE_EXTENSION_NAME]
  "SPV_GOOGLE_decorate_string"          -> [RequireExtension GOOGLE_DECORATE_STRING_EXTENSION_NAME]
  "SPV_EXT_fragment_invocation_density" -> [RequireExtension EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME]
  "SPV_KHR_physical_storage_buffer" ->
    [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
  "SPV_EXT_physical_storage_buffer" -> [RequireExtension EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
  "SPV_NV_cooperative_matrix"     -> [RequireExtension NV_COOPERATIVE_MATRIX_EXTENSION_NAME]
  "SPV_NV_shader_sm_builtins"     -> [RequireExtension NV_SHADER_SM_BUILTINS_EXTENSION_NAME]
  "SPV_EXT_fragment_shader_interlock" -> [RequireExtension EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
  "SPV_EXT_demote_to_helper_invocation" -> [RequireExtension EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME]
  "SPV_KHR_fragment_shading_rate" -> [RequireExtension KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
  "SPV_KHR_non_semantic_info"     -> [RequireExtension KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME]
  "SPV_EXT_shader_image_int64"    -> [RequireExtension EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME]
  "SPV_KHR_terminate_invocation"  -> [RequireExtension KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME]
  _                               -> []

spirvCapabilityRequirements :: ByteString -> [Requirement]
spirvCapabilityRequirements = \case
  "Matrix"            -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "Shader"            -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "InputAttachment"   -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "Sampled1D"         -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "Image1D"           -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "SampledBuffer"     -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "ImageBuffer"       -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "ImageQuery"        -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "DerivativeControl" -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "Geometry" ->
    [ RequireFeature { featureName       = "geometryShader"
                     , checkFeature      = geometryShader :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { geometryShader = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "Tessellation" ->
    [ RequireFeature { featureName       = "tessellationShader"
                     , checkFeature      = tessellationShader :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { tessellationShader = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "Float64" ->
    [ RequireFeature { featureName       = "shaderFloat64"
                     , checkFeature      = shaderFloat64 :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderFloat64 = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "Int64" ->
    [ RequireFeature { featureName       = "shaderInt64"
                     , checkFeature      = shaderInt64 :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderInt64 = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "Int64Atomics" ->
    [ RequireFeature { featureName       = "shaderBufferInt64Atomics"
                     , checkFeature      = shaderBufferInt64Atomics :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { shaderBufferInt64Atomics = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME]
                     }
    , RequireFeature { featureName       = "shaderSharedInt64Atomics"
                     , checkFeature      = shaderSharedInt64Atomics :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { shaderSharedInt64Atomics = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME]
                     }
    ]
  "AtomicFloat32AddEXT" ->
    [ RequireFeature
      { featureName       = "shaderBufferFloat32AtomicAdd"
      , checkFeature      = shaderBufferFloat32AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderBufferFloat32AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "shaderSharedFloat32AtomicAdd"
      , checkFeature      = shaderSharedFloat32AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderSharedFloat32AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "shaderImageFloat32AtomicAdd"
      , checkFeature      = shaderImageFloat32AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature     = \f -> f { shaderImageFloat32AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "sparseImageFloat32AtomicAdd"
      , checkFeature      = sparseImageFloat32AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature     = \f -> f { sparseImageFloat32AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    ]
  "AtomicFloat64AddEXT" ->
    [ RequireFeature
      { featureName       = "shaderBufferFloat64AtomicAdd"
      , checkFeature      = shaderBufferFloat64AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderBufferFloat64AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "shaderSharedFloat64AtomicAdd"
      , checkFeature      = shaderSharedFloat64AtomicAdd :: PhysicalDeviceShaderAtomicFloatFeaturesEXT -> Bool
      , enableFeature = \f -> f { shaderSharedFloat64AtomicAdd = True } :: PhysicalDeviceShaderAtomicFloatFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
      }
    ]
  "Int64ImageEXT" ->
    [ RequireFeature
        { featureName       = "shaderImageInt64Atomics"
        , checkFeature      = shaderImageInt64Atomics :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT -> Bool
        , enableFeature = \f -> f { shaderImageInt64Atomics = True } :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME]
        }
    ]
  "Int16" ->
    [ RequireFeature { featureName       = "shaderInt16"
                     , checkFeature      = shaderInt16 :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderInt16 = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "TessellationPointSize" ->
    [ RequireFeature
        { featureName       = "shaderTessellationAndGeometryPointSize"
        , checkFeature      = shaderTessellationAndGeometryPointSize :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderTessellationAndGeometryPointSize = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "GeometryPointSize" ->
    [ RequireFeature
        { featureName       = "shaderTessellationAndGeometryPointSize"
        , checkFeature      = shaderTessellationAndGeometryPointSize :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderTessellationAndGeometryPointSize = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "ImageGatherExtended" ->
    [ RequireFeature { featureName       = "shaderImageGatherExtended"
                     , checkFeature      = shaderImageGatherExtended :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderImageGatherExtended = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "StorageImageMultisample" ->
    [ RequireFeature { featureName       = "shaderStorageImageMultisample"
                     , checkFeature      = shaderStorageImageMultisample :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderStorageImageMultisample = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "UniformBufferArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderUniformBufferArrayDynamicIndexing"
        , checkFeature      = shaderUniformBufferArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderUniformBufferArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "SampledImageArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderSampledImageArrayDynamicIndexing"
        , checkFeature      = shaderSampledImageArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderSampledImageArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "StorageBufferArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageBufferArrayDynamicIndexing"
        , checkFeature      = shaderStorageBufferArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderStorageBufferArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "StorageImageArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageImageArrayDynamicIndexing"
        , checkFeature      = shaderStorageImageArrayDynamicIndexing :: PhysicalDeviceFeatures -> Bool
        , enableFeature     = \f -> f { shaderStorageImageArrayDynamicIndexing = True } :: PhysicalDeviceFeatures
        , requireMinVersion = Nothing
        , requireExtensions = []
        }
    ]
  "ClipDistance" ->
    [ RequireFeature { featureName       = "shaderClipDistance"
                     , checkFeature      = shaderClipDistance :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderClipDistance = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "CullDistance" ->
    [ RequireFeature { featureName       = "shaderCullDistance"
                     , checkFeature      = shaderCullDistance :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderCullDistance = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "ImageCubeArray" ->
    [ RequireFeature { featureName       = "imageCubeArray"
                     , checkFeature      = imageCubeArray :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { imageCubeArray = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "SampleRateShading" ->
    [ RequireFeature { featureName       = "sampleRateShading"
                     , checkFeature      = sampleRateShading :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { sampleRateShading = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "SparseResidency" ->
    [ RequireFeature { featureName       = "shaderResourceResidency"
                     , checkFeature      = shaderResourceResidency :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderResourceResidency = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "MinLod" ->
    [ RequireFeature { featureName       = "shaderResourceMinLod"
                     , checkFeature      = shaderResourceMinLod :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderResourceMinLod = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "SampledCubeArray" ->
    [ RequireFeature { featureName       = "imageCubeArray"
                     , checkFeature      = imageCubeArray :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { imageCubeArray = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "ImageMSArray" ->
    [ RequireFeature { featureName       = "shaderStorageImageMultisample"
                     , checkFeature      = shaderStorageImageMultisample :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { shaderStorageImageMultisample = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "StorageImageExtendedFormats" -> [RequireVersion $ MAKE_VERSION 1 0 0]
  "InterpolationFunction" ->
    [ RequireFeature { featureName       = "sampleRateShading"
                     , checkFeature      = sampleRateShading :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { sampleRateShading = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "StorageImageReadWithoutFormat" ->
    [ RequireFeature { featureName       = "shaderStorageImageReadWithoutFormat"
                     , checkFeature      = shaderStorageImageReadWithoutFormat :: PhysicalDeviceFeatures -> Bool
                     , enableFeature = \f -> f { shaderStorageImageReadWithoutFormat = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "StorageImageWriteWithoutFormat" ->
    [ RequireFeature { featureName       = "shaderStorageImageWriteWithoutFormat"
                     , checkFeature      = shaderStorageImageWriteWithoutFormat :: PhysicalDeviceFeatures -> Bool
                     , enableFeature = \f -> f { shaderStorageImageWriteWithoutFormat = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "MultiViewport" ->
    [ RequireFeature { featureName       = "multiViewport"
                     , checkFeature      = multiViewport :: PhysicalDeviceFeatures -> Bool
                     , enableFeature     = \f -> f { multiViewport = True } :: PhysicalDeviceFeatures
                     , requireMinVersion = Nothing
                     , requireExtensions = []
                     }
    ]
  "DrawParameters" ->
    [ RequireFeature { featureName       = "shaderDrawParameters"
                     , checkFeature      = shaderDrawParameters :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { shaderDrawParameters = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = []
                     }
    , RequireExtension KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
    ]
  "MultiView" ->
    [ RequireFeature { featureName       = "multiview"
                     , checkFeature      = multiview :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { multiview = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = [KHR_MULTIVIEW_EXTENSION_NAME]
                     }
    ]
  "DeviceGroup" -> [RequireVersion $ MAKE_VERSION 1 1 0, RequireExtension KHR_DEVICE_GROUP_EXTENSION_NAME]
  "VariablePointersStorageBuffer" ->
    [ RequireFeature
        { featureName       = "variablePointersStorageBuffer"
        , checkFeature      = variablePointersStorageBuffer :: PhysicalDeviceVulkan11Features -> Bool
        , enableFeature     = \f -> f { variablePointersStorageBuffer = True } :: PhysicalDeviceVulkan11Features
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = [KHR_VARIABLE_POINTERS_EXTENSION_NAME]
        }
    ]
  "VariablePointers" ->
    [ RequireFeature { featureName       = "variablePointers"
                     , checkFeature      = variablePointers :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { variablePointers = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = [KHR_VARIABLE_POINTERS_EXTENSION_NAME]
                     }
    ]
  "ShaderClockKHR"               -> [RequireExtension KHR_SHADER_CLOCK_EXTENSION_NAME]
  "StencilExportEXT"             -> [RequireExtension EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME]
  "SubgroupBallotKHR"            -> [RequireExtension EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME]
  "SubgroupVoteKHR"              -> [RequireExtension EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME]
  "ImageReadWriteLodAMD"         -> [RequireExtension AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME]
  "ImageGatherBiasLodAMD"        -> [RequireExtension AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME]
  "FragmentMaskAMD"              -> [RequireExtension AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME]
  "SampleMaskOverrideCoverageNV" -> [RequireExtension NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME]
  "GeometryShaderPassthroughNV"  -> [RequireExtension NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME]
  "ShaderViewportIndex" ->
    [ RequireFeature { featureName       = "shaderOutputViewportIndex"
                     , checkFeature      = shaderOutputViewportIndex :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature = \f -> f { shaderOutputViewportIndex = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = []
                     }
    ]
  "ShaderLayer" ->
    [ RequireFeature { featureName       = "shaderOutputLayer"
                     , checkFeature      = shaderOutputLayer :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { shaderOutputLayer = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = []
                     }
    ]
  "ShaderViewportIndexLayerEXT" -> [RequireExtension EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME]
  "ShaderViewportIndexLayerNV"  -> [RequireExtension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "ShaderViewportMaskNV"        -> [RequireExtension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "PerViewAttributesNV"         -> [RequireExtension NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME]
  "StorageBuffer16BitAccess" ->
    [ RequireFeature { featureName       = "storageBuffer16BitAccess"
                     , checkFeature      = storageBuffer16BitAccess :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { storageBuffer16BitAccess = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = [KHR_16BIT_STORAGE_EXTENSION_NAME]
                     }
    ]
  "UniformAndStorageBuffer16BitAccess" ->
    [ RequireFeature
        { featureName       = "uniformAndStorageBuffer16BitAccess"
        , checkFeature      = uniformAndStorageBuffer16BitAccess :: PhysicalDeviceVulkan11Features -> Bool
        , enableFeature     = \f -> f { uniformAndStorageBuffer16BitAccess = True } :: PhysicalDeviceVulkan11Features
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = [KHR_16BIT_STORAGE_EXTENSION_NAME]
        }
    ]
  "StoragePushConstant16" ->
    [ RequireFeature { featureName       = "storagePushConstant16"
                     , checkFeature      = storagePushConstant16 :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { storagePushConstant16 = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = [KHR_16BIT_STORAGE_EXTENSION_NAME]
                     }
    ]
  "StorageInputOutput16" ->
    [ RequireFeature { featureName       = "storageInputOutput16"
                     , checkFeature      = storageInputOutput16 :: PhysicalDeviceVulkan11Features -> Bool
                     , enableFeature     = \f -> f { storageInputOutput16 = True } :: PhysicalDeviceVulkan11Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 1 0
                     , requireExtensions = [KHR_16BIT_STORAGE_EXTENSION_NAME]
                     }
    ]
  "GroupNonUniform" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_BASIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformVote" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_VOTE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformArithmetic" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_ARITHMETIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformBallot" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_BALLOT_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformShuffle" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_SHUFFLE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformShuffleRelative" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformClustered" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_CLUSTERED_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformQuad" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_QUAD_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Just $ MAKE_VERSION 1 1 0
        , requireExtensions = []
        }
    ]
  "GroupNonUniformPartitionedNV" ->
    [ RequireProperty
        { propertyName      = "VkPhysicalDeviceVulkan11Properties"
        , checkProperty     = \p ->
          SUBGROUP_FEATURE_PARTITIONED_BIT_NV .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
        , requireMinVersion = Nothing
        , requireExtensions = [NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME]
        }
    ]
  "SampleMaskPostDepthCoverage" -> [RequireExtension EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME]
  "ShaderNonUniform" -> [RequireVersion $ MAKE_VERSION 1 2 0, RequireExtension EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
  "RuntimeDescriptorArray" ->
    [ RequireFeature { featureName       = "runtimeDescriptorArray"
                     , checkFeature      = runtimeDescriptorArray :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { runtimeDescriptorArray = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
                     }
    ]
  "InputAttachmentArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderInputAttachmentArrayDynamicIndexing"
        , checkFeature      = shaderInputAttachmentArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature = \f -> f { shaderInputAttachmentArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "UniformTexelBufferArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderUniformTexelBufferArrayDynamicIndexing"
        , checkFeature      = shaderUniformTexelBufferArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
                                f { shaderUniformTexelBufferArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "StorageTexelBufferArrayDynamicIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageTexelBufferArrayDynamicIndexing"
        , checkFeature      = shaderStorageTexelBufferArrayDynamicIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
                                f { shaderStorageTexelBufferArrayDynamicIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "UniformBufferArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderUniformBufferArrayNonUniformIndexing"
        , checkFeature      = shaderUniformBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
                                f { shaderUniformBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "SampledImageArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderSampledImageArrayNonUniformIndexing"
        , checkFeature      = shaderSampledImageArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature = \f -> f { shaderSampledImageArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "StorageBufferArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageBufferArrayNonUniformIndexing"
        , checkFeature      = shaderStorageBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
                                f { shaderStorageBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "StorageImageArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageImageArrayNonUniformIndexing"
        , checkFeature      = shaderStorageImageArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature = \f -> f { shaderStorageImageArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "InputAttachmentArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderInputAttachmentArrayNonUniformIndexing"
        , checkFeature      = shaderInputAttachmentArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
                                f { shaderInputAttachmentArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "UniformTexelBufferArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderUniformTexelBufferArrayNonUniformIndexing"
        , checkFeature      = shaderUniformTexelBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
          f { shaderUniformTexelBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "StorageTexelBufferArrayNonUniformIndexing" ->
    [ RequireFeature
        { featureName       = "shaderStorageTexelBufferArrayNonUniformIndexing"
        , checkFeature      = shaderStorageTexelBufferArrayNonUniformIndexing :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f ->
          f { shaderStorageTexelBufferArrayNonUniformIndexing = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
        }
    ]
  "Float16" ->
    [ RequireFeature { featureName       = "shaderFloat16"
                     , checkFeature      = shaderFloat16 :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { shaderFloat16 = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME]
                     }
    , RequireExtension AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME
    ]
  "Int8" ->
    [ RequireFeature { featureName       = "shaderInt8"
                     , checkFeature      = shaderInt8 :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { shaderInt8 = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME]
                     }
    ]
  "StorageBuffer8BitAccess" ->
    [ RequireFeature { featureName       = "storageBuffer8BitAccess"
                     , checkFeature      = storageBuffer8BitAccess :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { storageBuffer8BitAccess = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_8BIT_STORAGE_EXTENSION_NAME]
                     }
    ]
  "UniformAndStorageBuffer8BitAccess" ->
    [ RequireFeature
        { featureName       = "uniformAndStorageBuffer8BitAccess"
        , checkFeature      = uniformAndStorageBuffer8BitAccess :: PhysicalDeviceVulkan12Features -> Bool
        , enableFeature     = \f -> f { uniformAndStorageBuffer8BitAccess = True } :: PhysicalDeviceVulkan12Features
        , requireMinVersion = Just $ MAKE_VERSION 1 2 0
        , requireExtensions = [KHR_8BIT_STORAGE_EXTENSION_NAME]
        }
    ]
  "StoragePushConstant8" ->
    [ RequireFeature { featureName       = "storagePushConstant8"
                     , checkFeature      = storagePushConstant8 :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { storagePushConstant8 = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_8BIT_STORAGE_EXTENSION_NAME]
                     }
    ]
  "VulkanMemoryModel" ->
    [ RequireFeature { featureName       = "vulkanMemoryModel"
                     , checkFeature      = vulkanMemoryModel :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { vulkanMemoryModel = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
                     }
    ]
  "VulkanMemoryModelDeviceScope" ->
    [ RequireFeature { featureName       = "vulkanMemoryModelDeviceScope"
                     , checkFeature      = vulkanMemoryModelDeviceScope :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature = \f -> f { vulkanMemoryModelDeviceScope = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
                     }
    ]
  "DenormPreserve" ->
    [ RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormPreserveFloat32 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormPreserveFloat64 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    ]
  "DenormFlushToZero" ->
    [ RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormFlushToZeroFloat16 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormFlushToZeroFloat32 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderDenormFlushToZeroFloat64 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    ]
  "SignedZeroInfNanPreserve" ->
    [ RequireProperty
      { propertyName      = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty     = \p -> shaderSignedZeroInfNanPreserveFloat16 (p :: PhysicalDeviceVulkan12Properties)
      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
      }
    , RequireProperty
      { propertyName      = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty     = \p -> shaderSignedZeroInfNanPreserveFloat32 (p :: PhysicalDeviceVulkan12Properties)
      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
      }
    , RequireProperty
      { propertyName      = "VkPhysicalDeviceVulkan12Properties"
      , checkProperty     = \p -> shaderSignedZeroInfNanPreserveFloat64 (p :: PhysicalDeviceVulkan12Properties)
      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
      }
    ]
  "RoundingModeRTE" ->
    [ RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTEFloat16 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTEFloat32 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTEFloat64 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    ]
  "RoundingModeRTZ" ->
    [ RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTZFloat16 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTZFloat32 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    , RequireProperty { propertyName      = "VkPhysicalDeviceVulkan12Properties"
                      , checkProperty     = \p -> shaderRoundingModeRTZFloat64 (p :: PhysicalDeviceVulkan12Properties)
                      , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                      , requireExtensions = [KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
                      }
    ]
  "ComputeDerivativeGroupQuadsNV" ->
    [ RequireFeature
        { featureName       = "computeDerivativeGroupQuads"
        , checkFeature      = computeDerivativeGroupQuads :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> Bool
        , enableFeature     = \f ->
                                f { computeDerivativeGroupQuads = True } :: PhysicalDeviceComputeShaderDerivativesFeaturesNV
        , requireMinVersion = Nothing
        , requireExtensions = [NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
        }
    ]
  "ComputeDerivativeGroupLinearNV" ->
    [ RequireFeature
        { featureName       = "computeDerivativeGroupLinear"
        , checkFeature      = computeDerivativeGroupLinear :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> Bool
        , enableFeature     = \f ->
          f { computeDerivativeGroupLinear = True } :: PhysicalDeviceComputeShaderDerivativesFeaturesNV
        , requireMinVersion = Nothing
        , requireExtensions = [NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
        }
    ]
  "FragmentBarycentricNV" ->
    [ RequireFeature
        { featureName       = "fragmentShaderBarycentric"
        , checkFeature      = fragmentShaderBarycentric :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV -> Bool
        , enableFeature     = \f ->
                                f { fragmentShaderBarycentric = True } :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV
        , requireMinVersion = Nothing
        , requireExtensions = [NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME]
        }
    ]
  "ImageFootprintNV" ->
    [ RequireFeature { featureName       = "imageFootprint"
                     , checkFeature      = imageFootprint :: PhysicalDeviceShaderImageFootprintFeaturesNV -> Bool
                     , enableFeature = \f -> f { imageFootprint = True } :: PhysicalDeviceShaderImageFootprintFeaturesNV
                     , requireMinVersion = Nothing
                     , requireExtensions = [NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME]
                     }
    ]
  "ShadingRateNV" ->
    [ RequireFeature { featureName       = "shadingRateImage"
                     , checkFeature      = shadingRateImage :: PhysicalDeviceShadingRateImageFeaturesNV -> Bool
                     , enableFeature = \f -> f { shadingRateImage = True } :: PhysicalDeviceShadingRateImageFeaturesNV
                     , requireMinVersion = Nothing
                     , requireExtensions = [NV_SHADING_RATE_IMAGE_EXTENSION_NAME]
                     }
    ]
  "MeshShadingNV" -> [RequireExtension NV_MESH_SHADER_EXTENSION_NAME]
  "RayTracingProvisionalKHR" ->
    [ RequireFeature { featureName       = "rayTracing"
                     , checkFeature      = rayTracing :: PhysicalDeviceRayTracingFeaturesKHR -> Bool
                     , enableFeature     = \f -> f { rayTracing = True } :: PhysicalDeviceRayTracingFeaturesKHR
                     , requireMinVersion = Nothing
                     , requireExtensions = [KHR_RAY_TRACING_EXTENSION_NAME]
                     }
    ]
  "RayQueryProvisionalKHR" ->
    [ RequireFeature { featureName       = "rayQuery"
                     , checkFeature      = rayQuery :: PhysicalDeviceRayTracingFeaturesKHR -> Bool
                     , enableFeature     = \f -> f { rayQuery = True } :: PhysicalDeviceRayTracingFeaturesKHR
                     , requireMinVersion = Nothing
                     , requireExtensions = [KHR_RAY_TRACING_EXTENSION_NAME]
                     }
    ]
  "RayTraversalPrimitiveCullingProvisionalKHR" ->
    [ RequireFeature
        { featureName       = "rayTracingPrimitiveCulling"
        , checkFeature      = rayTracingPrimitiveCulling :: PhysicalDeviceRayTracingFeaturesKHR -> Bool
        , enableFeature     = \f -> f { rayTracingPrimitiveCulling = True } :: PhysicalDeviceRayTracingFeaturesKHR
        , requireMinVersion = Nothing
        , requireExtensions = [KHR_RAY_TRACING_EXTENSION_NAME]
        }
    ]
  "RayTracingNV" -> [RequireExtension NV_RAY_TRACING_EXTENSION_NAME]
  "TransformFeedback" ->
    [ RequireFeature
        { featureName       = "transformFeedback"
        , checkFeature      = transformFeedback :: PhysicalDeviceTransformFeedbackFeaturesEXT -> Bool
        , enableFeature     = \f -> f { transformFeedback = True } :: PhysicalDeviceTransformFeedbackFeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME]
        }
    ]
  "GeometryStreams" ->
    [ RequireFeature { featureName       = "geometryStreams"
                     , checkFeature      = geometryStreams :: PhysicalDeviceTransformFeedbackFeaturesEXT -> Bool
                     , enableFeature = \f -> f { geometryStreams = True } :: PhysicalDeviceTransformFeedbackFeaturesEXT
                     , requireMinVersion = Nothing
                     , requireExtensions = [EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME]
                     }
    ]
  "FragmentDensityEXT" ->
    [ RequireFeature
        { featureName       = "fragmentDensityMap"
        , checkFeature      = fragmentDensityMap :: PhysicalDeviceFragmentDensityMapFeaturesEXT -> Bool
        , enableFeature     = \f -> f { fragmentDensityMap = True } :: PhysicalDeviceFragmentDensityMapFeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME]
        }
    ]
  "PhysicalStorageBufferAddresses" ->
    [ RequireFeature { featureName       = "bufferDeviceAddress"
                     , checkFeature      = bufferDeviceAddress :: PhysicalDeviceVulkan12Features -> Bool
                     , enableFeature     = \f -> f { bufferDeviceAddress = True } :: PhysicalDeviceVulkan12Features
                     , requireMinVersion = Just $ MAKE_VERSION 1 2 0
                     , requireExtensions = [KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
                     }
    , RequireFeature
      { featureName       = "bufferDeviceAddress"
      , checkFeature      = bufferDeviceAddress :: PhysicalDeviceBufferDeviceAddressFeaturesEXT -> Bool
      , enableFeature     = \f -> f { bufferDeviceAddress = True } :: PhysicalDeviceBufferDeviceAddressFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
      }
    ]
  "CooperativeMatrixNV" ->
    [ RequireFeature { featureName       = "cooperativeMatrix"
                     , checkFeature      = cooperativeMatrix :: PhysicalDeviceCooperativeMatrixFeaturesNV -> Bool
                     , enableFeature = \f -> f { cooperativeMatrix = True } :: PhysicalDeviceCooperativeMatrixFeaturesNV
                     , requireMinVersion = Nothing
                     , requireExtensions = [NV_COOPERATIVE_MATRIX_EXTENSION_NAME]
                     }
    ]
  "IntegerFunctions2INTEL" ->
    [ RequireFeature
        { featureName       = "shaderIntegerFunctions2"
        , checkFeature      = shaderIntegerFunctions2 :: PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL -> Bool
        , enableFeature     = \f ->
                                f { shaderIntegerFunctions2 = True } :: PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
        , requireMinVersion = Nothing
        , requireExtensions = [INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME]
        }
    ]
  "ShaderSMBuiltinsNV" ->
    [ RequireFeature { featureName       = "shaderSMBuiltins"
                     , checkFeature      = shaderSMBuiltins :: PhysicalDeviceShaderSMBuiltinsFeaturesNV -> Bool
                     , enableFeature = \f -> f { shaderSMBuiltins = True } :: PhysicalDeviceShaderSMBuiltinsFeaturesNV
                     , requireMinVersion = Nothing
                     , requireExtensions = [NV_SHADER_SM_BUILTINS_EXTENSION_NAME]
                     }
    ]
  "FragmentShaderSampleInterlockEXT" ->
    [ RequireFeature
        { featureName       = "fragmentShaderSampleInterlock"
        , checkFeature      = fragmentShaderSampleInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
        , enableFeature     = \f ->
          f { fragmentShaderSampleInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
        }
    ]
  "FragmentShaderPixelInterlockEXT" ->
    [ RequireFeature
        { featureName       = "fragmentShaderPixelInterlock"
        , checkFeature      = fragmentShaderPixelInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
        , enableFeature     = \f ->
          f { fragmentShaderPixelInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
        }
    ]
  "FragmentShaderShadingRateInterlockEXT" ->
    [ RequireFeature
      { featureName       = "fragmentShaderShadingRateInterlock"
      , checkFeature = fragmentShaderShadingRateInterlock :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT -> Bool
      , enableFeature     = \f ->
        f { fragmentShaderShadingRateInterlock = True } :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT
      , requireMinVersion = Nothing
      , requireExtensions = [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
      }
    , RequireFeature { featureName       = "shadingRateImage"
                     , checkFeature      = shadingRateImage :: PhysicalDeviceShadingRateImageFeaturesNV -> Bool
                     , enableFeature = \f -> f { shadingRateImage = True } :: PhysicalDeviceShadingRateImageFeaturesNV
                     , requireMinVersion = Nothing
                     , requireExtensions = [NV_SHADING_RATE_IMAGE_EXTENSION_NAME]
                     }
    ]
  "DemoteToHelperInvocationEXT" ->
    [ RequireFeature
        { featureName       = "shaderDemoteToHelperInvocation"
        , checkFeature      = shaderDemoteToHelperInvocation :: PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
                              -> Bool
        , enableFeature     = \f ->
          f { shaderDemoteToHelperInvocation = True } :: PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
        , requireMinVersion = Nothing
        , requireExtensions = [EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME]
        }
    ]
  "FragmentShadingRateKHR" ->
    [ RequireFeature
      { featureName       = "pipelineFragmentShadingRate"
      , checkFeature      = pipelineFragmentShadingRate :: PhysicalDeviceFragmentShadingRateFeaturesKHR -> Bool
      , enableFeature = \f -> f { pipelineFragmentShadingRate = True } :: PhysicalDeviceFragmentShadingRateFeaturesKHR
      , requireMinVersion = Nothing
      , requireExtensions = [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "primitiveFragmentShadingRate"
      , checkFeature      = primitiveFragmentShadingRate :: PhysicalDeviceFragmentShadingRateFeaturesKHR -> Bool
      , enableFeature = \f -> f { primitiveFragmentShadingRate = True } :: PhysicalDeviceFragmentShadingRateFeaturesKHR
      , requireMinVersion = Nothing
      , requireExtensions = [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
      }
    , RequireFeature
      { featureName       = "attachmentFragmentShadingRate"
      , checkFeature      = attachmentFragmentShadingRate :: PhysicalDeviceFragmentShadingRateFeaturesKHR -> Bool
      , enableFeature = \f -> f { attachmentFragmentShadingRate = True } :: PhysicalDeviceFragmentShadingRateFeaturesKHR
      , requireMinVersion = Nothing
      , requireExtensions = [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
      }
    ]
  _ -> []

