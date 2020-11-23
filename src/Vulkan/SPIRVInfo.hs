{-# language CPP #-}
{-# language OverloadedLists #-}
module Vulkan.SPIRVInfo  () where

import Data.Version (makeVersion)
import Data.Version (Version)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
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
data SPIRVRequirement
  = Version Version
  | Extension ByteString
  | Feature ByteString ByteString (Vector ByteString)
  -- ^ Struct, feature, requires
  | Property ByteString ByteString ByteString (Vector ByteString)
  -- ^ Property, member, value, requires
  deriving(Show)

spirvExtensionRequirements :: ByteString -> [SPIRVRequirement]
spirvExtensionRequirements = \case
  "SPV_KHR_variable_pointers" -> [Version $ makeVersion [1, 1], Extension KHR_VARIABLE_POINTERS_EXTENSION_NAME]
  "SPV_AMD_shader_explicit_vertex_parameter" -> [Extension AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME]
  "SPV_AMD_gcn_shader"                       -> [Extension AMD_GCN_SHADER_EXTENSION_NAME]
  "SPV_AMD_gpu_shader_half_float"            -> [Extension AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME]
  "SPV_AMD_gpu_shader_int16"                 -> [Extension AMD_GPU_SHADER_INT16_EXTENSION_NAME]
  "SPV_AMD_shader_ballot"                    -> [Extension AMD_SHADER_BALLOT_EXTENSION_NAME]
  "SPV_AMD_shader_fragment_mask"             -> [Extension AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME]
  "SPV_AMD_shader_image_load_store_lod"      -> [Extension AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME]
  "SPV_AMD_shader_trinary_minmax"            -> [Extension AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME]
  "SPV_AMD_texture_gather_bias_lod"          -> [Extension AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME]
  "SPV_KHR_shader_draw_parameters" ->
    [Version $ makeVersion [1, 1], Extension KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME]
  "SPV_KHR_8bit_storage"   -> [Version $ makeVersion [1, 2], Extension KHR_8BIT_STORAGE_EXTENSION_NAME]
  "SPV_KHR_16bit_storage"  -> [Version $ makeVersion [1, 1], Extension KHR_16BIT_STORAGE_EXTENSION_NAME]
  "SPV_KHR_shader_clock"   -> [Extension KHR_SHADER_CLOCK_EXTENSION_NAME]
  "SPV_KHR_float_controls" -> [Version $ makeVersion [1, 2], Extension KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
  "SPV_KHR_storage_buffer_storage_class" ->
    [Version $ makeVersion [1, 1], Extension KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME]
  "SPV_KHR_post_depth_coverage"          -> [Extension EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME]
  "SPV_EXT_shader_stencil_export"        -> [Extension EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME]
  "SPV_KHR_shader_ballot"                -> [Extension EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME]
  "SPV_KHR_subgroup_vote"                -> [Extension EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME]
  "SPV_NV_sample_mask_override_coverage" -> [Extension NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME]
  "SPV_NV_geometry_shader_passthrough"   -> [Extension NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME]
  "SPV_NV_mesh_shader"                   -> [Extension NV_MESH_SHADER_EXTENSION_NAME]
  "SPV_NV_viewport_array2"               -> [Extension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "SPV_NV_shader_subgroup_partitioned"   -> [Extension NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME]
  "SPV_EXT_shader_viewport_index_layer" ->
    [Version $ makeVersion [1, 2], Extension EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME]
  "SPV_NVX_multiview_per_view_attributes" -> [Extension NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME]
  "SPV_EXT_descriptor_indexing" -> [Version $ makeVersion [1, 2], Extension EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
  "SPV_KHR_vulkan_memory_model" -> [Version $ makeVersion [1, 2], Extension KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
  "SPV_NV_compute_shader_derivatives"     -> [Extension NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
  "SPV_NV_fragment_shader_barycentric"    -> [Extension NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME]
  "SPV_NV_shader_image_footprint"         -> [Extension NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME]
  "SPV_NV_shading_rate"                   -> [Extension NV_SHADING_RATE_IMAGE_EXTENSION_NAME]
  "SPV_NV_ray_tracing"                    -> [Extension NV_RAY_TRACING_EXTENSION_NAME]
  "SPV_KHR_ray_tracing"                   -> [Extension KHR_RAY_TRACING_EXTENSION_NAME]
  "SPV_KHR_ray_query"                     -> [Extension KHR_RAY_TRACING_EXTENSION_NAME]
  "SPV_GOOGLE_hlsl_functionality1"        -> [Extension GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME]
  "SPV_GOOGLE_user_type"                  -> [Extension GOOGLE_USER_TYPE_EXTENSION_NAME]
  "SPV_GOOGLE_decorate_string"            -> [Extension GOOGLE_DECORATE_STRING_EXTENSION_NAME]
  "SPV_EXT_fragment_invocation_density"   -> [Extension EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME]
  "SPV_KHR_physical_storage_buffer" ->
    [Version $ makeVersion [1, 2], Extension KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
  "SPV_EXT_physical_storage_buffer" -> [Extension EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
  "SPV_NV_cooperative_matrix"     -> [Extension NV_COOPERATIVE_MATRIX_EXTENSION_NAME]
  "SPV_NV_shader_sm_builtins"     -> [Extension NV_SHADER_SM_BUILTINS_EXTENSION_NAME]
  "SPV_EXT_fragment_shader_interlock" -> [Extension EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
  "SPV_EXT_demote_to_helper_invocation" -> [Extension EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME]
  "SPV_KHR_fragment_shading_rate" -> [Extension KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
  "SPV_KHR_non_semantic_info"     -> [Extension KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME]
  "SPV_EXT_shader_image_int64"    -> [Extension EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME]
  "SPV_KHR_terminate_invocation"  -> [Extension KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME]
  _                               -> []

spirvCapabilityRequirements :: ByteString -> [SPIRVRequirement]
spirvCapabilityRequirements = \case
  "Matrix"            -> [Version $ makeVersion [1, 0]]
  "Shader"            -> [Version $ makeVersion [1, 0]]
  "InputAttachment"   -> [Version $ makeVersion [1, 0]]
  "Sampled1D"         -> [Version $ makeVersion [1, 0]]
  "Image1D"           -> [Version $ makeVersion [1, 0]]
  "SampledBuffer"     -> [Version $ makeVersion [1, 0]]
  "ImageBuffer"       -> [Version $ makeVersion [1, 0]]
  "ImageQuery"        -> [Version $ makeVersion [1, 0]]
  "DerivativeControl" -> [Version $ makeVersion [1, 0]]
  "Geometry"          -> [Feature "VkPhysicalDeviceFeatures" "geometryShader" []]
  "Tessellation"      -> [Feature "VkPhysicalDeviceFeatures" "tessellationShader" []]
  "Float64"           -> [Feature "VkPhysicalDeviceFeatures" "shaderFloat64" []]
  "Int64"             -> [Feature "VkPhysicalDeviceFeatures" "shaderInt64" []]
  "Int64Atomics" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderBufferInt64Atomics"
              ["VK_VERSION_1_2", KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceVulkan12Features"
              "shaderSharedInt64Atomics"
              ["VK_VERSION_1_2", KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME]
    ]
  "AtomicFloat32AddEXT" ->
    [ Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "shaderBufferFloat32AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "shaderSharedFloat32AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "shaderImageFloat32AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "sparseImageFloat32AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    ]
  "AtomicFloat64AddEXT" ->
    [ Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "shaderBufferFloat64AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
              "shaderSharedFloat64AtomicAdd"
              [EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME]
    ]
  "Int64ImageEXT" ->
    [ Feature "VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT"
              "shaderImageInt64Atomics"
              [EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME]
    ]
  "Int16"                   -> [Feature "VkPhysicalDeviceFeatures" "shaderInt16" []]
  "TessellationPointSize"   -> [Feature "VkPhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize" []]
  "GeometryPointSize"       -> [Feature "VkPhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize" []]
  "ImageGatherExtended"     -> [Feature "VkPhysicalDeviceFeatures" "shaderImageGatherExtended" []]
  "StorageImageMultisample" -> [Feature "VkPhysicalDeviceFeatures" "shaderStorageImageMultisample" []]
  "UniformBufferArrayDynamicIndexing" ->
    [Feature "VkPhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing" []]
  "SampledImageArrayDynamicIndexing" ->
    [Feature "VkPhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing" []]
  "StorageBufferArrayDynamicIndexing" ->
    [Feature "VkPhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing" []]
  "StorageImageArrayDynamicIndexing" ->
    [Feature "VkPhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing" []]
  "ClipDistance"                   -> [Feature "VkPhysicalDeviceFeatures" "shaderClipDistance" []]
  "CullDistance"                   -> [Feature "VkPhysicalDeviceFeatures" "shaderCullDistance" []]
  "ImageCubeArray"                 -> [Feature "VkPhysicalDeviceFeatures" "imageCubeArray" []]
  "SampleRateShading"              -> [Feature "VkPhysicalDeviceFeatures" "sampleRateShading" []]
  "SparseResidency"                -> [Feature "VkPhysicalDeviceFeatures" "shaderResourceResidency" []]
  "MinLod"                         -> [Feature "VkPhysicalDeviceFeatures" "shaderResourceMinLod" []]
  "SampledCubeArray"               -> [Feature "VkPhysicalDeviceFeatures" "imageCubeArray" []]
  "ImageMSArray"                   -> [Feature "VkPhysicalDeviceFeatures" "shaderStorageImageMultisample" []]
  "StorageImageExtendedFormats"    -> [Version $ makeVersion [1, 0]]
  "InterpolationFunction"          -> [Feature "VkPhysicalDeviceFeatures" "sampleRateShading" []]
  "StorageImageReadWithoutFormat"  -> [Feature "VkPhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat" []]
  "StorageImageWriteWithoutFormat" -> [Feature "VkPhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat" []]
  "MultiViewport"                  -> [Feature "VkPhysicalDeviceFeatures" "multiViewport" []]
  "DrawParameters" ->
    [ Feature "VkPhysicalDeviceVulkan11Features" "shaderDrawParameters" ["VK_VERSION_1_1"]
    , Extension KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
    ]
  "MultiView" ->
    [Feature "VkPhysicalDeviceVulkan11Features" "multiview" ["VK_VERSION_1_1", KHR_MULTIVIEW_EXTENSION_NAME]]
  "DeviceGroup" -> [Version $ makeVersion [1, 1], Extension KHR_DEVICE_GROUP_EXTENSION_NAME]
  "VariablePointersStorageBuffer" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "variablePointersStorageBuffer"
              ["VK_VERSION_1_1", KHR_VARIABLE_POINTERS_EXTENSION_NAME]
    ]
  "VariablePointers" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "variablePointers"
              ["VK_VERSION_1_1", KHR_VARIABLE_POINTERS_EXTENSION_NAME]
    ]
  "ShaderClockKHR"               -> [Extension KHR_SHADER_CLOCK_EXTENSION_NAME]
  "StencilExportEXT"             -> [Extension EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME]
  "SubgroupBallotKHR"            -> [Extension EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME]
  "SubgroupVoteKHR"              -> [Extension EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME]
  "ImageReadWriteLodAMD"         -> [Extension AMD_SHADER_IMAGE_LOAD_STORE_LOD_EXTENSION_NAME]
  "ImageGatherBiasLodAMD"        -> [Extension AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME]
  "FragmentMaskAMD"              -> [Extension AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME]
  "SampleMaskOverrideCoverageNV" -> [Extension NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME]
  "GeometryShaderPassthroughNV"  -> [Extension NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME]
  "ShaderViewportIndex" -> [Feature "VkPhysicalDeviceVulkan12Features" "shaderOutputViewportIndex" ["VK_VERSION_1_2"]]
  "ShaderLayer"                  -> [Feature "VkPhysicalDeviceVulkan12Features" "shaderOutputLayer" ["VK_VERSION_1_2"]]
  "ShaderViewportIndexLayerEXT"  -> [Extension EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME]
  "ShaderViewportIndexLayerNV"   -> [Extension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "ShaderViewportMaskNV"         -> [Extension NV_VIEWPORT_ARRAY2_EXTENSION_NAME]
  "PerViewAttributesNV"          -> [Extension NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME]
  "StorageBuffer16BitAccess" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "storageBuffer16BitAccess"
              ["VK_VERSION_1_1", KHR_16BIT_STORAGE_EXTENSION_NAME]
    ]
  "UniformAndStorageBuffer16BitAccess" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "uniformAndStorageBuffer16BitAccess"
              ["VK_VERSION_1_1", KHR_16BIT_STORAGE_EXTENSION_NAME]
    ]
  "StoragePushConstant16" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "storagePushConstant16"
              ["VK_VERSION_1_1", KHR_16BIT_STORAGE_EXTENSION_NAME]
    ]
  "StorageInputOutput16" ->
    [ Feature "VkPhysicalDeviceVulkan11Features"
              "storageInputOutput16"
              ["VK_VERSION_1_1", KHR_16BIT_STORAGE_EXTENSION_NAME]
    ]
  "GroupNonUniform" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_BASIC_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformVote" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_VOTE_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformArithmetic" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_ARITHMETIC_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformBallot" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_BALLOT_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformShuffle" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_SHUFFLE_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformShuffleRelative" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformClustered" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_CLUSTERED_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformQuad" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_QUAD_BIT"
               ["VK_VERSION_1_1"]
    ]
  "GroupNonUniformPartitionedNV" ->
    [ Property "VkPhysicalDeviceVulkan11Properties"
               "subgroupSupportedOperations"
               "VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
               [NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME]
    ]
  "SampleMaskPostDepthCoverage" -> [Extension EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME]
  "ShaderNonUniform"            -> [Version $ makeVersion [1, 2], Extension EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
  "RuntimeDescriptorArray" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "runtimeDescriptorArray"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "InputAttachmentArrayDynamicIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderInputAttachmentArrayDynamicIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "UniformTexelBufferArrayDynamicIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderUniformTexelBufferArrayDynamicIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "StorageTexelBufferArrayDynamicIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderStorageTexelBufferArrayDynamicIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "UniformBufferArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderUniformBufferArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "SampledImageArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderSampledImageArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "StorageBufferArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderStorageBufferArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "StorageImageArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderStorageImageArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "InputAttachmentArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderInputAttachmentArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "UniformTexelBufferArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderUniformTexelBufferArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "StorageTexelBufferArrayNonUniformIndexing" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderStorageTexelBufferArrayNonUniformIndexing"
              ["VK_VERSION_1_2", EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME]
    ]
  "Float16" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "shaderFloat16"
              ["VK_VERSION_1_2", KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME]
    , Extension AMD_GPU_SHADER_HALF_FLOAT_EXTENSION_NAME
    ]
  "Int8" ->
    [Feature "VkPhysicalDeviceVulkan12Features" "shaderInt8" ["VK_VERSION_1_2", KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME]]
  "StorageBuffer8BitAccess" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "storageBuffer8BitAccess"
              ["VK_VERSION_1_2", KHR_8BIT_STORAGE_EXTENSION_NAME]
    ]
  "UniformAndStorageBuffer8BitAccess" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "uniformAndStorageBuffer8BitAccess"
              ["VK_VERSION_1_2", KHR_8BIT_STORAGE_EXTENSION_NAME]
    ]
  "StoragePushConstant8" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "storagePushConstant8"
              ["VK_VERSION_1_2", KHR_8BIT_STORAGE_EXTENSION_NAME]
    ]
  "VulkanMemoryModel" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "vulkanMemoryModel"
              ["VK_VERSION_1_2", KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
    ]
  "VulkanMemoryModelDeviceScope" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "vulkanMemoryModelDeviceScope"
              ["VK_VERSION_1_2", KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME]
    ]
  "DenormPreserve" ->
    [ Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormPreserveFloat16"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormPreserveFloat32"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormPreserveFloat64"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    ]
  "DenormFlushToZero" ->
    [ Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormFlushToZeroFloat16"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormFlushToZeroFloat32"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderDenormFlushToZeroFloat64"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    ]
  "SignedZeroInfNanPreserve" ->
    [ Property "VkPhysicalDeviceVulkan12Properties"
               "shaderSignedZeroInfNanPreserveFloat16"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderSignedZeroInfNanPreserveFloat32"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderSignedZeroInfNanPreserveFloat64"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    ]
  "RoundingModeRTE" ->
    [ Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTEFloat16"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTEFloat32"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTEFloat64"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    ]
  "RoundingModeRTZ" ->
    [ Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTZFloat16"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTZFloat32"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    , Property "VkPhysicalDeviceVulkan12Properties"
               "shaderRoundingModeRTZFloat64"
               "VK_TRUE"
               ["VK_VERSION_1_2", KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME]
    ]
  "ComputeDerivativeGroupQuadsNV" ->
    [ Feature "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV"
              "computeDerivativeGroupQuads"
              [NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
    ]
  "ComputeDerivativeGroupLinearNV" ->
    [ Feature "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV"
              "computeDerivativeGroupLinear"
              [NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME]
    ]
  "FragmentBarycentricNV" ->
    [ Feature "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV"
              "fragmentShaderBarycentric"
              [NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME]
    ]
  "ImageFootprintNV" ->
    [ Feature "VkPhysicalDeviceShaderImageFootprintFeaturesNV"
              "imageFootprint"
              [NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME]
    ]
  "ShadingRateNV" ->
    [Feature "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage" [NV_SHADING_RATE_IMAGE_EXTENSION_NAME]]
  "MeshShadingNV" -> [Extension NV_MESH_SHADER_EXTENSION_NAME]
  "RayTracingProvisionalKHR" ->
    [Feature "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracing" [KHR_RAY_TRACING_EXTENSION_NAME]]
  "RayQueryProvisionalKHR" ->
    [Feature "VkPhysicalDeviceRayTracingFeaturesKHR" "rayQuery" [KHR_RAY_TRACING_EXTENSION_NAME]]
  "RayTraversalPrimitiveCullingProvisionalKHR" ->
    [Feature "VkPhysicalDeviceRayTracingFeaturesKHR" "rayTracingPrimitiveCulling" [KHR_RAY_TRACING_EXTENSION_NAME]]
  "RayTracingNV" -> [Extension NV_RAY_TRACING_EXTENSION_NAME]
  "TransformFeedback" ->
    [Feature "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "transformFeedback" [EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME]]
  "GeometryStreams" ->
    [Feature "VkPhysicalDeviceTransformFeedbackFeaturesEXT" "geometryStreams" [EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME]]
  "FragmentDensityEXT" ->
    [ Feature "VkPhysicalDeviceFragmentDensityMapFeaturesEXT"
              "fragmentDensityMap"
              [EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME]
    ]
  "PhysicalStorageBufferAddresses" ->
    [ Feature "VkPhysicalDeviceVulkan12Features"
              "bufferDeviceAddress"
              ["VK_VERSION_1_2", KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceBufferDeviceAddressFeaturesEXT"
              "bufferDeviceAddress"
              [EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME]
    ]
  "CooperativeMatrixNV" ->
    [Feature "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix" [NV_COOPERATIVE_MATRIX_EXTENSION_NAME]]
  "IntegerFunctions2INTEL" ->
    [ Feature "VkPhysicalDeviceShaderIntegerFunctions2FeaturesINTEL"
              "shaderIntegerFunctions2"
              [INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME]
    ]
  "ShaderSMBuiltinsNV" ->
    [Feature "VkPhysicalDeviceShaderSMBuiltinsFeaturesNV" "shaderSMBuiltins" [NV_SHADER_SM_BUILTINS_EXTENSION_NAME]]
  "FragmentShaderSampleInterlockEXT" ->
    [ Feature "VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT"
              "fragmentShaderSampleInterlock"
              [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
    ]
  "FragmentShaderPixelInterlockEXT" ->
    [ Feature "VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT"
              "fragmentShaderPixelInterlock"
              [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
    ]
  "FragmentShaderShadingRateInterlockEXT" ->
    [ Feature "VkPhysicalDeviceFragmentShaderInterlockFeaturesEXT"
              "fragmentShaderShadingRateInterlock"
              [EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceShadingRateImageFeaturesNV" "shadingRateImage" [NV_SHADING_RATE_IMAGE_EXTENSION_NAME]
    ]
  "DemoteToHelperInvocationEXT" ->
    [ Feature "VkPhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT"
              "shaderDemoteToHelperInvocation"
              [EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME]
    ]
  "FragmentShadingRateKHR" ->
    [ Feature "VkPhysicalDeviceFragmentShadingRateFeaturesKHR"
              "pipelineFragmentShadingRate"
              [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceFragmentShadingRateFeaturesKHR"
              "primitiveFragmentShadingRate"
              [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
    , Feature "VkPhysicalDeviceFragmentShadingRateFeaturesKHR"
              "attachmentFragmentShadingRate"
              [KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME]
    ]
  _ -> []

