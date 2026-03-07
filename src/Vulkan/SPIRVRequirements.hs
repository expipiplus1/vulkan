{-# language CPP #-}
{-# language OverloadedLists #-}
{-# language TupleSections #-}
-- No documentation found for Chapter "SPIRVRequirements"
module Vulkan.SPIRVRequirements  ( spirvExtensionRequirements
                                 , spirvCapabilityRequirements
                                 ) where

import Data.Bits (Bits)
import Data.Bits (zeroBits)
import Vulkan.Requirement (DeviceRequirement(..))
import Vulkan.Requirement (InstanceRequirement(..))
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures(..))
import Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Features(..))
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import Vulkan.Core12 (PhysicalDeviceVulkan11Properties(..))
import Vulkan.Core12 (PhysicalDeviceVulkan12Features(..))
import Vulkan.Version (pattern MAKE_API_VERSION)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_ARITHMETIC_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_BALLOT_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_BASIC_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_CLUSTERED_BIT))
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
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_AMD_shader_explicit_vertex_parameter" -> (,) [] []
  "SPV_AMD_gcn_shader" -> (,) [] []
  "SPV_AMD_gpu_shader_half_float" -> (,) [] []
  "SPV_AMD_gpu_shader_int16" -> (,) [] []
  "SPV_AMD_shader_ballot" -> (,) [] []
  "SPV_AMD_shader_fragment_mask" -> (,) [] []
  "SPV_AMD_shader_image_load_store_lod" -> (,) [] []
  "SPV_AMD_shader_trinary_minmax" -> (,) [] []
  "SPV_AMD_texture_gather_bias_lod" -> (,) [] []
  "SPV_AMD_shader_early_and_late_fragment_tests" -> (,) [] []
  "SPV_KHR_shader_draw_parameters" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_8bit_storage" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_16bit_storage" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_shader_clock" -> (,) [] []
  "SPV_KHR_float_controls" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_storage_buffer_storage_class" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_post_depth_coverage" -> (,) [] []
  "SPV_EXT_shader_stencil_export" -> (,) [] []
  "SPV_KHR_shader_ballot" -> (,) [] []
  "SPV_KHR_subgroup_vote" -> (,) [] []
  "SPV_NV_sample_mask_override_coverage" -> (,) [] []
  "SPV_NV_geometry_shader_passthrough" -> (,) [] []
  "SPV_NV_mesh_shader" -> (,) [] []
  "SPV_NV_viewport_array2" -> (,) [] []
  "SPV_NV_shader_subgroup_partitioned" -> (,) [] []
  "SPV_EXT_shader_subgroup_partitioned" -> (,) [] []
  "SPV_NV_shader_invocation_reorder" -> (,) [] []
  "SPV_EXT_shader_viewport_index_layer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_NVX_multiview_per_view_attributes" -> (,) [] []
  "SPV_EXT_descriptor_indexing" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_KHR_vulkan_memory_model" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_NV_compute_shader_derivatives" -> (,) [] []
  "SPV_NV_fragment_shader_barycentric" -> (,) [] []
  "SPV_NV_shader_image_footprint" -> (,) [] []
  "SPV_NV_shading_rate" -> (,) [] []
  "SPV_NV_ray_tracing" -> (,) [] []
  "SPV_KHR_ray_tracing" -> (,) [] []
  "SPV_KHR_ray_query" -> (,) [] []
  "SPV_KHR_ray_cull_mask" -> (,) [] []
  "SPV_GOOGLE_hlsl_functionality1" -> (,) [] []
  "SPV_GOOGLE_user_type" -> (,) [] []
  "SPV_GOOGLE_decorate_string" -> (,) [] []
  "SPV_EXT_fragment_invocation_density" -> (,) [] []
  "SPV_KHR_physical_storage_buffer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "SPV_EXT_physical_storage_buffer" -> (,) [] []
  "SPV_NV_cooperative_matrix" -> (,) [] []
  "SPV_NV_shader_sm_builtins" -> (,) [] []
  "SPV_EXT_fragment_shader_interlock" -> (,) [] []
  "SPV_EXT_demote_to_helper_invocation" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_KHR_fragment_shading_rate" -> (,) [] []
  "SPV_KHR_non_semantic_info" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_EXT_shader_image_int64" -> (,) [] []
  "SPV_KHR_terminate_invocation" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_KHR_multiview" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_KHR_workgroup_memory_explicit_layout" -> (,) [] []
  "SPV_EXT_shader_atomic_float_add" -> (,) [] []
  "SPV_KHR_fragment_shader_barycentric" -> (,) [] []
  "SPV_KHR_subgroup_uniform_control_flow" -> (,) [] []
  "SPV_EXT_shader_atomic_float_min_max" -> (,) [] []
  "SPV_EXT_shader_atomic_float16_add" -> (,) [] []
  "SPV_NV_shader_atomic_fp16_vector" -> (,) [] []
  "SPV_EXT_fragment_fully_covered" -> (,) [] []
  "SPV_KHR_integer_dot_product" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 3 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 3 0]
  "SPV_INTEL_shader_integer_functions2" -> (,) [] []
  "SPV_KHR_device_group" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "SPV_QCOM_image_processing" -> (,) [] []
  "SPV_QCOM_image_processing2" -> (,) [] []
  "SPV_QCOM_cooperative_matrix_conversion" -> (,) [] []
  "SPV_EXT_mesh_shader" -> (,) [] []
  "SPV_KHR_ray_tracing_position_fetch" -> (,) [] []
  "SPV_EXT_shader_tile_image" -> (,) [] []
  "SPV_EXT_opacity_micromap" -> (,) [] []
  "SPV_KHR_cooperative_matrix" -> (,) [] []
  "SPV_ARM_core_builtins" -> (,) [] []
  "SPV_HUAWEI_cluster_culling_shader" -> (,) [] []
  "SPV_HUAWEI_subpass_shading" -> (,) [] []
  "SPV_NV_ray_tracing_motion_blur" -> (,) [] []
  "SPV_KHR_maximal_reconvergence" -> (,) [] []
  "SPV_KHR_subgroup_rotate" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 4 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 4 0]
  "SPV_KHR_expect_assume" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 4 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 4 0]
  "SPV_KHR_float_controls2" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 4 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 4 0]
  "SPV_KHR_fma" -> (,) [] []
  "SPV_KHR_quad_control" -> (,) [] []
  "SPV_KHR_bfloat16" -> (,) [] []
  "SPV_NV_raw_access_chains" -> (,) [] []
  "SPV_KHR_compute_shader_derivatives" -> (,) [] []
  "SPV_EXT_replicated_composites" -> (,) [] []
  "SPV_KHR_relaxed_extended_instruction" -> (,) [] []
  "SPV_NV_cooperative_matrix2" -> (,) [] []
  "SPV_NV_tensor_addressing" -> (,) [] []
  "SPV_NV_linear_swept_spheres" -> (,) [] []
  "SPV_NV_cluster_acceleration_structure" -> (,) [] []
  "SPV_NV_cooperative_vector" -> (,) [] []
  "SPV_NV_push_constant_bank" -> (,) [] []
  "SPV_EXT_shader_invocation_reorder" -> (,) [] []
  "SPV_QCOM_tile_shading" -> (,) [] []
  "SPV_ARM_tensors" -> (,) [] []
  "SPV_EXT_float8" -> (,) [] []
  "SPV_ARM_graph" -> (,) [] []
  "SPV_KHR_untyped_pointers" -> (,) [] []
  "SPV_EXT_shader_64bit_indexing" -> (,) [] []
  "SPV_EXT_long_vector" -> (,) [] []
  "SPV_EXT_descriptor_heap" -> (,) [] []
  _ -> ([], [])

spirvCapabilityRequirements :: ByteString -> ([InstanceRequirement], [DeviceRequirement])
spirvCapabilityRequirements = \case
  "Matrix" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Shader" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "InputAttachment" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Sampled1D" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Image1D" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "SampledBuffer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "ImageBuffer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "ImageQuery" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "DerivativeControl" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "Geometry" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "geometryShader"
          , checkFeature = \PhysicalDeviceFeatures{geometryShader} -> geometryShader
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{geometryShader = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "Tessellation" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "tessellationShader"
          , checkFeature = \PhysicalDeviceFeatures{tessellationShader} -> tessellationShader
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{tessellationShader = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "Float64" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderFloat64"
          , checkFeature = \PhysicalDeviceFeatures{shaderFloat64} -> shaderFloat64
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderFloat64 = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "Int64" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderInt64"
          , checkFeature = \PhysicalDeviceFeatures{shaderInt64} -> shaderInt64
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderInt64 = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "Int64Atomics" -> (,) [] []
  "AtomicFloat16AddEXT" -> (,) [] []
  "AtomicFloat32AddEXT" -> (,) [] []
  "AtomicFloat64AddEXT" -> (,) [] []
  "AtomicFloat16MinMaxEXT" -> (,) [] []
  "AtomicFloat32MinMaxEXT" -> (,) [] []
  "AtomicFloat64MinMaxEXT" -> (,) [] []
  "AtomicFloat16VectorNV" -> (,) [] []
  "Int64ImageEXT" -> (,) [] []
  "Int16" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderInt16"
          , checkFeature = \PhysicalDeviceFeatures{shaderInt16} -> shaderInt16
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderInt16 = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "TessellationPointSize" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderTessellationAndGeometryPointSize"
          , checkFeature = \PhysicalDeviceFeatures{shaderTessellationAndGeometryPointSize} -> shaderTessellationAndGeometryPointSize
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderTessellationAndGeometryPointSize = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "GeometryPointSize" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderTessellationAndGeometryPointSize"
          , checkFeature = \PhysicalDeviceFeatures{shaderTessellationAndGeometryPointSize} -> shaderTessellationAndGeometryPointSize
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderTessellationAndGeometryPointSize = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "ImageGatherExtended" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderImageGatherExtended"
          , checkFeature = \PhysicalDeviceFeatures{shaderImageGatherExtended} -> shaderImageGatherExtended
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderImageGatherExtended = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageImageMultisample" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageImageMultisample"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageImageMultisample} -> shaderStorageImageMultisample
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageImageMultisample = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "UniformBufferArrayDynamicIndexing" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderUniformBufferArrayDynamicIndexing"
          , checkFeature = \PhysicalDeviceFeatures{shaderUniformBufferArrayDynamicIndexing} -> shaderUniformBufferArrayDynamicIndexing
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderUniformBufferArrayDynamicIndexing = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "SampledImageArrayDynamicIndexing" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderSampledImageArrayDynamicIndexing"
          , checkFeature = \PhysicalDeviceFeatures{shaderSampledImageArrayDynamicIndexing} -> shaderSampledImageArrayDynamicIndexing
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderSampledImageArrayDynamicIndexing = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageBufferArrayDynamicIndexing" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageBufferArrayDynamicIndexing"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageBufferArrayDynamicIndexing} -> shaderStorageBufferArrayDynamicIndexing
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageBufferArrayDynamicIndexing = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageImageArrayDynamicIndexing" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageImageArrayDynamicIndexing"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageImageArrayDynamicIndexing} -> shaderStorageImageArrayDynamicIndexing
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageImageArrayDynamicIndexing = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "ClipDistance" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderClipDistance"
          , checkFeature = \PhysicalDeviceFeatures{shaderClipDistance} -> shaderClipDistance
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderClipDistance = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "CullDistance" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderCullDistance"
          , checkFeature = \PhysicalDeviceFeatures{shaderCullDistance} -> shaderCullDistance
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderCullDistance = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "ImageCubeArray" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "imageCubeArray"
          , checkFeature = \PhysicalDeviceFeatures{imageCubeArray} -> imageCubeArray
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{imageCubeArray = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "SampleRateShading" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "sampleRateShading"
          , checkFeature = \PhysicalDeviceFeatures{sampleRateShading} -> sampleRateShading
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{sampleRateShading = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "SparseResidency" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderResourceResidency"
          , checkFeature = \PhysicalDeviceFeatures{shaderResourceResidency} -> shaderResourceResidency
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderResourceResidency = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "MinLod" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderResourceMinLod"
          , checkFeature = \PhysicalDeviceFeatures{shaderResourceMinLod} -> shaderResourceMinLod
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderResourceMinLod = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "SampledCubeArray" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "imageCubeArray"
          , checkFeature = \PhysicalDeviceFeatures{imageCubeArray} -> imageCubeArray
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{imageCubeArray = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "ImageMSArray" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageImageMultisample"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageImageMultisample} -> shaderStorageImageMultisample
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageImageMultisample = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageImageExtendedFormats" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 0 0]
  "InterpolationFunction" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "sampleRateShading"
          , checkFeature = \PhysicalDeviceFeatures{sampleRateShading} -> sampleRateShading
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{sampleRateShading = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageImageReadWithoutFormat" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageImageReadWithoutFormat"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageImageReadWithoutFormat} -> shaderStorageImageReadWithoutFormat
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageImageReadWithoutFormat = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "StorageImageWriteWithoutFormat" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "shaderStorageImageWriteWithoutFormat"
          , checkFeature = \PhysicalDeviceFeatures{shaderStorageImageWriteWithoutFormat} -> shaderStorageImageWriteWithoutFormat
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{shaderStorageImageWriteWithoutFormat = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "MultiViewport" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 0 0]
      [ RequireDeviceFeature
          { featureName = "multiViewport"
          , checkFeature = \PhysicalDeviceFeatures{multiViewport} -> multiViewport
          , enableFeature = \PhysicalDeviceFeatures{..} -> PhysicalDeviceFeatures{multiViewport = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 0 0
      ]
  "DrawParameters" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "shaderDrawParameters"
          , checkFeature = \PhysicalDeviceVulkan11Features{shaderDrawParameters} -> shaderDrawParameters
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{shaderDrawParameters = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "MultiView" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "multiview"
          , checkFeature = \PhysicalDeviceVulkan11Features{multiview} -> multiview
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{multiview = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "DeviceGroup" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 1 0]
  "VariablePointersStorageBuffer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "variablePointersStorageBuffer"
          , checkFeature = \PhysicalDeviceVulkan11Features{variablePointersStorageBuffer} -> variablePointersStorageBuffer
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{variablePointersStorageBuffer = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "VariablePointers" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "variablePointers"
          , checkFeature = \PhysicalDeviceVulkan11Features{variablePointers} -> variablePointers
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{variablePointers = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "ShaderClockKHR" -> (,) [] []
  "StencilExportEXT" -> (,) [] []
  "SubgroupBallotKHR" -> (,) [] []
  "SubgroupVoteKHR" -> (,) [] []
  "ImageReadWriteLodAMD" -> (,) [] []
  "ImageGatherBiasLodAMD" -> (,) [] []
  "FragmentMaskAMD" -> (,) [] []
  "SampleMaskOverrideCoverageNV" -> (,) [] []
  "GeometryShaderPassthroughNV" -> (,) [] []
  "ShaderViewportIndex" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "shaderOutputViewportIndex"
          , checkFeature = \PhysicalDeviceVulkan12Features{shaderOutputViewportIndex} -> shaderOutputViewportIndex
          , enableFeature = \PhysicalDeviceVulkan12Features{..} -> PhysicalDeviceVulkan12Features{shaderOutputViewportIndex = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "ShaderLayer" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "shaderOutputLayer"
          , checkFeature = \PhysicalDeviceVulkan12Features{shaderOutputLayer} -> shaderOutputLayer
          , enableFeature = \PhysicalDeviceVulkan12Features{..} -> PhysicalDeviceVulkan12Features{shaderOutputLayer = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "ShaderViewportIndexLayerEXT" -> (,) [] []
  "ShaderViewportMaskNV" -> (,) [] []
  "PerViewAttributesNV" -> (,) [] []
  "StorageBuffer16BitAccess" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "storageBuffer16BitAccess"
          , checkFeature = \PhysicalDeviceVulkan11Features{storageBuffer16BitAccess} -> storageBuffer16BitAccess
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{storageBuffer16BitAccess = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "UniformAndStorageBuffer16BitAccess" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "uniformAndStorageBuffer16BitAccess"
          , checkFeature = \PhysicalDeviceVulkan11Features{uniformAndStorageBuffer16BitAccess} -> uniformAndStorageBuffer16BitAccess
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{uniformAndStorageBuffer16BitAccess = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "StoragePushConstant16" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "storagePushConstant16"
          , checkFeature = \PhysicalDeviceVulkan11Features{storagePushConstant16} -> storagePushConstant16
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{storagePushConstant16 = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "StorageInputOutput16" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [ RequireDeviceFeature
          { featureName = "storageInputOutput16"
          , checkFeature = \PhysicalDeviceVulkan11Features{storageInputOutput16} -> storageInputOutput16
          , enableFeature = \PhysicalDeviceVulkan11Features{..} -> PhysicalDeviceVulkan11Features{storageInputOutput16 = True, ..}
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 2 0
      ]
  "GroupNonUniform" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_BASIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformVote" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_VOTE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformArithmetic" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_ARITHMETIC_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformBallot" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_BALLOT_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformShuffle" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_SHUFFLE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformShuffleRelative" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformClustered" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_CLUSTERED_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformQuad" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 1 0]
      [ RequireDeviceProperty
          { propertyName = "VkPhysicalDeviceVulkan11Properties"
          , checkProperty = \p -> SUBGROUP_FEATURE_QUAD_BIT .&&. subgroupSupportedOperations (p :: PhysicalDeviceVulkan11Properties)
          }
      , RequireDeviceVersion $ MAKE_API_VERSION 1 1 0
      ]
  "GroupNonUniformPartitionedEXT" -> (,) [] []
  "SampleMaskPostDepthCoverage" -> (,) [] []
  "ShaderNonUniform" ->
    (,)
      [RequireInstanceVersion $ MAKE_API_VERSION 1 2 0]
      [RequireDeviceVersion $ MAKE_API_VERSION 1 2 0]
  "RuntimeDescriptorArray" -> (,) [] []
  "InputAttachmentArrayDynamicIndexing" -> (,) [] []
  "UniformTexelBufferArrayDynamicIndexing" -> (,) [] []
  "StorageTexelBufferArrayDynamicIndexing" -> (,) [] []
  "UniformBufferArrayNonUniformIndexing" -> (,) [] []
  "SampledImageArrayNonUniformIndexing" -> (,) [] []
  "StorageBufferArrayNonUniformIndexing" -> (,) [] []
  "StorageImageArrayNonUniformIndexing" -> (,) [] []
  "InputAttachmentArrayNonUniformIndexing" -> (,) [] []
  "UniformTexelBufferArrayNonUniformIndexing" -> (,) [] []
  "StorageTexelBufferArrayNonUniformIndexing" -> (,) [] []
  "FragmentFullyCoveredEXT" -> (,) [] []
  "Float16" -> (,) [] []
  "Int8" -> (,) [] []
  "StorageBuffer8BitAccess" -> (,) [] []
  "UniformAndStorageBuffer8BitAccess" -> (,) [] []
  "StoragePushConstant8" -> (,) [] []
  "VulkanMemoryModel" -> (,) [] []
  "VulkanMemoryModelDeviceScope" -> (,) [] []
  "DenormPreserve" -> (,) [] []
  "DenormFlushToZero" -> (,) [] []
  "SignedZeroInfNanPreserve" -> (,) [] []
  "RoundingModeRTE" -> (,) [] []
  "RoundingModeRTZ" -> (,) [] []
  "ComputeDerivativeGroupQuadsKHR" -> (,) [] []
  "ComputeDerivativeGroupLinearKHR" -> (,) [] []
  "ImageFootprintNV" -> (,) [] []
  "MeshShadingNV" -> (,) [] []
  "RayTracingKHR" -> (,) [] []
  "RayQueryKHR" -> (,) [] []
  "RayTraversalPrimitiveCullingKHR" -> (,) [] []
  "RayCullMaskKHR" -> (,) [] []
  "RayTracingNV" -> (,) [] []
  "RayTracingMotionBlurNV" -> (,) [] []
  "TransformFeedback" -> (,) [] []
  "GeometryStreams" -> (,) [] []
  "FragmentDensityEXT" -> (,) [] []
  "PhysicalStorageBufferAddresses" -> (,) [] []
  "CooperativeMatrixNV" -> (,) [] []
  "IntegerFunctions2INTEL" -> (,) [] []
  "ShaderSMBuiltinsNV" -> (,) [] []
  "FragmentShaderSampleInterlockEXT" -> (,) [] []
  "FragmentShaderPixelInterlockEXT" -> (,) [] []
  "FragmentShaderShadingRateInterlockEXT" -> (,) [] []
  "DemoteToHelperInvocation" -> (,) [] []
  "FragmentShadingRateKHR" -> (,) [] []
  "WorkgroupMemoryExplicitLayoutKHR" -> (,) [] []
  "WorkgroupMemoryExplicitLayout8BitAccessKHR" -> (,) [] []
  "WorkgroupMemoryExplicitLayout16BitAccessKHR" -> (,) [] []
  "DotProductInputAll" -> (,) [] []
  "DotProductInput4x8Bit" -> (,) [] []
  "DotProductInput4x8BitPacked" -> (,) [] []
  "DotProduct" -> (,) [] []
  "FragmentBarycentricKHR" -> (,) [] []
  "TextureSampleWeightedQCOM" -> (,) [] []
  "TextureBoxFilterQCOM" -> (,) [] []
  "TextureBlockMatchQCOM" -> (,) [] []
  "TextureBlockMatch2QCOM" -> (,) [] []
  "MeshShadingEXT" -> (,) [] []
  "RayTracingOpacityMicromapEXT" -> (,) [] []
  "CoreBuiltinsARM" -> (,) [] []
  "ShaderInvocationReorderNV" -> (,) [] []
  "ClusterCullingShadingHUAWEI" -> (,) [] []
  "RayTracingPositionFetchKHR" -> (,) [] []
  "RayQueryPositionFetchKHR" -> (,) [] []
  "TileImageColorReadAccessEXT" -> (,) [] []
  "TileImageDepthReadAccessEXT" -> (,) [] []
  "TileImageStencilReadAccessEXT" -> (,) [] []
  "CooperativeMatrixKHR" -> (,) [] []
  "CooperativeMatrixConversionQCOM" -> (,) [] []
  "ShaderEnqueueAMDX" -> (,) [] []
  "GroupNonUniformRotateKHR" -> (,) [] []
  "ExpectAssumeKHR" -> (,) [] []
  "FloatControls2" -> (,) [] []
  "QuadControlKHR" -> (,) [] []
  "BFloat16TypeKHR" -> (,) [] []
  "BFloat16DotProductKHR" -> (,) [] []
  "BFloat16CooperativeMatrixKHR" -> (,) [] []
  "RawAccessChainsNV" -> (,) [] []
  "ReplicatedCompositesEXT" -> (,) [] []
  "TensorAddressingNV" -> (,) [] []
  "CooperativeMatrixReductionsNV" -> (,) [] []
  "CooperativeMatrixConversionsNV" -> (,) [] []
  "CooperativeMatrixPerElementOperationsNV" -> (,) [] []
  "CooperativeMatrixTensorAddressingNV" -> (,) [] []
  "CooperativeMatrixBlockLoadsNV" -> (,) [] []
  "RayTracingSpheresGeometryNV" -> (,) [] []
  "RayTracingLinearSweptSpheresGeometryNV" -> (,) [] []
  "RayTracingClusterAccelerationStructureNV" -> (,) [] []
  "CooperativeVectorNV" -> (,) [] []
  "CooperativeVectorTrainingNV" -> (,) [] []
  "PushConstantBanksNV" -> (,) [] []
  "ShaderInvocationReorderEXT" -> (,) [] []
  "TileShadingQCOM" -> (,) [] []
  "TensorsARM" -> (,) [] []
  "StorageTensorArrayDynamicIndexingARM" -> (,) [] []
  "StorageTensorArrayNonUniformIndexingARM" -> (,) [] []
  "Float8EXT" -> (,) [] []
  "Float8CooperativeMatrixEXT" -> (,) [] []
  "GraphARM" -> (,) [] []
  "UntypedPointersKHR" -> (,) [] []
  "FMAKHR" -> (,) [] []
  "Shader64BitIndexingEXT" -> (,) [] []
  "LongVectorEXT" -> (,) [] []
  "DescriptorHeapEXT" -> (,) [] []
  _ -> ([], [])
