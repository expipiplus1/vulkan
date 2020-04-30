{-# language CPP #-}
module Graphics.Vulkan.Core10.BaseType  ( boolToBool32
                                        , bool32ToBool
                                        , Bool32( FALSE
                                                , TRUE
                                                , ..
                                                )
                                        , SampleMask
                                        , Flags
                                        , DeviceSize
                                        , DeviceAddress
                                        ) where

import Data.Bool (bool)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
boolToBool32 :: Bool -> Bool32
boolToBool32 = bool FALSE TRUE

bool32ToBool :: Bool32 -> Bool
bool32ToBool = \case
  FALSE -> False
  TRUE  -> True


-- | VkBool32 - Vulkan boolean type
--
-- = Description
--
-- 'TRUE' represents a boolean __True__ (integer 1) value, and 'FALSE' a
-- boolean __False__ (integer 0) value.
--
-- All values returned from a Vulkan implementation in a 'Bool32' will be
-- either 'TRUE' or 'FALSE'.
--
-- Applications /must/ not pass any other values than 'TRUE' or 'FALSE'
-- into a Vulkan implementation where a 'Bool32' is expected.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureBuildGeometryInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateGeometryTypeInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryInstancesDataKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.DescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr.DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.PerformanceOverrideInfoINTEL',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.PerformanceValueDataINTEL',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
-- 'Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
-- 'Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.PhysicalDeviceRayTracingFeaturesKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
-- 'Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
-- 'Graphics.Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceSparseProperties',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Features',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Properties',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInternalRepresentationKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableStatisticValueKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
-- 'Graphics.Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD',
-- 'Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.TextureLODGatherFormatPropertiesAMD',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD',
-- 'Graphics.Vulkan.Core10.Fence.waitForFences'
newtype Bool32 = Bool32 Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern FALSE = Bool32 0
-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern TRUE = Bool32 1
{-# complete FALSE,
             TRUE :: Bool32 #-}

instance Show Bool32 where
  showsPrec p = \case
    FALSE -> showString "FALSE"
    TRUE -> showString "TRUE"
    Bool32 x -> showParen (p >= 11) (showString "Bool32 " . showsPrec 11 x)

instance Read Bool32 where
  readPrec = parens (choose [("FALSE", pure FALSE)
                            , ("TRUE", pure TRUE)]
                     +++
                     prec 10 (do
                       expectP (Ident "Bool32")
                       v <- step readPrec
                       pure (Bool32 v)))


-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
type SampleMask = Word32


-- | VkFlags - Vulkan bitmasks
--
-- = Description
--
-- Bitmasks are passed to many commands and structures to compactly
-- represent options, but 'Flags' is not used directly in the API. Instead,
-- a @Vk*Flags@ type which is an alias of 'Flags', and whose name matches
-- the corresponding @Vk*FlagBits@ that are valid for that type, is used.
--
-- Any @Vk*Flags@ member or parameter used in the API as an input /must/ be
-- a valid combination of bit flags. A valid combination is either zero or
-- the bitwise OR of valid bit flags. A bit flag is valid if:
--
-- -   The bit flag is defined as part of the @Vk*FlagBits@ type, where the
--     bits type is obtained by taking the flag type and replacing the
--     trailing 'Flags' with @FlagBits@. For example, a flag value of type
--     'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags'
--     /must/ contain only bit flags defined by
--     'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'.
--
-- -   The flag is allowed in the context in which it is being used. For
--     example, in some cases, certain bit flags or combinations of bit
--     flags are mutually exclusive.
--
-- Any @Vk*Flags@ member or parameter returned from a query command or
-- otherwise output from Vulkan to the application /may/ contain bit flags
-- undefined in its corresponding @Vk*FlagBits@ type. An application
-- /cannot/ rely on the state of these unspecified bits.
--
-- Only the low-order 31 bits (bit positions zero through 30) are available
-- for use as flag bits.
--
-- Note
--
-- This restriction is due to poorly defined behavior by C compilers given
-- a C enumerant value of @0x80000000@. In some cases adding this enumerant
-- value may increase the size of the underlying @Vk*FlagBits@ type,
-- breaking the ABI.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags'
type Flags = Word32


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryAabbsDataKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.BindAccelerationStructureMemoryInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferCopy',
-- 'Graphics.Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Graphics.Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.MemoryHeap',
-- 'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.StridedBufferRegionKHR',
-- 'Graphics.Vulkan.Core10.Image.SubresourceLayout',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdBuildAccelerationStructureIndirectKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdTraceRaysIndirectKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults',
-- 'Graphics.Vulkan.Core10.Memory.mapMemory'
type DeviceSize = Word64


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.BindVertexBufferIndirectCommandNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressConstKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressKHR'
type DeviceAddress = Word64

