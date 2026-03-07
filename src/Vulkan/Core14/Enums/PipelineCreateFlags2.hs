{-# language CPP #-}
-- No documentation found for Chapter "PipelineCreateFlags2"
module Vulkan.Core14.Enums.PipelineCreateFlags2  ( PipelineCreateFlags2
                                                 , PipelineCreateFlagBits2( PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT
                                                                          , PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT
                                                                          , PIPELINE_CREATE_2_DERIVATIVE_BIT
                                                                          , PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
                                                                          , PIPELINE_CREATE_2_DISPATCH_BASE_BIT
                                                                          , PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT
                                                                          , PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT
                                                                          , PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT
                                                                          , PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT
                                                                          , PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT
                                                                          , PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
                                                                          , PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM
                                                                          , PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT
                                                                          , PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR
                                                                          , PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV
                                                                          , PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV
                                                                          , PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
                                                                          , PIPELINE_CREATE_2_LIBRARY_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
                                                                          , PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT
                                                                          , PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV
                                                                          , PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV
                                                                          , PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX
                                                                          , ..
                                                                          )
                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags64)
type PipelineCreateFlags2 = PipelineCreateFlagBits2

-- | VkPipelineCreateFlagBits2 - Bitmask controlling how a pipeline is
-- created
--
-- = Description
--
-- -   'PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT' specifies that the
--     created pipeline will not be optimized. Using this flag /may/ reduce
--     the time taken to create the pipeline.
--
-- -   'PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT' specifies that the
--     pipeline to be created is allowed to be the parent of a pipeline
--     that will be created in a subsequent pipeline creation call.
--
-- -   'PIPELINE_CREATE_2_DERIVATIVE_BIT' specifies that the pipeline to be
--     created will be a child of a previously created parent pipeline.
--
-- -   'PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT' specifies that
--     any shader input variables decorated as @ViewIndex@ will be assigned
--     values as if they were decorated as @DeviceIndex@.
--
-- -   'PIPELINE_CREATE_2_DISPATCH_BASE_BIT' specifies that a compute
--     pipeline /can/ be used with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase'
--     with a non-zero base workgroup.
--
-- -   'PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV' specifies that a pipeline
--     is created with all shaders in the deferred state. Before using the
--     pipeline the application /must/ call
--     'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV' exactly once
--     on each shader in the pipeline before using the pipeline.
--
-- -   'PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR' specifies that the
--     shader compiler should capture statistics for the pipeline
--     executables produced by the compile process which /can/ later be
--     retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time.
--
-- -   'PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--     specifies that the shader compiler should capture the internal
--     representations of pipeline executables produced by the compile
--     process which /can/ later be retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time. When capturing IR from pipelines created with pipeline
--     libraries, there is no guarantee that IR from libraries /can/ be
--     retrieved from the linked pipeline. Applications /should/ retrieve
--     IR from each library, and any linked pipelines, separately.
--
-- -   'PIPELINE_CREATE_2_LIBRARY_BIT_KHR' specifies that the pipeline
--     /cannot/ be used directly, and instead defines a /pipeline library/
--     that /can/ be combined with other pipelines using the
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure. This is available in ray tracing and graphics pipelines.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--     specifies that an any-hit shader will always be present when an
--     any-hit shader would be executed. A NULL any-hit shader is an
--     any-hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--     specifies that a closest hit shader will always be present when a
--     closest hit shader would be executed. A NULL closest hit shader is a
--     closest hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--     specifies that a miss shader will always be present when a miss
--     shader would be executed. A NULL miss shader is a miss shader which
--     is effectively 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such
--     as from a shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--     specifies that an intersection shader will always be present when an
--     intersection shader would be executed. A NULL intersection shader is
--     an intersection shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR' specifies
--     that all built-in primitives including triangles, spheres, and LSS
--     primitives will be skipped during traversal using
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-pipeline-trace-ray pipeline trace ray>
--     instructions.
--
-- -   'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PIPELINE_CREATE_2_RAY_TRACING_SKIP_BUILT_IN_PRIMITIVES_BIT_KHR'
--     is an alias for
--     'PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR' specifies that
--     AABB primitives will be skipped during traversal using
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-pipeline-trace-ray pipeline trace ray>
--     instructions.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--     specifies that the shader group handles /can/ be saved and reused on
--     a subsequent run (e.g. for trace capture and replay).
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV'
--     specifies that the pipeline is allowed to use spheres or linear
--     swept spheres as a geometry type in the acceleration structures.
--     Using this flag /may/ affect performance.
--
-- -   'PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV' specifies that the
--     pipeline can be used in combination with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#device-generated-commands>.
--
-- -   'PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT' specifies that the
--     pipeline /can/ be used in a
--     'Vulkan.Extensions.Handles.IndirectExecutionSetEXT'.
--
-- -   'PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT' specifies
--     that pipeline creation will fail if a compile is required for
--     creation of a valid 'Vulkan.Core10.Handles.Pipeline' object;
--     'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED' will be
--     returned by pipeline creation, and the
--     'Vulkan.Core10.Handles.Pipeline' will be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- -   When creating multiple pipelines,
--     'PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT' specifies that
--     control will be returned to the application if any individual
--     pipeline returns a result which is not
--     'Vulkan.Core10.Enums.Result.SUCCESS' rather than continuing to
--     create additional pipelines.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV' specifies that
--     the pipeline is allowed to use @OpTraceRayMotionNV@.
--
-- -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     specifies that the pipeline will be used with a fragment shading
--     rate attachment.
--
-- -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--     specifies that the pipeline will be used with a fragment density map
--     attachment.
--
-- -   'PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE' specifies
--     that the pipeline /can/ be used with layered fragment density maps.
--
-- -   'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' specifies that
--     pipeline libraries being linked into this library /should/ have link
--     time optimizations applied. If this bit is omitted, implementations
--     /should/ instead perform linking as rapidly as possible.
--
-- -   'PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--     specifies that pipeline libraries should retain any information
--     necessary to later perform an optimal link with
--     'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT'.
--
-- -   'PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT' specifies that a
--     pipeline will be used with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorbuffers descriptor buffers>,
--     rather than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets descriptor sets>.
--
-- -   'PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT' specifies
--     that the pipeline /may/ be used with an attachment
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     including color attachments.
--
-- -   'PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     specifies that the pipeline /may/ be used with an attachment
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-feedbackloop feedback loop>
--     including depth-stencil attachments.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT' specifies
--     that the ray tracing pipeline /can/ be used with acceleration
--     structures which reference an opacity micromap array.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--     specifies that the ray tracing pipeline /can/ be used with
--     acceleration structures which reference a displacement micromap
--     array.
--
-- -   'PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT' specifies that the
--     pipeline /must/ not be bound to a protected command buffer.
--
-- -   'PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT' specifies that the
--     pipeline /must/ not be bound to an unprotected command buffer.
--
-- -   'PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR' specifies that
--     'Vulkan.Extensions.Handles.PipelineBinaryKHR' objects /can/ be
--     created from the pipeline. If
--     'PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR' is used, implementations
--     /should/ not store pipeline data to an internal cache, if such a
--     cache exists as stated by
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-pipelineBinaryInternalCache pipelineBinaryInternalCache>.
--     If
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-pipelineBinaryPrefersInternalCache pipelineBinaryPrefersInternalCache>
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', applications /should/ not
--     use 'PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'.
--
-- -   'PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT' specifies that
--     the pipeline will be used in a render pass that is begun with
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'.
--
-- -   'PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX' specifies that the
--     pipeline will be used in an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#executiongraphs execution graph>
--
-- -   'PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM' specifies that
--     the pipeline /must/ not be used with acceleration structures which
--     reference an opacity micromap array.
--
-- -   'PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT' specifies that the
--     pipeline enables
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-64bindexing 64-bit indexing>.
--
-- -   'PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT' specifies that the
--     pipeline will use descriptor heap mappings instead of descriptor set
--     layouts.
--
-- It is valid to set both 'PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT' and
-- 'PIPELINE_CREATE_2_DERIVATIVE_BIT'. This allows a pipeline to be both a
-- parent and possibly a child in a pipeline hierarchy. See
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- When an implementation is looking up a pipeline in a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-cache pipeline cache>,
-- if that pipeline is being created using linked libraries,
-- implementations /should/ always return an equivalent pipeline created
-- with 'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' if available,
-- whether or not that bit was specified.
--
-- Using 'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' (or not) when
-- linking pipeline libraries is intended as a performance tradeoff between
-- host and device. If the bit is omitted, linking should be faster and
-- produce a pipeline more rapidly, but performance of the pipeline on the
-- target device may be reduced. If the bit is included, linking may be
-- slower but should produce a pipeline with device performance comparable
-- to a monolithically created pipeline. Using both options can allow
-- latency-sensitive applications to generate a suboptimal but usable
-- pipeline quickly, and then perform an optimal link in the background,
-- substituting the result for the suboptimally linked pipeline as soon as
-- it is available.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'PipelineCreateFlags2'
newtype PipelineCreateFlagBits2 = PipelineCreateFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT"
pattern PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT = PipelineCreateFlagBits2 0x0000000000000001

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT"
pattern PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT = PipelineCreateFlagBits2 0x0000000000000002

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DERIVATIVE_BIT"
pattern PIPELINE_CREATE_2_DERIVATIVE_BIT = PipelineCreateFlagBits2 0x0000000000000004

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = PipelineCreateFlagBits2 0x0000000000000008

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DISPATCH_BASE_BIT"
pattern PIPELINE_CREATE_2_DISPATCH_BASE_BIT = PipelineCreateFlagBits2 0x0000000000000010

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT"
pattern PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT = PipelineCreateFlagBits2 0x0000000000000100

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT"
pattern PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT = PipelineCreateFlagBits2 0x0000000000000200

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT"
pattern PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT = PipelineCreateFlagBits2 0x0000000008000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT"
pattern PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT = PipelineCreateFlagBits2 0x0000000040000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT"
pattern PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT = PipelineCreateFlagBits2 0x0000080000000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
pattern PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE = PipelineCreateFlagBits2 0x0000010000000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM"
pattern PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM = PipelineCreateFlagBits2 0x0000002000000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT"
pattern PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT = PipelineCreateFlagBits2 0x0000004000000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR"
pattern PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR = PipelineCreateFlagBits2 0x0000000080000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT"
pattern PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT = PipelineCreateFlagBits2 0x0000000020000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV"
pattern PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV = PipelineCreateFlagBits2 0x0000000010000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits2 0x0000000004000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits2 0x0000000002000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
pattern PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT = PipelineCreateFlagBits2 0x0000000001000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
pattern PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT = PipelineCreateFlagBits2 0x0000000000400000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineCreateFlagBits2 0x0000000000200000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV"
pattern PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV = PipelineCreateFlagBits2 0x0000000000100000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV"
pattern PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV = PipelineCreateFlagBits2 0x0000000000040000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR = PipelineCreateFlagBits2 0x0000000000080000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000020000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000010000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000008000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000004000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000002000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR = PipelineCreateFlagBits2 0x0000000000001000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_LIBRARY_BIT_KHR"
pattern PIPELINE_CREATE_2_LIBRARY_BIT_KHR = PipelineCreateFlagBits2 0x0000000000000800

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
pattern PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT = PipelineCreateFlagBits2 0x0000000000800000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT"
pattern PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT = PipelineCreateFlagBits2 0x0000000000000400

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
pattern PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000000080

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR"
pattern PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR = PipelineCreateFlagBits2 0x0000000000000040

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV"
pattern PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV = PipelineCreateFlagBits2 0x0000000000000020

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT"
pattern PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT = PipelineCreateFlagBits2 0x0000000400000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV"
pattern PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV = PipelineCreateFlagBits2 0x0000000200000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT"
pattern PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT = PipelineCreateFlagBits2 0x0000001000000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2" "VK_PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX"
pattern PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX = PipelineCreateFlagBits2 0x0000000100000000

conNamePipelineCreateFlagBits2 :: String
conNamePipelineCreateFlagBits2 = "PipelineCreateFlagBits2"

enumPrefixPipelineCreateFlagBits2 :: String
enumPrefixPipelineCreateFlagBits2 = "PIPELINE_CREATE_2_"

showTablePipelineCreateFlagBits2 :: [(PipelineCreateFlagBits2, String)]
showTablePipelineCreateFlagBits2 =
  [
    ( PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT
    , "DISABLE_OPTIMIZATION_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT
    , "ALLOW_DERIVATIVES_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_DERIVATIVE_BIT
    , "DERIVATIVE_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
    , "VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_DISPATCH_BASE_BIT
    , "DISPATCH_BASE_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT
    , "FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT
    , "EARLY_RETURN_ON_FAILURE_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT
    , "NO_PROTECTED_ACCESS_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT
    , "PROTECTED_ACCESS_ONLY_BIT"
    )
  ,
    ( PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT
    , "64_BIT_INDEXING_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE
    , "PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE"
    )
  ,
    ( PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM
    , "DISALLOW_OPACITY_MICROMAP_BIT_ARM"
    )
  ,
    ( PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT
    , "INDIRECT_BINDABLE_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR
    , "CAPTURE_DATA_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT
    , "DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV
    , "RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
    , "RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
    , "RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV
    , "RAY_TRACING_ALLOW_MOTION_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV
    , "INDIRECT_BINDABLE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
    , "RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR
    , "RAY_TRACING_SKIP_AABBS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
    , "RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_LIBRARY_BIT_KHR
    , "LIBRARY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
    , "RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT
    , "LINK_TIME_OPTIMIZATION_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
    , "CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR
    , "CAPTURE_STATISTICS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV
    , "DEFER_COMPILE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT
    , "ENABLE_LEGACY_DITHERING_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV
    , "RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT
    , "DESCRIPTOR_HEAP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX
    , "EXECUTION_GRAPH_BIT_AMDX"
    )
  ]

instance Show PipelineCreateFlagBits2 where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCreateFlagBits2
      showTablePipelineCreateFlagBits2
      conNamePipelineCreateFlagBits2
      (\(PipelineCreateFlagBits2 x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCreateFlagBits2 where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCreateFlagBits2
      showTablePipelineCreateFlagBits2
      conNamePipelineCreateFlagBits2
      PipelineCreateFlagBits2
