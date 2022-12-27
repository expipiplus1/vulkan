{-# language CPP #-}
-- No documentation found for Chapter "PipelineCreateFlagBits"
module Vulkan.Core10.Enums.PipelineCreateFlagBits  ( PipelineCreateFlags
                                                   , PipelineCreateFlagBits( PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
                                                                           , PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
                                                                           , PIPELINE_CREATE_DERIVATIVE_BIT
                                                                           , PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT
                                                                           , PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT
                                                                           , PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
                                                                           , PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                           , PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                           , PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV
                                                                           , PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT
                                                                           , PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
                                                                           , PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT
                                                                           , PIPELINE_CREATE_LIBRARY_BIT_KHR
                                                                           , PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV
                                                                           , PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
                                                                           , PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR
                                                                           , PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
                                                                           , PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
                                                                           , PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
                                                                           , PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                           , PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT
                                                                           , PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT
                                                                           , PIPELINE_CREATE_DISPATCH_BASE_BIT
                                                                           , PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
                                                                           , ..
                                                                           )
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineCreateFlags = PipelineCreateFlagBits

-- | VkPipelineCreateFlagBits - Bitmask controlling how a pipeline is created
--
-- = Description
--
-- -   'PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT' specifies that the
--     created pipeline will not be optimized. Using this flag /may/ reduce
--     the time taken to create the pipeline.
--
-- -   'PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT' specifies that the pipeline
--     to be created is allowed to be the parent of a pipeline that will be
--     created in a subsequent pipeline creation call.
--
-- -   'PIPELINE_CREATE_DERIVATIVE_BIT' specifies that the pipeline to be
--     created will be a child of a previously created parent pipeline.
--
-- -   'PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT' specifies that
--     any shader input variables decorated as @ViewIndex@ will be assigned
--     values as if they were decorated as @DeviceIndex@.
--
-- -   'Vulkan.Core11.Promoted_From_VK_KHR_device_group.PIPELINE_CREATE_DISPATCH_BASE'
--     specifies that a compute pipeline /can/ be used with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase'
--     with a non-zero base workgroup.
--
-- -   'PIPELINE_CREATE_DEFER_COMPILE_BIT_NV' specifies that a pipeline is
--     created with all shaders in the deferred state. Before using the
--     pipeline the application /must/ call
--     'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV' exactly once
--     on each shader in the pipeline before using the pipeline.
--
-- -   'PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR' specifies that the
--     shader compiler should capture statistics for the pipeline
--     executables produced by the compile process which /can/ later be
--     retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time.
--
-- -   'PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR' specifies
--     that the shader compiler should capture the internal representations
--     of pipeline executables produced by the compile process which /can/
--     later be retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time. When capturing IR from pipelines created with pipeline
--     libraries, there is no guarantee that IR from libraries /can/ be
--     retrieved from the linked pipeline. Applications /should/ retrieve
--     IR from each library, and any linked pipelines, separately.
--
-- -   'PIPELINE_CREATE_LIBRARY_BIT_KHR' specifies that the pipeline
--     /cannot/ be used directly, and instead defines a /pipeline library/
--     that /can/ be combined with other pipelines using the
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure. This is available in ray tracing and graphics pipelines.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--     specifies that an any-hit shader will always be present when an
--     any-hit shader would be executed. A NULL any-hit shader is an
--     any-hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--     specifies that a closest hit shader will always be present when a
--     closest hit shader would be executed. A NULL closest hit shader is a
--     closest hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR' specifies
--     that a miss shader will always be present when a miss shader would
--     be executed. A NULL miss shader is a miss shader which is
--     effectively 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as
--     from a shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--     specifies that an intersection shader will always be present when an
--     intersection shader would be executed. A NULL intersection shader is
--     an intersection shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR' specifies that
--     triangle primitives will be skipped during traversal using
--     @OpTraceRayKHR@.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR' specifies that AABB
--     primitives will be skipped during traversal using @OpTraceRayKHR@.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--     specifies that the shader group handles /can/ be saved and reused on
--     a subsequent run (e.g. for trace capture and replay).
--
-- -   'PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV' specifies that the
--     pipeline can be used in combination with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-generated-commands>.
--
-- -   'PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT' specifies
--     that pipeline creation will fail if a compile is required for
--     creation of a valid 'Vulkan.Core10.Handles.Pipeline' object;
--     'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED' will be
--     returned by pipeline creation, and the
--     'Vulkan.Core10.Handles.Pipeline' will be set to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- -   When creating multiple pipelines,
--     'PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT' specifies that control
--     will be returned to the application if any individual pipeline
--     returns a result which is not 'Vulkan.Core10.Enums.Result.SUCCESS'
--     rather than continuing to create additional pipelines.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV' specifies that the
--     pipeline is allowed to use @OpTraceRayMotionNV@.
--
-- -   'PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     specifies that the pipeline will be used with a fragment shading
--     rate attachment.
--
-- -   'PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--     specifies that the pipeline will be used with a fragment density map
--     attachment.
--
-- -   'PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT' specifies that
--     pipeline libraries being linked into this library /should/ have link
--     time optimizations applied. If this bit is omitted, implementations
--     /should/ instead perform linking as rapidly as possible.
--
-- -   'PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--     specifies that pipeline libraries should retain any information
--     necessary to later perform an optimal link with
--     'PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'.
--
-- -   'PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT' specifies that a
--     pipeline will be used with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorbuffers descriptor buffers>,
--     rather than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets descriptor sets>.
--
-- -   'PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT' specifies
--     that the pipeline /may/ be used with an attachment feedback loop
--     including color attachments.
--
-- -   'PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     specifies that the pipeline /may/ be used with an attachment
--     feedback loop including depth-stencil attachments.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT' specifies
--     that the pipeline /can/ be used with acceleration structures which
--     reference an opacity micromap array.
--
-- -   'PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT' specifies that the
--     pipeline /must/ not be bound to a protected command buffer.
--
-- -   'PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT' specifies that the
--     pipeline /must/ not be bound to an unprotected command buffer.
--
-- It is valid to set both 'PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT' and
-- 'PIPELINE_CREATE_DERIVATIVE_BIT'. This allows a pipeline to be both a
-- parent and possibly a child in a pipeline hierarchy. See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- When an implementation is looking up a pipeline in a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-cache pipeline cache>,
-- if that pipeline is being created using linked libraries,
-- implementations /should/ always return an equivalent pipeline created
-- with 'PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT' if available,
-- whether or not that bit was specified.
--
-- Note
--
-- Using 'PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT' (or not) when
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'PipelineCreateFlags'
newtype PipelineCreateFlagBits = PipelineCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = PipelineCreateFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = PipelineCreateFlagBits 0x00000002

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
pattern PIPELINE_CREATE_DERIVATIVE_BIT = PipelineCreateFlagBits 0x00000004

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT"
pattern PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT = PipelineCreateFlagBits 0x40000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT"
pattern PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT = PipelineCreateFlagBits 0x08000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
pattern PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT = PipelineCreateFlagBits 0x01000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits 0x04000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits 0x02000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV"
pattern PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV = PipelineCreateFlagBits 0x00100000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT"
pattern PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT = PipelineCreateFlagBits 0x00000400

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
pattern PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT = PipelineCreateFlagBits 0x00800000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT"
pattern PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT = PipelineCreateFlagBits 0x20000000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_LIBRARY_BIT_KHR"
pattern PIPELINE_CREATE_LIBRARY_BIT_KHR = PipelineCreateFlagBits 0x00000800

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV"
pattern PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV = PipelineCreateFlagBits 0x00040000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
pattern PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR = PipelineCreateFlagBits 0x00000080

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR"
pattern PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR = PipelineCreateFlagBits 0x00000040

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV = PipelineCreateFlagBits 0x00000020

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR = PipelineCreateFlagBits 0x00080000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR = PipelineCreateFlagBits 0x00002000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR = PipelineCreateFlagBits 0x00001000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00020000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00010000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00008000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits 0x00004000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
pattern PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT = PipelineCreateFlagBits 0x00400000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineCreateFlagBits 0x00200000

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT"
pattern PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT = PipelineCreateFlagBits 0x00000200

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT"
pattern PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT = PipelineCreateFlagBits 0x00000100

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISPATCH_BASE_BIT"
pattern PIPELINE_CREATE_DISPATCH_BASE_BIT = PipelineCreateFlagBits 0x00000010

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = PipelineCreateFlagBits 0x00000008

conNamePipelineCreateFlagBits :: String
conNamePipelineCreateFlagBits = "PipelineCreateFlagBits"

enumPrefixPipelineCreateFlagBits :: String
enumPrefixPipelineCreateFlagBits = "PIPELINE_CREATE_"

showTablePipelineCreateFlagBits :: [(PipelineCreateFlagBits, String)]
showTablePipelineCreateFlagBits =
  [
    ( PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
    , "DISABLE_OPTIMIZATION_BIT"
    )
  ,
    ( PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
    , "ALLOW_DERIVATIVES_BIT"
    )
  ,
    ( PIPELINE_CREATE_DERIVATIVE_BIT
    , "DERIVATIVE_BIT"
    )
  ,
    ( PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT
    , "PROTECTED_ACCESS_ONLY_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT
    , "NO_PROTECTED_ACCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
    , "RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV
    , "RAY_TRACING_ALLOW_MOTION_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT
    , "LINK_TIME_OPTIMIZATION_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
    , "RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT
    , "DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_LIBRARY_BIT_KHR
    , "LIBRARY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV
    , "INDIRECT_BINDABLE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
    , "CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR
    , "CAPTURE_STATISTICS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
    , "DEFER_COMPILE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
    , "RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR
    , "RAY_TRACING_SKIP_AABBS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
    , "RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
    , "RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT
    , "EARLY_RETURN_ON_FAILURE_BIT"
    )
  ,
    ( PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT
    , "FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT"
    )
  ,
    ( PIPELINE_CREATE_DISPATCH_BASE_BIT
    , "DISPATCH_BASE_BIT"
    )
  ,
    ( PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
    , "VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
    )
  ]

instance Show PipelineCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCreateFlagBits
      showTablePipelineCreateFlagBits
      conNamePipelineCreateFlagBits
      (\(PipelineCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCreateFlagBits
      showTablePipelineCreateFlagBits
      conNamePipelineCreateFlagBits
      PipelineCreateFlagBits
