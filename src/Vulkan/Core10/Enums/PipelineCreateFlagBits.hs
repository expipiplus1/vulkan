{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineCreateFlagBits  ( PipelineCreateFlagBits( PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
                                                                           , PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
                                                                           , PIPELINE_CREATE_DERIVATIVE_BIT
                                                                           , PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT
                                                                           , PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT
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
                                                                           , PIPELINE_CREATE_DISPATCH_BASE_BIT
                                                                           , PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
                                                                           , ..
                                                                           )
                                                   , PipelineCreateFlags
                                                   ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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
--     shader compiler should capture statistics for the executables
--     produced by the compile process which /can/ later be retrieved by
--     calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time.
--
-- -   'PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR' specifies
--     that the shader compiler should capture the internal representations
--     of executables produced by the compile process which /can/ later be
--     retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time.
--
-- -   'PIPELINE_CREATE_LIBRARY_BIT_KHR' specifies that the pipeline
--     /cannot/ be used directly, and instead defines a /pipeline library/
--     that /can/ be combined with other pipelines using the
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure. This is available in ray tracing pipelines.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--     specifies that an any-hit shader will always be present when an
--     any-hit shader would be executed.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--     specifies that a closest hit shader will always be present when a
--     closest hit shader would be executed.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR' specifies
--     that a miss shader will always be present when a miss shader would
--     be executed.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--     specifies that an intersection shader will always be present when an
--     intersection shader would be executed.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR' specifies that
--     triangle primitives will be skipped during traversal using
--     @OpTraceKHR@.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR' specifies that AABB
--     primitives will be skipped during traversal using @OpTraceKHR@.
--
-- -   'PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--     specifies that the shader group handles /can/ be saved and reused on
--     a subsequent run (e.g. for trace capture and replay).
--
-- -   'PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV' specifies that the
--     pipeline can be used in combination with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#device-generated-commands>.
--
-- -   'PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     specifies that pipeline creation will fail if a compile is required
--     for creation of a valid 'Vulkan.Core10.Handles.Pipeline' object;
--     'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT' will be
--     returned by pipeline creation, and the
--     'Vulkan.Core10.Handles.Pipeline' will be set to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- -   When creating multiple pipelines,
--     'PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT' specifies that
--     control will be returned to the application on failure of the
--     corresponding pipeline rather than continuing to create additional
--     pipelines.
--
-- It is valid to set both 'PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT' and
-- 'PIPELINE_CREATE_DERIVATIVE_BIT'. This allows a pipeline to be both a
-- parent and possibly a child in a pipeline hierarchy. See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- = See Also
--
-- 'PipelineCreateFlags'
newtype PipelineCreateFlagBits = PipelineCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = PipelineCreateFlagBits 0x00000001
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = PipelineCreateFlagBits 0x00000002
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
pattern PIPELINE_CREATE_DERIVATIVE_BIT = PipelineCreateFlagBits 0x00000004
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT"
pattern PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT = PipelineCreateFlagBits 0x00000200
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT"
pattern PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT = PipelineCreateFlagBits 0x00000100
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
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISPATCH_BASE_BIT"
pattern PIPELINE_CREATE_DISPATCH_BASE_BIT = PipelineCreateFlagBits 0x00000010
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = PipelineCreateFlagBits 0x00000008

type PipelineCreateFlags = PipelineCreateFlagBits

instance Show PipelineCreateFlagBits where
  showsPrec p = \case
    PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT -> showString "PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
    PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT -> showString "PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
    PIPELINE_CREATE_DERIVATIVE_BIT -> showString "PIPELINE_CREATE_DERIVATIVE_BIT"
    PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT -> showString "PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT"
    PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT -> showString "PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT"
    PIPELINE_CREATE_LIBRARY_BIT_KHR -> showString "PIPELINE_CREATE_LIBRARY_BIT_KHR"
    PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV -> showString "PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV"
    PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR -> showString "PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
    PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR -> showString "PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR"
    PIPELINE_CREATE_DEFER_COMPILE_BIT_NV -> showString "PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
    PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
    PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR -> showString "PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
    PIPELINE_CREATE_DISPATCH_BASE_BIT -> showString "PIPELINE_CREATE_DISPATCH_BASE_BIT"
    PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT -> showString "PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
    PipelineCreateFlagBits x -> showParen (p >= 11) (showString "PipelineCreateFlagBits 0x" . showHex x)

instance Read PipelineCreateFlagBits where
  readPrec = parens (choose [("PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", pure PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT)
                            , ("PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT", pure PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT)
                            , ("PIPELINE_CREATE_DERIVATIVE_BIT", pure PIPELINE_CREATE_DERIVATIVE_BIT)
                            , ("PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT", pure PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT)
                            , ("PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT", pure PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT)
                            , ("PIPELINE_CREATE_LIBRARY_BIT_KHR", pure PIPELINE_CREATE_LIBRARY_BIT_KHR)
                            , ("PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV", pure PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV)
                            , ("PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR", pure PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR)
                            , ("PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR", pure PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR)
                            , ("PIPELINE_CREATE_DEFER_COMPILE_BIT_NV", pure PIPELINE_CREATE_DEFER_COMPILE_BIT_NV)
                            , ("PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR)
                            , ("PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR", pure PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR)
                            , ("PIPELINE_CREATE_DISPATCH_BASE_BIT", pure PIPELINE_CREATE_DISPATCH_BASE_BIT)
                            , ("PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT", pure PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCreateFlagBits")
                       v <- step readPrec
                       pure (PipelineCreateFlagBits v)))

