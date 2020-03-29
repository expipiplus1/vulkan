{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits  ( PipelineCreateFlagBits( PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
                                                                                    , PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
                                                                                    , PIPELINE_CREATE_DERIVATIVE_BIT
                                                                                    , PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
                                                                                    , PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR
                                                                                    , PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
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
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkPipelineCreateFlagBits - Bitmask controlling how a pipeline is created
--
-- = Description
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT' specifies that the created
-- pipeline will not be optimized. Using this flag /may/ reduce the time
-- taken to create the pipeline.
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = PipelineCreateFlagBits 0x00000001
-- | 'PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT' specifies that the pipeline to
-- be created is allowed to be the parent of a pipeline that will be
-- created in a subsequent call to
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines' or
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines'.
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = PipelineCreateFlagBits 0x00000002
-- | 'PIPELINE_CREATE_DERIVATIVE_BIT' specifies that the pipeline to be
-- created will be a child of a previously created parent pipeline.
pattern PIPELINE_CREATE_DERIVATIVE_BIT = PipelineCreateFlagBits 0x00000004
-- | 'PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR' specifies
-- that the shader compiler should capture the internal representations of
-- executables produced by the compile process which /can/ later be
-- retrieved by calling
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR'.
-- Enabling this flag /must/ not affect the final compiled pipeline but
-- /may/ disable pipeline caching or otherwise affect pipeline creation
-- time.
pattern PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR = PipelineCreateFlagBits 0x00000080
-- | 'PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR' specifies that the shader
-- compiler should capture statistics for the executables produced by the
-- compile process which /can/ later be retrieved by calling
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR'.
-- Enabling this flag /must/ not affect the final compiled pipeline but
-- /may/ disable pipeline caching or otherwise affect pipeline creation
-- time.
pattern PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR = PipelineCreateFlagBits 0x00000040
-- | 'PIPELINE_CREATE_DEFER_COMPILE_BIT_NV' specifies that a pipeline is
-- created with all shaders in the deferred state. Before using the
-- pipeline the application /must/ call
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV' exactly
-- once on each shader in the pipeline before using the pipeline.
pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV = PipelineCreateFlagBits 0x00000020
-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISPATCH_BASE_BIT"
pattern PIPELINE_CREATE_DISPATCH_BASE_BIT = PipelineCreateFlagBits 0x00000010
-- | 'PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT' specifies that any
-- shader input variables decorated as @ViewIndex@ will be assigned values
-- as if they were decorated as @DeviceIndex@.
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = PipelineCreateFlagBits 0x00000008

type PipelineCreateFlags = PipelineCreateFlagBits

instance Show PipelineCreateFlagBits where
  showsPrec p = \case
    PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT -> showString "PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
    PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT -> showString "PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
    PIPELINE_CREATE_DERIVATIVE_BIT -> showString "PIPELINE_CREATE_DERIVATIVE_BIT"
    PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR -> showString "PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
    PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR -> showString "PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR"
    PIPELINE_CREATE_DEFER_COMPILE_BIT_NV -> showString "PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
    PIPELINE_CREATE_DISPATCH_BASE_BIT -> showString "PIPELINE_CREATE_DISPATCH_BASE_BIT"
    PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT -> showString "PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
    PipelineCreateFlagBits x -> showParen (p >= 11) (showString "PipelineCreateFlagBits 0x" . showHex x)

instance Read PipelineCreateFlagBits where
  readPrec = parens (choose [("PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", pure PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT)
                            , ("PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT", pure PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT)
                            , ("PIPELINE_CREATE_DERIVATIVE_BIT", pure PIPELINE_CREATE_DERIVATIVE_BIT)
                            , ("PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR", pure PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR)
                            , ("PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR", pure PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR)
                            , ("PIPELINE_CREATE_DEFER_COMPILE_BIT_NV", pure PIPELINE_CREATE_DEFER_COMPILE_BIT_NV)
                            , ("PIPELINE_CREATE_DISPATCH_BASE_BIT", pure PIPELINE_CREATE_DISPATCH_BASE_BIT)
                            , ("PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT", pure PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCreateFlagBits")
                       v <- step readPrec
                       pure (PipelineCreateFlagBits v)))

