{-# language CPP #-}
-- No documentation found for Chapter "PipelineCreationFeedbackFlagBits"
module Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits  ( pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
                                                             , pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
                                                             , pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
                                                             , PipelineCreationFeedbackFlags
                                                             , PipelineCreationFeedbackFlagBits( PIPELINE_CREATION_FEEDBACK_VALID_BIT
                                                                                               , PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT
                                                                                               , PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT
                                                                                               , ..
                                                                                               )
                                                             ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = PIPELINE_CREATION_FEEDBACK_VALID_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT


type PipelineCreationFeedbackFlags = PipelineCreationFeedbackFlagBits

-- | VkPipelineCreationFeedbackFlagBits - Bitmask specifying pipeline or
-- pipeline stage creation feedback
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_feedback VK_EXT_pipeline_creation_feedback>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedback',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo',
-- 'PipelineCreationFeedbackFlags'
newtype PipelineCreationFeedbackFlagBits = PipelineCreationFeedbackFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_CREATION_FEEDBACK_VALID_BIT' indicates that the feedback
-- information is valid.
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT = PipelineCreationFeedbackFlagBits 0x00000001

-- | 'PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT'
-- indicates that a readily usable pipeline or pipeline stage was found in
-- the @pipelineCache@ specified by the application in the pipeline
-- creation command.
--
-- An implementation /should/ set the
-- 'PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT' bit if
-- it was able to avoid the large majority of pipeline or pipeline stage
-- creation work by using the @pipelineCache@ parameter of
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV', or
-- 'Vulkan.Core10.Pipeline.createComputePipelines'. When an implementation
-- sets this bit for the entire pipeline, it /may/ leave it unset for any
-- stage.
--
-- Note
--
-- Implementations are encouraged to provide a meaningful signal to
-- applications using this bit. The intention is to communicate to the
-- application that the pipeline or pipeline stage was created “as fast as
-- it gets” using the pipeline cache provided by the application. If an
-- implementation uses an internal cache, it is discouraged from setting
-- this bit as the feedback would be unactionable.
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT = PipelineCreationFeedbackFlagBits 0x00000002

-- | 'PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT' indicates
-- that the base pipeline specified by the @basePipelineHandle@ or
-- @basePipelineIndex@ member of the @Vk*PipelineCreateInfo@ structure was
-- used to accelerate the creation of the pipeline.
--
-- An implementation /should/ set the
-- 'PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT' bit if it
-- was able to avoid a significant amount of work by using the base
-- pipeline.
--
-- Note
--
-- While “significant amount of work” is subjective, implementations are
-- encouraged to provide a meaningful signal to applications using this
-- bit. For example, a 1% reduction in duration may not warrant setting
-- this bit, while a 50% reduction would.
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT = PipelineCreationFeedbackFlagBits 0x00000004

conNamePipelineCreationFeedbackFlagBits :: String
conNamePipelineCreationFeedbackFlagBits = "PipelineCreationFeedbackFlagBits"

enumPrefixPipelineCreationFeedbackFlagBits :: String
enumPrefixPipelineCreationFeedbackFlagBits = "PIPELINE_CREATION_FEEDBACK_"

showTablePipelineCreationFeedbackFlagBits :: [(PipelineCreationFeedbackFlagBits, String)]
showTablePipelineCreationFeedbackFlagBits =
  [
    ( PIPELINE_CREATION_FEEDBACK_VALID_BIT
    , "VALID_BIT"
    )
  ,
    ( PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT
    , "APPLICATION_PIPELINE_CACHE_HIT_BIT"
    )
  ,
    ( PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT
    , "BASE_PIPELINE_ACCELERATION_BIT"
    )
  ]

instance Show PipelineCreationFeedbackFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCreationFeedbackFlagBits
      showTablePipelineCreationFeedbackFlagBits
      conNamePipelineCreationFeedbackFlagBits
      (\(PipelineCreationFeedbackFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCreationFeedbackFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCreationFeedbackFlagBits
      showTablePipelineCreationFeedbackFlagBits
      conNamePipelineCreationFeedbackFlagBits
      PipelineCreationFeedbackFlagBits
