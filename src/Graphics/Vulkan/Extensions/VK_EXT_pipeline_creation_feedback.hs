{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( withCStructPipelineCreationFeedbackCreateInfoEXT
  , fromCStructPipelineCreationFeedbackCreateInfoEXT
  , PipelineCreationFeedbackCreateInfoEXT(..)
  , withCStructPipelineCreationFeedbackEXT
  , fromCStructPipelineCreationFeedbackEXT
  , PipelineCreationFeedbackEXT(..)
  , PipelineCreationFeedbackFlagBitsEXT
  , pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  , pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , PipelineCreationFeedbackFlagsEXT
  , pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word64
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , VkPipelineCreationFeedbackFlagBitsEXT(..)
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  )



-- | VkPipelineCreationFeedbackCreateInfoEXT - Request for feedback about the
-- creation of a pipeline
--
-- = Description
--
-- An implementation /should/ write pipeline creation feedback to
-- @pPipelineCreationFeedback@ and /may/ write pipeline stage creation
-- feedback to @pPipelineStageCreationFeedbacks@. An implementation /must/
-- set or clear the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT'
-- in
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'::@flags@
-- for @pPipelineCreationFeedback@ and every element of
-- @pPipelineStageCreationFeedbacks@.
--
-- __Note__
--
-- One common scenario for an implementation to skip per-stage feedback is
-- when
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- is set in @pPipelineCreationFeedback@.
--
-- When chained to
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo', the
-- @i@ element of @pPipelineStageCreationFeedbacks@ corresponds to the @i@
-- element of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@pStages@.
-- When chained to
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo', the
-- first element of @pPipelineStageCreationFeedbacks@ corresponds to
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo'::@stage@.
--
-- == Valid Usage
--
-- -   When chained to
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@stageCount@
--
-- -   When chained to
--     'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal 1
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT'
--
-- -   @pPipelineCreationFeedback@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'
--     structure
--
-- -   @pPipelineStageCreationFeedbacks@ /must/ be a valid pointer to an
--     array of @pipelineStageCreationFeedbackCount@
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'
--     structures
--
-- -   @pipelineStageCreationFeedbackCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineCreationFeedback"
  pipelineCreationFeedback :: PipelineCreationFeedbackEXT
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineCreationFeedbackCreateInfoEXT" "pPipelineStageCreationFeedbacks"
  pipelineStageCreationFeedbacks :: Vector PipelineCreationFeedbackEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineCreationFeedbackCreateInfoEXT' and
-- marshal a 'PipelineCreationFeedbackCreateInfoEXT' into it. The 'VkPipelineCreationFeedbackCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineCreationFeedbackCreateInfoEXT :: PipelineCreationFeedbackCreateInfoEXT -> (VkPipelineCreationFeedbackCreateInfoEXT -> IO a) -> IO a
withCStructPipelineCreationFeedbackCreateInfoEXT marshalled cont = withVec withCStructPipelineCreationFeedbackEXT (pipelineStageCreationFeedbacks (marshalled :: PipelineCreationFeedbackCreateInfoEXT)) (\pPPipelineStageCreationFeedbacks -> (\a -> withCStructPipelineCreationFeedbackEXT a . flip with) (pipelineCreationFeedback (marshalled :: PipelineCreationFeedbackCreateInfoEXT)) (\pPPipelineCreationFeedback -> maybeWith withSomeVkStruct (next (marshalled :: PipelineCreationFeedbackCreateInfoEXT)) (\pPNext -> cont (VkPipelineCreationFeedbackCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT pPNext pPPipelineCreationFeedback (fromIntegral (Data.Vector.length (pipelineStageCreationFeedbacks (marshalled :: PipelineCreationFeedbackCreateInfoEXT)))) pPPipelineStageCreationFeedbacks))))

-- | A function to read a 'VkPipelineCreationFeedbackCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineCreationFeedbackCreateInfoEXT'.
fromCStructPipelineCreationFeedbackCreateInfoEXT :: VkPipelineCreationFeedbackCreateInfoEXT -> IO PipelineCreationFeedbackCreateInfoEXT
fromCStructPipelineCreationFeedbackCreateInfoEXT c = PipelineCreationFeedbackCreateInfoEXT <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineCreationFeedbackCreateInfoEXT)))
                                                                                           <*> (fromCStructPipelineCreationFeedbackEXT <=< peek) (vkPPipelineCreationFeedback (c :: VkPipelineCreationFeedbackCreateInfoEXT))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkPipelineStageCreationFeedbackCount (c :: VkPipelineCreationFeedbackCreateInfoEXT))) (((fromCStructPipelineCreationFeedbackEXT <=<) . peekElemOff) (vkPPipelineStageCreationFeedbacks (c :: VkPipelineCreationFeedbackCreateInfoEXT))))

instance Zero PipelineCreationFeedbackCreateInfoEXT where
  zero = PipelineCreationFeedbackCreateInfoEXT Nothing
                                               zero
                                               Data.Vector.empty



-- | VkPipelineCreationFeedbackEXT - Feedback about the creation of a
-- pipeline or pipeline stage
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT'
-- is not set in @flags@, an implementation /must/ not set any other bits
-- in @flags@, and all other
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT'
-- data members are undefined.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagsEXT'
data PipelineCreationFeedbackEXT = PipelineCreationFeedbackEXT
  { -- No documentation found for Nested "PipelineCreationFeedbackEXT" "flags"
  flags :: PipelineCreationFeedbackFlagsEXT
  , -- No documentation found for Nested "PipelineCreationFeedbackEXT" "duration"
  duration :: Word64
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineCreationFeedbackEXT' and
-- marshal a 'PipelineCreationFeedbackEXT' into it. The 'VkPipelineCreationFeedbackEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineCreationFeedbackEXT :: PipelineCreationFeedbackEXT -> (VkPipelineCreationFeedbackEXT -> IO a) -> IO a
withCStructPipelineCreationFeedbackEXT marshalled cont = cont (VkPipelineCreationFeedbackEXT (flags (marshalled :: PipelineCreationFeedbackEXT)) (duration (marshalled :: PipelineCreationFeedbackEXT)))

-- | A function to read a 'VkPipelineCreationFeedbackEXT' and all additional
-- structures in the pointer chain into a 'PipelineCreationFeedbackEXT'.
fromCStructPipelineCreationFeedbackEXT :: VkPipelineCreationFeedbackEXT -> IO PipelineCreationFeedbackEXT
fromCStructPipelineCreationFeedbackEXT c = PipelineCreationFeedbackEXT <$> pure (vkFlags (c :: VkPipelineCreationFeedbackEXT))
                                                                       <*> pure (vkDuration (c :: VkPipelineCreationFeedbackEXT))

instance Zero PipelineCreationFeedbackEXT where
  zero = PipelineCreationFeedbackEXT zero
                                     zero


-- | VkPipelineCreationFeedbackFlagBitsEXT - Bitmask specifying pipeline or
-- pipeline stage creation feedback
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagsEXT'
type PipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT


{-# complete PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT, PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT, PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: PipelineCreationFeedbackFlagBitsEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT'
-- indicates that the feedback information is valid.
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- indicates that a readily usable pipeline or pipeline stage was found in
-- the @pipelineCache@ specified by the application in the pipeline
-- creation command.
--
-- An implementation /should/ set the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- bit if it was able to avoid the large majority of pipeline or pipeline
-- stage creation work by using the @pipelineCache@ parameter of
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines', or
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines'. When an
-- implementation sets this bit for the entire pipeline, it /may/ leave it
-- unset for any stage.
--
-- __Note__
--
-- Implementations are encouraged to provide a meaningful signal to
-- applications using this bit. The intention is to communicate to the
-- application that the pipeline or pipeline stage was created \"as fast as
-- it gets\" using the pipeline cache provided by the application. If an
-- implementation uses an internal cache, it is discouraged from setting
-- this bit as the feedback would be unactionable.
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT'
-- indicates that the base pipeline specified by the @basePipelineHandle@
-- or @basePipelineIndex@ member of the @Vk*PipelineCreateInfo@ structure
-- was used to accelerate the creation of the pipeline.
--
-- An implementation /should/ set the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT'
-- bit if it was able to avoid a significant amount of work by using the
-- base pipeline.
--
-- __Note__
--
-- While \"significant amount of work\" is subjective, implementations are
-- encouraged to provide a meaningful signal to applications using this
-- bit. For example, a 1% reduction in duration may not warrant setting
-- this bit, while a 50% reduction would.
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: (a ~ PipelineCreationFeedbackFlagBitsEXT) => a
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT

-- | VkPipelineCreationFeedbackFlagsEXT - Bitmask of
-- VkPipelineCreationFeedbackFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagsEXT'
-- is a bitmask type for providing zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackFlagBitsEXT'
type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
