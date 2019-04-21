{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , VkPipelineCreationFeedbackFlagBitsEXT(..)
  , pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , VkPipelineCreationFeedbackFlagsEXT
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  , VkFlags
  )


-- | VkPipelineCreationFeedbackCreateInfoEXT - Request for feedback about the
-- creation of a pipeline
--
-- = Description
--
-- An implementation /should/ write pipeline creation feedback to
-- @pPipelineCreationFeedback@ and /may/ write pipeline stage creation
-- feedback to @pPipelineStageCreationFeedbacks@. An implementation /must/
-- set or clear the 'VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' in
-- 'VkPipelineCreationFeedbackEXT'::@flags@ for @pPipelineCreationFeedback@
-- and every element of @pPipelineStageCreationFeedbacks@.
--
-- __Note__
--
-- One common scenario for an implementation to skip per-stage feedback is
-- when
-- 'VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- is set in @pPipelineCreationFeedback@.
--
-- When chained to
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
-- or 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo', the
-- @i@ element of @pPipelineStageCreationFeedbacks@ corresponds to the @i@
-- element of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'::@pStages@
-- or
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
--     'VkPipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'::@stageCount@
--
-- -   When chained to
--     'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
--     'VkPipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal 1
--
-- -   When chained to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV',
--     'VkPipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'::@stageCount@
--
-- Unresolved directive in VkPipelineCreationFeedbackCreateInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineCreationFeedbackCreateInfoEXT.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'VkPipelineCreationFeedbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
data VkPipelineCreationFeedbackCreateInfoEXT = VkPipelineCreationFeedbackCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pPipelineCreationFeedback@ is a pointer to a
  -- 'VkPipelineCreationFeedbackEXT' structure.
  vkPPipelineCreationFeedback :: Ptr VkPipelineCreationFeedbackEXT
  , -- | @pipelineStageCreationFeedbackCount@ is the number of elements in
  -- @pPipelineStageCreationFeedbacks@.
  vkPipelineStageCreationFeedbackCount :: Word32
  , -- | @pPipelineStageCreationFeedbacks@ is an array of size
  -- @pipelineStageCreationFeedbackCount@ of 'VkPipelineCreationFeedbackEXT'
  -- structures.
  vkPPipelineStageCreationFeedbacks :: Ptr VkPipelineCreationFeedbackEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineCreationFeedbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCreationFeedbackCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPPipelineCreationFeedback (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPipelineStageCreationFeedbackCount (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPPipelineStageCreationFeedbacks (poked :: VkPipelineCreationFeedbackCreateInfoEXT))

instance Zero VkPipelineCreationFeedbackCreateInfoEXT where
  zero = VkPipelineCreationFeedbackCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
                                                 zero
                                                 zero
                                                 zero
                                                 zero

-- | VkPipelineCreationFeedbackEXT - Feedback about the creation of a
-- pipeline or pipeline stage
--
-- = Description
--
-- If the 'VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' is not set in
-- @flags@, an implementation /must/ not set any other bits in @flags@, and
-- all other 'VkPipelineCreationFeedbackEXT' data members are undefined.
--
-- Unresolved directive in VkPipelineCreationFeedbackEXT.txt -
-- include::{generated}\/validity\/structs\/VkPipelineCreationFeedbackEXT.txt[]
--
-- = See Also
--
-- 'VkPipelineCreationFeedbackCreateInfoEXT',
-- 'VkPipelineCreationFeedbackFlagBitsEXT'
data VkPipelineCreationFeedbackEXT = VkPipelineCreationFeedbackEXT
  { -- | @flags@ is a bitmask of 'VkPipelineCreationFeedbackFlagBitsEXT'
  -- providing feedback about the creation of a pipeline or of a pipeline
  -- stage.
  vkFlags :: VkPipelineCreationFeedbackFlagsEXT
  , -- | @duration@ is the duration spent creating a pipeline or pipeline stage
  -- in nanoseconds.
  vkDuration :: Word64
  }
  deriving (Eq, Show)

instance Storable VkPipelineCreationFeedbackEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPipelineCreationFeedbackEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkPipelineCreationFeedbackEXT))
                *> poke (ptr `plusPtr` 8) (vkDuration (poked :: VkPipelineCreationFeedbackEXT))

instance Zero VkPipelineCreationFeedbackEXT where
  zero = VkPipelineCreationFeedbackEXT zero
                                       zero

-- ** VkPipelineCreationFeedbackFlagBitsEXT

-- | VkPipelineCreationFeedbackFlagBitsEXT - Bitmask specifying pipeline or
-- pipeline stage creation feedback
--
-- = See Also
--
-- 'VkPipelineCreationFeedbackCreateInfoEXT',
-- 'VkPipelineCreationFeedbackEXT'
newtype VkPipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineCreationFeedbackFlagBitsEXT where
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
  showsPrec p (VkPipelineCreationFeedbackFlagBitsEXT x) = showParen (p >= 11) (showString "VkPipelineCreationFeedbackFlagBitsEXT " . showsPrec 11 x)

instance Read VkPipelineCreationFeedbackFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT",                          pure VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT)
                             , ("VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT", pure VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT)
                             , ("VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT",     pure VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCreationFeedbackFlagBitsEXT")
                        v <- step readPrec
                        pure (VkPipelineCreationFeedbackFlagBitsEXT v)
                        )
                    )

-- | 'VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' indicates that the
-- feedback information is valid.
pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000001

-- | 'VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- indicates that a readily usable pipeline or pipeline stage was found in
-- the @pipelineCache@ specified by the application in the pipeline
-- creation command.
--
-- An implementation /should/ set the
-- 'VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- bit if it was able to avoid the large majority of pipeline or pipeline
-- stage creation work by using the @pipelineCache@ parameter of
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateRayTracingPipelinesNV',
-- or 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines'. When an
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
pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000002

-- | 'VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT'
-- indicates that the base pipeline specified by the @basePipelineHandle@
-- or @basePipelineIndex@ member of the @Vk*PipelineCreateInfo@ structure
-- was used to accelerate the creation of the pipeline.
--
-- An implementation /should/ set the
-- 'VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT' bit
-- if it was able to avoid a significant amount of work by using the base
-- pipeline.
--
-- __Note__
--
-- While \"significant amount of work\" is subjective, implementations are
-- encouraged to provide a meaningful signal to applications using this
-- bit. For example, a 1% reduction in duration may not warrant setting
-- this bit, while a 50% reduction would.
pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000004

-- | VkPipelineCreationFeedbackFlagsEXT - Bitmask of
-- VkPipelineCreationFeedbackFlagBitsEXT
--
-- = Description
--
-- 'VkPipelineCreationFeedbackFlagsEXT' is a bitmask type for providing
-- zero or more 'VkPipelineCreationFeedbackFlagBitsEXT'.
--
-- = See Also
--
-- 'VkPipelineCreationFeedbackEXT', 'VkPipelineCreationFeedbackFlagBitsEXT'
type VkPipelineCreationFeedbackFlagsEXT = VkPipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT = VkStructureType 1000192000
