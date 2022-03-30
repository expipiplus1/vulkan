{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_pipeline_creation_feedback"
module Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedback(..)
                                                                      , PipelineCreationFeedbackCreateInfo(..)
                                                                      , StructureType(..)
                                                                      , PipelineCreationFeedbackFlagBits(..)
                                                                      , PipelineCreationFeedbackFlags
                                                                      ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits (PipelineCreationFeedbackFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO))
import Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits (PipelineCreationFeedbackFlagBits(..))
import Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits (PipelineCreationFeedbackFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPipelineCreationFeedback - Feedback about the creation of a pipeline
-- or pipeline stage
--
-- = Description
--
-- If the
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_VALID_BIT'
-- is not set in @flags@, an implementation /must/ not set any other bits
-- in @flags@, and the values of all other 'PipelineCreationFeedback' data
-- members are undefined.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_feedback VK_EXT_pipeline_creation_feedback>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'PipelineCreationFeedbackCreateInfo',
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PipelineCreationFeedbackFlagBits',
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PipelineCreationFeedbackFlags'
data PipelineCreationFeedback = PipelineCreationFeedback
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PipelineCreationFeedbackFlagBits'
    -- providing feedback about the creation of a pipeline or of a pipeline
    -- stage.
    flags :: PipelineCreationFeedbackFlags
  , -- | @duration@ is the duration spent creating a pipeline or pipeline stage
    -- in nanoseconds.
    duration :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreationFeedback)
#endif
deriving instance Show PipelineCreationFeedback

instance ToCStruct PipelineCreationFeedback where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreationFeedback{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlags)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (duration)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct PipelineCreationFeedback where
  peekCStruct p = do
    flags <- peek @PipelineCreationFeedbackFlags ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlags))
    duration <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    pure $ PipelineCreationFeedback
             flags duration

instance Storable PipelineCreationFeedback where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreationFeedback where
  zero = PipelineCreationFeedback
           zero
           zero


-- | VkPipelineCreationFeedbackCreateInfo - Request for feedback about the
-- creation of a pipeline
--
-- = Description
--
-- An implementation /should/ write pipeline creation feedback to
-- @pPipelineCreationFeedback@ and /may/ write pipeline stage creation
-- feedback to @pPipelineStageCreationFeedbacks@. An implementation /must/
-- set or clear the
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_VALID_BIT'
-- in 'PipelineCreationFeedback'::@flags@ for @pPipelineCreationFeedback@
-- and every element of @pPipelineStageCreationFeedbacks@.
--
-- Note
--
-- One common scenario for an implementation to skip per-stage feedback is
-- when
-- 'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT'
-- is set in @pPipelineCreationFeedback@.
--
-- When chained to
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV', or
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', the @i@ element of
-- @pPipelineStageCreationFeedbacks@ corresponds to the @i@ element of
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR'::@pStages@,
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'::@pStages@,
-- or 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@. When
-- chained to 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo', the first
-- element of @pPipelineStageCreationFeedbacks@ corresponds to
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'::@stage@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO'
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfo-pPipelineCreationFeedback-parameter#
--     @pPipelineCreationFeedback@ /must/ be a valid pointer to a
--     'PipelineCreationFeedback' structure
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfo-pPipelineStageCreationFeedbacks-parameter#
--     If @pipelineStageCreationFeedbackCount@ is not @0@,
--     @pPipelineStageCreationFeedbacks@ /must/ be a valid pointer to an
--     array of @pipelineStageCreationFeedbackCount@
--     'PipelineCreationFeedback' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_feedback VK_EXT_pipeline_creation_feedback>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'PipelineCreationFeedback',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCreationFeedbackCreateInfo = PipelineCreationFeedbackCreateInfo
  { -- | @pPipelineCreationFeedback@ is a pointer to a 'PipelineCreationFeedback'
    -- structure.
    pipelineCreationFeedback :: Ptr PipelineCreationFeedback
  , -- | @pipelineStageCreationFeedbackCount@ is the number of elements in
    -- @pPipelineStageCreationFeedbacks@.
    pipelineStageCreationFeedbackCount :: Word32
  , -- | @pPipelineStageCreationFeedbacks@ is a pointer to an array of
    -- @pipelineStageCreationFeedbackCount@ 'PipelineCreationFeedback'
    -- structures.
    pipelineStageCreationFeedbacks :: Ptr PipelineCreationFeedback
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreationFeedbackCreateInfo)
#endif
deriving instance Show PipelineCreationFeedbackCreateInfo

instance ToCStruct PipelineCreationFeedbackCreateInfo where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreationFeedbackCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedback))) (pipelineCreationFeedback)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (pipelineStageCreationFeedbackCount)
    poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedback))) (pipelineStageCreationFeedbacks)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedback))) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedback))) (zero)
    f

instance FromCStruct PipelineCreationFeedbackCreateInfo where
  peekCStruct p = do
    pPipelineCreationFeedback <- peek @(Ptr PipelineCreationFeedback) ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedback)))
    pipelineStageCreationFeedbackCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPipelineStageCreationFeedbacks <- peek @(Ptr PipelineCreationFeedback) ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedback)))
    pure $ PipelineCreationFeedbackCreateInfo
             pPipelineCreationFeedback pipelineStageCreationFeedbackCount pPipelineStageCreationFeedbacks

instance Storable PipelineCreationFeedbackCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreationFeedbackCreateInfo where
  zero = PipelineCreationFeedbackCreateInfo
           zero
           zero
           zero

