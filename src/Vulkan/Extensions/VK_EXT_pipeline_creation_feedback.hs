{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedbackEXT(..)
                                                            , PipelineCreationFeedbackCreateInfoEXT(..)
                                                            , PipelineCreationFeedbackFlagBitsEXT( PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
                                                                                                 , PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
                                                                                                 , PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
                                                                                                 , ..
                                                                                                 )
                                                            , PipelineCreationFeedbackFlagsEXT
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT))
-- | VkPipelineCreationFeedbackEXT - Feedback about the creation of a
-- pipeline or pipeline stage
--
-- = Description
--
-- If the 'PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' is not set in @flags@,
-- an implementation /must/ not set any other bits in @flags@, and the
-- values of all other 'PipelineCreationFeedbackEXT' data members are
-- undefined.
--
-- = See Also
--
-- 'PipelineCreationFeedbackCreateInfoEXT',
-- 'PipelineCreationFeedbackFlagBitsEXT',
-- 'PipelineCreationFeedbackFlagsEXT'
data PipelineCreationFeedbackEXT = PipelineCreationFeedbackEXT
  { -- | @flags@ is a bitmask of 'PipelineCreationFeedbackFlagBitsEXT' providing
    -- feedback about the creation of a pipeline or of a pipeline stage.
    flags :: PipelineCreationFeedbackFlagsEXT
  , -- | @duration@ is the duration spent creating a pipeline or pipeline stage
    -- in nanoseconds.
    duration :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreationFeedbackEXT)
#endif
deriving instance Show PipelineCreationFeedbackEXT

instance ToCStruct PipelineCreationFeedbackEXT where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreationFeedbackEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlagsEXT)) (flags)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (duration)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlagsEXT)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct PipelineCreationFeedbackEXT where
  peekCStruct p = do
    flags <- peek @PipelineCreationFeedbackFlagsEXT ((p `plusPtr` 0 :: Ptr PipelineCreationFeedbackFlagsEXT))
    duration <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    pure $ PipelineCreationFeedbackEXT
             flags duration

instance Storable PipelineCreationFeedbackEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreationFeedbackEXT where
  zero = PipelineCreationFeedbackEXT
           zero
           zero


-- | VkPipelineCreationFeedbackCreateInfoEXT - Request for feedback about the
-- creation of a pipeline
--
-- = Description
--
-- An implementation /should/ write pipeline creation feedback to
-- @pPipelineCreationFeedback@ and /may/ write pipeline stage creation
-- feedback to @pPipelineStageCreationFeedbacks@. An implementation /must/
-- set or clear the 'PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' in
-- 'PipelineCreationFeedbackEXT'::@flags@ for @pPipelineCreationFeedback@
-- and every element of @pPipelineStageCreationFeedbacks@.
--
-- Note
--
-- One common scenario for an implementation to skip per-stage feedback is
-- when 'PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- is set in @pPipelineCreationFeedback@.
--
-- When chained to
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV', or
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', the @i@ element of
-- @pPipelineStageCreationFeedbacks@ corresponds to the @i@ element of
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR'::@pStages@,
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'::@pStages@,
-- or 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@. When
-- chained to 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo', the first
-- element of @pPipelineStageCreationFeedbacks@ corresponds to
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'::@stage@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pipelineStageCreationFeedbackCount-02668#
--     When chained to 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'PipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@stageCount@
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pipelineStageCreationFeedbackCount-02669#
--     When chained to 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'PipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal 1
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pipelineStageCreationFeedbackCount-02670#
--     When chained to
--     'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
--     'PipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR'::@stageCount@
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pipelineStageCreationFeedbackCount-02969#
--     When chained to
--     'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
--     'PipelineCreationFeedbackEXT'::@pipelineStageCreationFeedbackCount@
--     /must/ equal
--     'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'::@stageCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pPipelineCreationFeedback-parameter#
--     @pPipelineCreationFeedback@ /must/ be a valid pointer to a
--     'PipelineCreationFeedbackEXT' structure
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pPipelineStageCreationFeedbacks-parameter#
--     @pPipelineStageCreationFeedbacks@ /must/ be a valid pointer to an
--     array of @pipelineStageCreationFeedbackCount@
--     'PipelineCreationFeedbackEXT' structures
--
-- -   #VUID-VkPipelineCreationFeedbackCreateInfoEXT-pipelineStageCreationFeedbackCount-arraylength#
--     @pipelineStageCreationFeedbackCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'PipelineCreationFeedbackEXT',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfoEXT
  { -- | @pPipelineCreationFeedback@ is a pointer to a
    -- 'PipelineCreationFeedbackEXT' structure.
    pipelineCreationFeedback :: Ptr PipelineCreationFeedbackEXT
  , -- | @pipelineStageCreationFeedbackCount@ is the number of elements in
    -- @pPipelineStageCreationFeedbacks@.
    pipelineStageCreationFeedbackCount :: Word32
  , -- | @pPipelineStageCreationFeedbacks@ is a pointer to an array of
    -- @pipelineStageCreationFeedbackCount@ 'PipelineCreationFeedbackEXT'
    -- structures.
    pipelineStageCreationFeedbacks :: Ptr PipelineCreationFeedbackEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreationFeedbackCreateInfoEXT)
#endif
deriving instance Show PipelineCreationFeedbackCreateInfoEXT

instance ToCStruct PipelineCreationFeedbackCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreationFeedbackCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedbackEXT))) (pipelineCreationFeedback)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (pipelineStageCreationFeedbackCount)
    poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedbackEXT))) (pipelineStageCreationFeedbacks)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedbackEXT))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedbackEXT))) (zero)
    f

instance FromCStruct PipelineCreationFeedbackCreateInfoEXT where
  peekCStruct p = do
    pPipelineCreationFeedback <- peek @(Ptr PipelineCreationFeedbackEXT) ((p `plusPtr` 16 :: Ptr (Ptr PipelineCreationFeedbackEXT)))
    pipelineStageCreationFeedbackCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pPipelineStageCreationFeedbacks <- peek @(Ptr PipelineCreationFeedbackEXT) ((p `plusPtr` 32 :: Ptr (Ptr PipelineCreationFeedbackEXT)))
    pure $ PipelineCreationFeedbackCreateInfoEXT
             pPipelineCreationFeedback pipelineStageCreationFeedbackCount pPipelineStageCreationFeedbacks

instance Storable PipelineCreationFeedbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreationFeedbackCreateInfoEXT where
  zero = PipelineCreationFeedbackCreateInfoEXT
           zero
           zero
           zero


-- | VkPipelineCreationFeedbackFlagBitsEXT - Bitmask specifying pipeline or
-- pipeline stage creation feedback
--
-- = See Also
--
-- 'PipelineCreationFeedbackCreateInfoEXT', 'PipelineCreationFeedbackEXT',
-- 'PipelineCreationFeedbackFlagsEXT'
newtype PipelineCreationFeedbackFlagBitsEXT = PipelineCreationFeedbackFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT' indicates that the feedback
-- information is valid.
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = PipelineCreationFeedbackFlagBitsEXT 0x00000001
-- | 'PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT'
-- indicates that a readily usable pipeline or pipeline stage was found in
-- the @pipelineCache@ specified by the application in the pipeline
-- creation command.
--
-- An implementation /should/ set the
-- 'PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT' bit
-- if it was able to avoid the large majority of pipeline or pipeline stage
-- creation work by using the @pipelineCache@ parameter of
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV', or
-- 'Vulkan.Core10.Pipeline.createComputePipelines'. When an implementation
-- sets this bit for the entire pipeline, it /may/ leave it unset for any
-- stage.
--
-- Note
--
-- Implementations are encouraged to provide a meaningful signal to
-- applications using this bit. The intention is to communicate to the
-- application that the pipeline or pipeline stage was created \"as fast as
-- it gets\" using the pipeline cache provided by the application. If an
-- implementation uses an internal cache, it is discouraged from setting
-- this bit as the feedback would be unactionable.
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = PipelineCreationFeedbackFlagBitsEXT 0x00000002
-- | 'PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT'
-- indicates that the base pipeline specified by the @basePipelineHandle@
-- or @basePipelineIndex@ member of the @Vk*PipelineCreateInfo@ structure
-- was used to accelerate the creation of the pipeline.
--
-- An implementation /should/ set the
-- 'PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT' bit if
-- it was able to avoid a significant amount of work by using the base
-- pipeline.
--
-- Note
--
-- While \"significant amount of work\" is subjective, implementations are
-- encouraged to provide a meaningful signal to applications using this
-- bit. For example, a 1% reduction in duration may not warrant setting
-- this bit, while a 50% reduction would.
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = PipelineCreationFeedbackFlagBitsEXT 0x00000004

type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT

instance Show PipelineCreationFeedbackFlagBitsEXT where
  showsPrec p = \case
    PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT -> showString "PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
    PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT -> showString "PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
    PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT -> showString "PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
    PipelineCreationFeedbackFlagBitsEXT x -> showParen (p >= 11) (showString "PipelineCreationFeedbackFlagBitsEXT 0x" . showHex x)

instance Read PipelineCreationFeedbackFlagBitsEXT where
  readPrec = parens (choose [("PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT", pure PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT)
                            , ("PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT", pure PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT)
                            , ("PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT", pure PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCreationFeedbackFlagBitsEXT")
                       v <- step readPrec
                       pure (PipelineCreationFeedbackFlagBitsEXT v)))


type EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1


type EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

