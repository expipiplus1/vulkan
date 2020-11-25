{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_pipeline_creation_feedback"
module Vulkan.Extensions.VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedbackEXT(..)
                                                            , PipelineCreationFeedbackCreateInfoEXT(..)
                                                            , PipelineCreationFeedbackFlagsEXT
                                                            , PipelineCreationFeedbackFlagBitsEXT( PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
                                                                                                 , PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
                                                                                                 , PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
                                                                                                 , ..
                                                                                                 )
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
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
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
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

-- No documentation found for TopLevel "VkPipelineCreationFeedbackEXT"
data PipelineCreationFeedbackEXT = PipelineCreationFeedbackEXT
  { -- No documentation found for Nested "VkPipelineCreationFeedbackEXT" "flags"
    flags :: PipelineCreationFeedbackFlagsEXT
  , -- No documentation found for Nested "VkPipelineCreationFeedbackEXT" "duration"
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



-- No documentation found for TopLevel "VkPipelineCreationFeedbackCreateInfoEXT"
data PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pPipelineCreationFeedback"
    pipelineCreationFeedback :: Ptr PipelineCreationFeedbackEXT
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pipelineStageCreationFeedbackCount"
    pipelineStageCreationFeedbackCount :: Word32
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pPipelineStageCreationFeedbacks"
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


type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VkPipelineCreationFeedbackFlagBitsEXT"
newtype PipelineCreationFeedbackFlagBitsEXT = PipelineCreationFeedbackFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = PipelineCreationFeedbackFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT =
  PipelineCreationFeedbackFlagBitsEXT 0x00000002
-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
pattern PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = PipelineCreationFeedbackFlagBitsEXT 0x00000004

conNamePipelineCreationFeedbackFlagBitsEXT :: String
conNamePipelineCreationFeedbackFlagBitsEXT = "PipelineCreationFeedbackFlagBitsEXT"

enumPrefixPipelineCreationFeedbackFlagBitsEXT :: String
enumPrefixPipelineCreationFeedbackFlagBitsEXT = "PIPELINE_CREATION_FEEDBACK_"

showTablePipelineCreationFeedbackFlagBitsEXT :: [(PipelineCreationFeedbackFlagBitsEXT, String)]
showTablePipelineCreationFeedbackFlagBitsEXT =
  [ (PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT                         , "VALID_BIT_EXT")
  , (PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT, "APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT")
  , (PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT    , "BASE_PIPELINE_ACCELERATION_BIT_EXT")
  ]


instance Show PipelineCreationFeedbackFlagBitsEXT where
showsPrec = enumShowsPrec enumPrefixPipelineCreationFeedbackFlagBitsEXT
                          showTablePipelineCreationFeedbackFlagBitsEXT
                          conNamePipelineCreationFeedbackFlagBitsEXT
                          (\(PipelineCreationFeedbackFlagBitsEXT x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineCreationFeedbackFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixPipelineCreationFeedbackFlagBitsEXT
                          showTablePipelineCreationFeedbackFlagBitsEXT
                          conNamePipelineCreationFeedbackFlagBitsEXT
                          PipelineCreationFeedbackFlagBitsEXT


type EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1


type EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

