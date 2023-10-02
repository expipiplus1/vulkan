{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheHeaderTheseTypesArePartOfThe"
module Vulkan.Core10.PipelineCacheHeaderTheseTypesArePartOfThe  ( PipelineCacheStageValidationIndexEntry(..)
                                                                , PipelineCacheSafetyCriticalIndexEntry(..)
                                                                , PipelineCacheHeaderVersionSafetyCriticalOne(..)
                                                                , PipelineCacheHeaderVersion(..)
                                                                , PipelineCacheValidationVersion(..)
                                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.OtherTypes (PipelineCacheHeaderVersionOne)
import Vulkan.Core10.Enums.PipelineCacheValidationVersion (PipelineCacheValidationVersion)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion(..))
import Vulkan.Core10.Enums.PipelineCacheValidationVersion (PipelineCacheValidationVersion(..))
-- No documentation found for TopLevel "VkPipelineCacheStageValidationIndexEntry"
data PipelineCacheStageValidationIndexEntry = PipelineCacheStageValidationIndexEntry
  { -- No documentation found for Nested "VkPipelineCacheStageValidationIndexEntry" "codeSize"
    codeSize :: Word64
  , -- No documentation found for Nested "VkPipelineCacheStageValidationIndexEntry" "codeOffset"
    codeOffset :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheStageValidationIndexEntry)
#endif
deriving instance Show PipelineCacheStageValidationIndexEntry

instance ToCStruct PipelineCacheStageValidationIndexEntry where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheStageValidationIndexEntry{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (codeSize)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (codeOffset)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct PipelineCacheStageValidationIndexEntry where
  peekCStruct p = do
    codeSize <- peek @Word64 ((p `plusPtr` 0 :: Ptr Word64))
    codeOffset <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    pure $ PipelineCacheStageValidationIndexEntry
             codeSize codeOffset

instance Storable PipelineCacheStageValidationIndexEntry where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheStageValidationIndexEntry where
  zero = PipelineCacheStageValidationIndexEntry
           zero
           zero


-- No documentation found for TopLevel "VkPipelineCacheSafetyCriticalIndexEntry"
data PipelineCacheSafetyCriticalIndexEntry = PipelineCacheSafetyCriticalIndexEntry
  { -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "pipelineIdentifier"
    pipelineIdentifier :: ByteString
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "pipelineMemorySize"
    pipelineMemorySize :: Word64
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "jsonSize"
    jsonSize :: Word64
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "jsonOffset"
    jsonOffset :: Word64
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "stageIndexCount"
    stageIndexCount :: Word32
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "stageIndexStride"
    stageIndexStride :: Word32
  , -- No documentation found for Nested "VkPipelineCacheSafetyCriticalIndexEntry" "stageIndexOffset"
    stageIndexOffset :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheSafetyCriticalIndexEntry)
#endif
deriving instance Show PipelineCacheSafetyCriticalIndexEntry

instance ToCStruct PipelineCacheSafetyCriticalIndexEntry where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheSafetyCriticalIndexEntry{..} f = do
    pokeFixedLengthByteString ((p `plusPtr` 0 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineIdentifier)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (pipelineMemorySize)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (jsonSize)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (jsonOffset)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (stageIndexCount)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (stageIndexStride)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (stageIndexOffset)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    pokeFixedLengthByteString ((p `plusPtr` 0 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    f

instance FromCStruct PipelineCacheSafetyCriticalIndexEntry where
  peekCStruct p = do
    pipelineIdentifier <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 0 :: Ptr (FixedArray UUID_SIZE Word8)))
    pipelineMemorySize <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    jsonSize <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    jsonOffset <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    stageIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    stageIndexStride <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    stageIndexOffset <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    pure $ PipelineCacheSafetyCriticalIndexEntry
             pipelineIdentifier
             pipelineMemorySize
             jsonSize
             jsonOffset
             stageIndexCount
             stageIndexStride
             stageIndexOffset

instance Storable PipelineCacheSafetyCriticalIndexEntry where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheSafetyCriticalIndexEntry where
  zero = PipelineCacheSafetyCriticalIndexEntry
           mempty
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkPipelineCacheHeaderVersionSafetyCriticalOne"
data PipelineCacheHeaderVersionSafetyCriticalOne = PipelineCacheHeaderVersionSafetyCriticalOne
  { -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "headerVersionOne"
    headerVersionOne :: PipelineCacheHeaderVersionOne
  , -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "validationVersion"
    validationVersion :: PipelineCacheValidationVersion
  , -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "implementationData"
    implementationData :: Word32
  , -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "pipelineIndexCount"
    pipelineIndexCount :: Word32
  , -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "pipelineIndexStride"
    pipelineIndexStride :: Word32
  , -- No documentation found for Nested "VkPipelineCacheHeaderVersionSafetyCriticalOne" "pipelineIndexOffset"
    pipelineIndexOffset :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheHeaderVersionSafetyCriticalOne)
#endif
deriving instance Show PipelineCacheHeaderVersionSafetyCriticalOne

instance ToCStruct PipelineCacheHeaderVersionSafetyCriticalOne where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheHeaderVersionSafetyCriticalOne{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCacheHeaderVersionOne)) (headerVersionOne)
    poke ((p `plusPtr` 32 :: Ptr PipelineCacheValidationVersion)) (validationVersion)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (implementationData)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (pipelineIndexCount)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (pipelineIndexStride)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (pipelineIndexOffset)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PipelineCacheHeaderVersionOne)) (zero)
    poke ((p `plusPtr` 32 :: Ptr PipelineCacheValidationVersion)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    f

instance FromCStruct PipelineCacheHeaderVersionSafetyCriticalOne where
  peekCStruct p = do
    headerVersionOne <- peekCStruct @PipelineCacheHeaderVersionOne ((p `plusPtr` 0 :: Ptr PipelineCacheHeaderVersionOne))
    validationVersion <- peek @PipelineCacheValidationVersion ((p `plusPtr` 32 :: Ptr PipelineCacheValidationVersion))
    implementationData <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    pipelineIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pipelineIndexStride <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pipelineIndexOffset <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    pure $ PipelineCacheHeaderVersionSafetyCriticalOne
             headerVersionOne
             validationVersion
             implementationData
             pipelineIndexCount
             pipelineIndexStride
             pipelineIndexOffset

instance Storable PipelineCacheHeaderVersionSafetyCriticalOne where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheHeaderVersionSafetyCriticalOne where
  zero = PipelineCacheHeaderVersionSafetyCriticalOne
           zero
           zero
           zero
           zero
           zero
           zero

