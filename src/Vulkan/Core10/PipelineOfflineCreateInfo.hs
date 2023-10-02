{-# language CPP #-}
-- No documentation found for Chapter "PipelineOfflineCreateInfo"
module Vulkan.Core10.PipelineOfflineCreateInfo  ( PipelineOfflineCreateInfo(..)
                                                , StructureType(..)
                                                , PipelineMatchControl(..)
                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.PipelineMatchControl (PipelineMatchControl)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_OFFLINE_CREATE_INFO))
import Vulkan.Core10.Enums.PipelineMatchControl (PipelineMatchControl(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- No documentation found for TopLevel "VkPipelineOfflineCreateInfo"
data PipelineOfflineCreateInfo = PipelineOfflineCreateInfo
  { -- No documentation found for Nested "VkPipelineOfflineCreateInfo" "pipelineIdentifier"
    pipelineIdentifier :: ByteString
  , -- No documentation found for Nested "VkPipelineOfflineCreateInfo" "matchControl"
    matchControl :: PipelineMatchControl
  , -- No documentation found for Nested "VkPipelineOfflineCreateInfo" "poolEntrySize"
    poolEntrySize :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineOfflineCreateInfo)
#endif
deriving instance Show PipelineOfflineCreateInfo

instance ToCStruct PipelineOfflineCreateInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineOfflineCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_OFFLINE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineIdentifier)
    poke ((p `plusPtr` 32 :: Ptr PipelineMatchControl)) (matchControl)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (poolEntrySize)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_OFFLINE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 32 :: Ptr PipelineMatchControl)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PipelineOfflineCreateInfo where
  peekCStruct p = do
    pipelineIdentifier <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    matchControl <- peek @PipelineMatchControl ((p `plusPtr` 32 :: Ptr PipelineMatchControl))
    poolEntrySize <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ PipelineOfflineCreateInfo
             pipelineIdentifier matchControl poolEntrySize

instance Storable PipelineOfflineCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineOfflineCreateInfo where
  zero = PipelineOfflineCreateInfo
           mempty
           zero
           zero

