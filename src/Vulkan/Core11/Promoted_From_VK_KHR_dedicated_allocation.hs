{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dedicated_allocation"
module Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation  ( MemoryDedicatedRequirements(..)
                                                                , MemoryDedicatedAllocateInfo(..)
                                                                , StructureType(..)
                                                                ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkMemoryDedicatedRequirements"
data MemoryDedicatedRequirements = MemoryDedicatedRequirements
  { -- No documentation found for Nested "VkMemoryDedicatedRequirements" "prefersDedicatedAllocation"
    prefersDedicatedAllocation :: Bool
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "requiresDedicatedAllocation"
    requiresDedicatedAllocation :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryDedicatedRequirements)
#endif
deriving instance Show MemoryDedicatedRequirements

instance ToCStruct MemoryDedicatedRequirements where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryDedicatedRequirements{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (prefersDedicatedAllocation))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (requiresDedicatedAllocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct MemoryDedicatedRequirements where
  peekCStruct p = do
    prefersDedicatedAllocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    requiresDedicatedAllocation <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ MemoryDedicatedRequirements
             (bool32ToBool prefersDedicatedAllocation) (bool32ToBool requiresDedicatedAllocation)


instance Storable MemoryDedicatedRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryDedicatedRequirements where
  zero = MemoryDedicatedRequirements
           zero
           zero



-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfo"
data MemoryDedicatedAllocateInfo = MemoryDedicatedAllocateInfo
  { -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "image"
    image :: Image
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "buffer"
    buffer :: Buffer
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryDedicatedAllocateInfo)
#endif
deriving instance Show MemoryDedicatedAllocateInfo

instance ToCStruct MemoryDedicatedAllocateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryDedicatedAllocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryDedicatedAllocateInfo where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    pure $ MemoryDedicatedAllocateInfo
             image buffer


instance Storable MemoryDedicatedAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryDedicatedAllocateInfo where
  zero = MemoryDedicatedAllocateInfo
           zero
           zero

