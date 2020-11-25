{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_dedicated_allocation"
module Vulkan.Extensions.VK_NV_dedicated_allocation  ( DedicatedAllocationImageCreateInfoNV(..)
                                                     , DedicatedAllocationBufferCreateInfoNV(..)
                                                     , DedicatedAllocationMemoryAllocateInfoNV(..)
                                                     , NV_DEDICATED_ALLOCATION_SPEC_VERSION
                                                     , pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION
                                                     , NV_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                     , pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV))

-- No documentation found for TopLevel "VkDedicatedAllocationImageCreateInfoNV"
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "dedicatedAllocation"
    dedicatedAllocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DedicatedAllocationImageCreateInfoNV)
#endif
deriving instance Show DedicatedAllocationImageCreateInfoNV

instance ToCStruct DedicatedAllocationImageCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DedicatedAllocationImageCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dedicatedAllocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DedicatedAllocationImageCreateInfoNV where
  peekCStruct p = do
    dedicatedAllocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DedicatedAllocationImageCreateInfoNV
             (bool32ToBool dedicatedAllocation)


instance Storable DedicatedAllocationImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DedicatedAllocationImageCreateInfoNV where
  zero = DedicatedAllocationImageCreateInfoNV
           zero



-- No documentation found for TopLevel "VkDedicatedAllocationBufferCreateInfoNV"
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "dedicatedAllocation"
    dedicatedAllocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DedicatedAllocationBufferCreateInfoNV)
#endif
deriving instance Show DedicatedAllocationBufferCreateInfoNV

instance ToCStruct DedicatedAllocationBufferCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DedicatedAllocationBufferCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dedicatedAllocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DedicatedAllocationBufferCreateInfoNV where
  peekCStruct p = do
    dedicatedAllocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DedicatedAllocationBufferCreateInfoNV
             (bool32ToBool dedicatedAllocation)


instance Storable DedicatedAllocationBufferCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DedicatedAllocationBufferCreateInfoNV where
  zero = DedicatedAllocationBufferCreateInfoNV
           zero



-- No documentation found for TopLevel "VkDedicatedAllocationMemoryAllocateInfoNV"
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "image"
    image :: Image
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "buffer"
    buffer :: Buffer
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DedicatedAllocationMemoryAllocateInfoNV)
#endif
deriving instance Show DedicatedAllocationMemoryAllocateInfoNV

instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DedicatedAllocationMemoryAllocateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DedicatedAllocationMemoryAllocateInfoNV where
  peekCStruct p = do
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    pure $ DedicatedAllocationMemoryAllocateInfoNV
             image buffer


instance Storable DedicatedAllocationMemoryAllocateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DedicatedAllocationMemoryAllocateInfoNV where
  zero = DedicatedAllocationMemoryAllocateInfoNV
           zero
           zero


type NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1


type NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"

