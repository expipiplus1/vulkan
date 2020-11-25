{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_memory"
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory  ( ExternalMemoryImageCreateInfo(..)
                                                           , ExternalMemoryBufferCreateInfo(..)
                                                           , ExportMemoryAllocateInfo(..)
                                                           , StructureType(..)
                                                           , Result(..)
                                                           , QUEUE_FAMILY_EXTERNAL
                                                           , pattern QUEUE_FAMILY_EXTERNAL
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
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO))
import Vulkan.Core10.APIConstants (QUEUE_FAMILY_EXTERNAL)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_EXTERNAL)

-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfo"
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "handleTypes"
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryImageCreateInfo)
#endif
deriving instance Show ExternalMemoryImageCreateInfo

instance ToCStruct ExternalMemoryImageCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryImageCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExternalMemoryImageCreateInfo where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExternalMemoryImageCreateInfo
             handleTypes


instance Storable ExternalMemoryImageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryImageCreateInfo where
  zero = ExternalMemoryImageCreateInfo
           zero



-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfo"
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "handleTypes"
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryBufferCreateInfo)
#endif
deriving instance Show ExternalMemoryBufferCreateInfo

instance ToCStruct ExternalMemoryBufferCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryBufferCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExternalMemoryBufferCreateInfo where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExternalMemoryBufferCreateInfo
             handleTypes


instance Storable ExternalMemoryBufferCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryBufferCreateInfo where
  zero = ExternalMemoryBufferCreateInfo
           zero



-- No documentation found for TopLevel "VkExportMemoryAllocateInfo"
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- No documentation found for Nested "VkExportMemoryAllocateInfo" "handleTypes"
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryAllocateInfo)
#endif
deriving instance Show ExportMemoryAllocateInfo

instance ToCStruct ExportMemoryAllocateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryAllocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMemoryAllocateInfo where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExportMemoryAllocateInfo
             handleTypes


instance Storable ExportMemoryAllocateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryAllocateInfo where
  zero = ExportMemoryAllocateInfo
           zero

