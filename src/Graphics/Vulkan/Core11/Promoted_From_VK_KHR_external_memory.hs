{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory  ( ExternalMemoryImageCreateInfo(..)
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO))
import Graphics.Vulkan.Core10.APIConstants (QUEUE_FAMILY_EXTERNAL)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
import Graphics.Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_EXTERNAL)
-- | VkExternalMemoryImageCreateInfo - Specify that an image may be backed by
-- external memory
--
-- = Members
--
-- Note
--
-- A 'ExternalMemoryImageCreateInfo' structure must be included in the
-- creation parameters for an image that will be bound to memory that is
-- either exported or imported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- | @handleTypes@ /must/ not be @0@
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable)
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
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlags)) (zero)
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


-- | VkExternalMemoryBufferCreateInfo - Specify that a buffer may be backed
-- by external memory
--
-- = Members
--
-- Note
--
-- A 'ExternalMemoryBufferCreateInfo' structure must be included in the
-- creation parameters for a buffer that will be bound to memory that is
-- either exported or imported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- | @handleTypes@ /must/ be a valid combination of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- values
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable)
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


-- | VkExportMemoryAllocateInfo - Specify exportable handle types for a
-- device memory object
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ /must/ be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO'
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- | @handleTypes@ is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying one or more memory handle types the application /can/ export
    -- from the resulting allocation. The application /can/ request multiple
    -- handle types for the same allocation.
    handleTypes :: ExternalMemoryHandleTypeFlags }
  deriving (Typeable)
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

