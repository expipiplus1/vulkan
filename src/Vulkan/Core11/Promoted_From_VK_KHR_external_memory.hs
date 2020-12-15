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
import Data.Kind (Type)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO))
import Vulkan.Core10.APIConstants (QUEUE_FAMILY_EXTERNAL)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_EXTERNAL)
-- | VkExternalMemoryImageCreateInfo - Specify that an image may be backed by
-- external memory
--
-- = Members
--
-- Note
--
-- A 'ExternalMemoryImageCreateInfo' structure with a non-zero @handleType@
-- field must be included in the creation parameters for an image that will
-- be bound to memory that is either exported or imported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryImageCreateInfo = ExternalMemoryImageCreateInfo
  { -- | @handleTypes@ is zero, or a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying one or more external memory handle types.
    --
    -- #VUID-VkExternalMemoryImageCreateInfo-handleTypes-parameter#
    -- @handleTypes@ /must/ be a valid combination of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- values
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


-- | VkExternalMemoryBufferCreateInfo - Specify that a buffer may be backed
-- by external memory
--
-- = Members
--
-- Note
--
-- A 'ExternalMemoryBufferCreateInfo' structure with a non-zero
-- @handleTypes@ field must be included in the creation parameters for a
-- buffer that will be bound to memory that is either exported or imported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryBufferCreateInfo = ExternalMemoryBufferCreateInfo
  { -- | @handleTypes@ is zero, or a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying one or more external memory handle types.
    --
    -- #VUID-VkExternalMemoryBufferCreateInfo-handleTypes-parameter#
    -- @handleTypes@ /must/ be a valid combination of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- values
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


-- | VkExportMemoryAllocateInfo - Specify exportable handle types for a
-- device memory object
--
-- == Valid Usage
--
-- -   #VUID-VkExportMemoryAllocateInfo-handleTypes-00656# The bits in
--     @handleTypes@ /must/ be supported and compatible, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMemoryAllocateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO'
--
-- -   #VUID-VkExportMemoryAllocateInfo-handleTypes-parameter#
--     @handleTypes@ /must/ be a valid combination of
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryAllocateInfo = ExportMemoryAllocateInfo
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- specifying one or more memory handle types the application /can/ export
    -- from the resulting allocation. The application /can/ request multiple
    -- handle types for the same allocation.
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

