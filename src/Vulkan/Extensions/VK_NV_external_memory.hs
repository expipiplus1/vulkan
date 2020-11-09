{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory  ( ExternalMemoryImageCreateInfoNV(..)
                                                , ExportMemoryAllocateInfoNV(..)
                                                , NV_EXTERNAL_MEMORY_SPEC_VERSION
                                                , pattern NV_EXTERNAL_MEMORY_SPEC_VERSION
                                                , NV_EXTERNAL_MEMORY_EXTENSION_NAME
                                                , pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME
                                                , ExternalMemoryHandleTypeFlagBitsNV(..)
                                                , ExternalMemoryHandleTypeFlagsNV
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
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
-- | VkExternalMemoryImageCreateInfoNV - Specify that an image may be backed
-- by external memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExternalMemoryImageCreateInfoNV-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV'
--
-- -   #VUID-VkExternalMemoryImageCreateInfoNV-handleTypes-parameter#
--     @handleTypes@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryImageCreateInfoNV = ExternalMemoryImageCreateInfoNV
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- specifying one or more external memory handle types.
    handleTypes :: ExternalMemoryHandleTypeFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryImageCreateInfoNV)
#endif
deriving instance Show ExternalMemoryImageCreateInfoNV

instance ToCStruct ExternalMemoryImageCreateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryImageCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExternalMemoryImageCreateInfoNV where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExternalMemoryImageCreateInfoNV
             handleTypes

instance Storable ExternalMemoryImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryImageCreateInfoNV where
  zero = ExternalMemoryImageCreateInfoNV
           zero


-- | VkExportMemoryAllocateInfoNV - Specify memory handle types that may be
-- exported
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMemoryAllocateInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV'
--
-- -   #VUID-VkExportMemoryAllocateInfoNV-handleTypes-parameter#
--     @handleTypes@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryAllocateInfoNV = ExportMemoryAllocateInfoNV
  { -- | @handleTypes@ is a bitmask of
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- specifying one or more memory handle types that /may/ be exported.
    -- Multiple handle types /may/ be requested for the same allocation as long
    -- as they are compatible, as reported by
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV'.
    handleTypes :: ExternalMemoryHandleTypeFlagsNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryAllocateInfoNV)
#endif
deriving instance Show ExportMemoryAllocateInfoNV

instance ToCStruct ExportMemoryAllocateInfoNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryAllocateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleTypes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMemoryAllocateInfoNV where
  peekCStruct p = do
    handleTypes <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    pure $ ExportMemoryAllocateInfoNV
             handleTypes

instance Storable ExportMemoryAllocateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryAllocateInfoNV where
  zero = ExportMemoryAllocateInfoNV
           zero


type NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

