{-# language CPP #-}
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
-- | VkDedicatedAllocationImageCreateInfoNV - Specify that an image is bound
-- to a dedicated memory resource
--
-- = Description
--
-- Note
--
-- Using a dedicated allocation for color and depth\/stencil attachments or
-- other large images /may/ improve performance on some devices.
--
-- == Valid Usage
--
-- -   #VUID-VkDedicatedAllocationImageCreateInfoNV-dedicatedAllocation-00994#
--     If @dedicatedAllocation@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ /must/ not include
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDedicatedAllocationImageCreateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- | @dedicatedAllocation@ specifies whether the image will have a dedicated
    -- allocation bound to it.
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


-- | VkDedicatedAllocationBufferCreateInfoNV - Specify that a buffer is bound
-- to a dedicated memory resource
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDedicatedAllocationBufferCreateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- | @dedicatedAllocation@ specifies whether the buffer will have a dedicated
    -- allocation bound to it.
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


-- | VkDedicatedAllocationMemoryAllocateInfoNV - Specify a dedicated memory
-- allocation resource
--
-- == Valid Usage
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-image-00649# At
--     least one of @image@ and @buffer@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-image-00650# If
--     @image@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the image
--     /must/ have been created with
--     'DedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@ equal
--     to 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-buffer-00651# If
--     @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the buffer
--     /must/ have been created with
--     'DedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@ equal
--     to 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-image-00652# If
--     @image@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     equal the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ of the
--     image
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-buffer-00653# If
--     @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     equal the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ of the
--     buffer
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-image-00654# If
--     @image@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation, the memory being imported /must/ also be a dedicated
--     image allocation and @image@ /must/ be identical to the image
--     associated with the imported memory
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-buffer-00655# If
--     @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation, the memory being imported /must/ also be a dedicated
--     buffer allocation and @buffer@ /must/ be identical to the buffer
--     associated with the imported memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV'
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-image-parameter# If
--     @image@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-buffer-parameter# If
--     @buffer@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkDedicatedAllocationMemoryAllocateInfoNV-commonparent# Both
--     of @buffer@, and @image@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- | @image@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of an
    -- image which this memory will be bound to.
    image :: Image
  , -- | @buffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of a
    -- buffer which this memory will be bound to.
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

