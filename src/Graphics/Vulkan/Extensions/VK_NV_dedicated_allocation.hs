{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation  ( DedicatedAllocationImageCreateInfoNV(..)
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV))
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
-- -   If @dedicatedAllocation@ is 'Graphics.Vulkan.Core10.BaseType.TRUE',
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@flags@ /must/ not
--     include
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- | @dedicatedAllocation@ specifies whether the image will have a dedicated
    -- allocation bound to it.
    dedicatedAllocation :: Bool }
  deriving (Typeable)
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
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- | @dedicatedAllocation@ specifies whether the buffer will have a dedicated
    -- allocation bound to it.
    dedicatedAllocation :: Bool }
  deriving (Typeable)
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
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the image /must/ have been created with
--     'DedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@ equal
--     to 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', the buffer /must/
--     have been created with
--     'DedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@ equal
--     to 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@
--     of the image
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@
--     of the buffer
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--     and 'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated image allocation and @image@ /must/ be identical to the
--     image associated with the imported memory.
--
-- -   If @buffer@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--     and 'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated buffer allocation and @buffer@ /must/ be identical to the
--     buffer associated with the imported memory.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV'
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @image@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   Both of @buffer@, and @image@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- | @image@ is 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle
    -- of an image which this memory will be bound to.
    image :: Image
  , -- | @buffer@ is 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE' or a
    -- handle of a buffer which this memory will be bound to.
    buffer :: Buffer
  }
  deriving (Typeable)
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

