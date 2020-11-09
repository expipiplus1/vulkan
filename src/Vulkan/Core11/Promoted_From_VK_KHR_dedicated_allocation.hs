{-# language CPP #-}
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
-- | VkMemoryDedicatedRequirements - Structure describing dedicated
-- allocation requirements of buffer and image resources
--
-- = Description
--
-- When the implementation sets @requiresDedicatedAllocation@ to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', it /must/ also set
-- @prefersDedicatedAllocation@ to 'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- If the 'MemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'
-- call, @requiresDedicatedAllocation@ /may/ be
-- 'Vulkan.Core10.FundamentalTypes.TRUE' under one of the following
-- conditions:
--
-- -   The @pNext@ chain of 'Vulkan.Core10.Buffer.BufferCreateInfo' for the
--     call to 'Vulkan.Core10.Buffer.createBuffer' used to create the
--     buffer being queried included a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'
--     structure, and any of the handle types specified in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     requires dedicated allocation, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferProperties'
--     in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'::@externalMemoryProperties.externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to
--     'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- 'Vulkan.Core10.FundamentalTypes.FALSE' by the implementation whenever a
-- 'MemoryDedicatedRequirements' structure is included in the @pNext@ chain
-- of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed to a call to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'.
--
-- If the 'MemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'
-- call and
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
-- was set in 'Vulkan.Core10.Buffer.BufferCreateInfo'::@flags@ when
-- @buffer@ was created then the implementation /must/ set both
-- @prefersDedicatedAllocation@ and @requiresDedicatedAllocation@ to
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- If the 'MemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
-- call, @requiresDedicatedAllocation@ /may/ be
-- 'Vulkan.Core10.FundamentalTypes.TRUE' under one of the following
-- conditions:
--
-- -   The @pNext@ chain of 'Vulkan.Core10.Image.ImageCreateInfo' for the
--     call to 'Vulkan.Core10.Image.createImage' used to create the image
--     being queried included a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     structure, and any of the handle types specified in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     requires dedicated allocation, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--     in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'::@externalMemoryProperties.externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to
--     'Vulkan.Core10.FundamentalTypes.TRUE'.
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- 'Vulkan.Core10.FundamentalTypes.FALSE' by the implementation whenever a
-- 'MemoryDedicatedRequirements' structure is included in the @pNext@ chain
-- of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed to a call to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'.
--
-- If the 'MemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
-- call and
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
-- was set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ when @image@
-- was created then the implementation /must/ set both
-- @prefersDedicatedAllocation@ and @requiresDedicatedAllocation@ to
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryDedicatedRequirements-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryDedicatedRequirements = MemoryDedicatedRequirements
  { -- | @prefersDedicatedAllocation@ specifies that the implementation would
    -- prefer a dedicated allocation for this resource. The application is
    -- still free to suballocate the resource but it /may/ get better
    -- performance if a dedicated allocation is used.
    prefersDedicatedAllocation :: Bool
  , -- | @requiresDedicatedAllocation@ specifies that a dedicated allocation is
    -- required for this resource.
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


-- | VkMemoryDedicatedAllocateInfo - Specify a dedicated memory allocation
-- resource
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-01432# At least one of
--     @image@ and @buffer@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-02964# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and the memory is not an
--     imported Android Hardware Buffer,
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     equal the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ of the
--     image
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-01434# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@ /must/ have been
--     created without
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_BINDING_BIT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-buffer-02965# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and the memory is not an
--     imported Android Hardware Buffer,
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     equal the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ of the
--     buffer
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-buffer-01436# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/ have been
--     created without
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--     set in 'Vulkan.Core10.Buffer.BufferCreateInfo'::@flags@
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-01876# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation with handle type
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     and the external handle was created by the Vulkan API, then the
--     memory being imported /must/ also be a dedicated image allocation
--     and @image@ must be identical to the image associated with the
--     imported memory
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-buffer-01877# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation with handle type
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     and the external handle was created by the Vulkan API, then the
--     memory being imported /must/ also be a dedicated buffer allocation
--     and @buffer@ /must/ be identical to the buffer associated with the
--     imported memory
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-01878# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation with handle type
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the memory being imported /must/ also be a dedicated image
--     allocation and @image@ /must/ be identical to the image associated
--     with the imported memory
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-buffer-01879# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' and
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' defines a memory import
--     operation with handle type
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the memory being imported /must/ also be a dedicated buffer
--     allocation and @buffer@ /must/ be identical to the buffer associated
--     with the imported memory
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-01797# If @image@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@ /must/ not have
--     been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO'
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-image-parameter# If @image@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-buffer-parameter# If @buffer@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkMemoryDedicatedAllocateInfo-commonparent# Both of @buffer@,
--     and @image@ that are valid handles of non-ignored parameters /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryDedicatedAllocateInfo = MemoryDedicatedAllocateInfo
  { -- | @image@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of an
    -- image which this memory will be bound to.
    image :: Image
  , -- | @buffer@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a handle of a
    -- buffer which this memory will be bound to.
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

