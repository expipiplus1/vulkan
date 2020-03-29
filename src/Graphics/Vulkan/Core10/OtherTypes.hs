{-# language CPP #-}
module Graphics.Vulkan.Core10.OtherTypes  ( MemoryBarrier(..)
                                          , BufferMemoryBarrier(..)
                                          , ImageMemoryBarrier(..)
                                          , DrawIndirectCommand(..)
                                          , DrawIndexedIndirectCommand(..)
                                          , DispatchIndirectCommand(..)
                                          , BaseOutStructure(..)
                                          , BaseInStructure(..)
                                          , ObjectType(..)
                                          , VendorId(..)
                                          ) where

import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Graphics.Vulkan.Core10.SharedTypes (ImageSubresourceRange)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER))
import Graphics.Vulkan.CStruct.Extends (BaseInStructure(..))
import Graphics.Vulkan.CStruct.Extends (BaseOutStructure(..))
import Graphics.Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Graphics.Vulkan.Core10.Enums.VendorId (VendorId(..))
-- | VkMemoryBarrier - Structure specifying a global memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data MemoryBarrier = MemoryBarrier
  { -- | @srcAccessMask@ /must/ be a valid combination of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ /must/ be a valid combination of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
    dstAccessMask :: AccessFlags
  }
  deriving (Typeable)
deriving instance Show MemoryBarrier

instance ToCStruct MemoryBarrier where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryBarrier{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryBarrier where
  peekCStruct p = do
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    pure $ MemoryBarrier
             srcAccessMask dstAccessMask

instance Storable MemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryBarrier where
  zero = MemoryBarrier
           zero
           zero


-- | VkBufferMemoryBarrier - Structure specifying a buffer memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT',
-- memory writes performed by that access type are also made visible, as
-- that access type is not performed through a resource.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT' or
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT',
-- available memory writes are also made visible to accesses of those
-- types, as those access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second access scope includes no
-- access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified buffer range, and the first access scope includes no
-- access, as if @srcAccessMask@ was @0@.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be
--     greater than @0@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be
--     less than or equal to than the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer' minus @offset@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     at least one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@
--     /must/ be 'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     and one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', the
--     other /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED' or a
--     special queue family reserved for external memory ownership
--     transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @srcQueueFamilyIndex@ is
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED',
--     @dstQueueFamilyIndex@ /must/ also be
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @srcQueueFamilyIndex@ is not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', it
--     /must/ be a valid queue family or a special queue family reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @dstQueueFamilyIndex@ is not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', it
--     /must/ be a valid queue family or a special queue family reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with a
--     sharing mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE',
--     and @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', at least
--     one of them /must/ be the same as the family of the queue that will
--     execute this barrier
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- | @srcAccessMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying
    -- a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying
    -- a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | 'Graphics.Vulkan.Core10.Handles.Buffer' is a handle to the buffer whose
    -- backing memory is affected by the barrier.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes into the backing memory for
    -- 'Graphics.Vulkan.Core10.Handles.Buffer'; this is relative to the base
    -- offset as bound to the buffer (see
    -- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory').
    offset :: DeviceSize
  , -- | @size@ is a size in bytes of the affected area of backing memory for
    -- 'Graphics.Vulkan.Core10.Handles.Buffer', or
    -- 'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE' to use the range from
    -- @offset@ to the end of the buffer.
    size :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show BufferMemoryBarrier

instance ToCStruct BufferMemoryBarrier where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferMemoryBarrier{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (srcQueueFamilyIndex)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (dstQueueFamilyIndex)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferMemoryBarrier where
  peekCStruct p = do
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    buffer <- peek @Buffer ((p `plusPtr` 32 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    pure $ BufferMemoryBarrier
             srcAccessMask dstAccessMask srcQueueFamilyIndex dstQueueFamilyIndex buffer offset size

instance Storable BufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferMemoryBarrier where
  zero = BufferMemoryBarrier
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageMemoryBarrier - Structure specifying the parameters of an image
-- memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT',
-- memory writes performed by that access type are also made visible, as
-- that access type is not performed through a resource.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT' or
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT',
-- available memory writes are also made visible to accesses of those
-- types, as those access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second access scope
-- includes no access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first access scope
-- includes no access, as if @srcAccessMask@ was @0@.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range.
--
-- Layout transitions that are performed via image memory barriers execute
-- in their entirety in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- relative to other image layout transitions submitted to the same queue,
-- including those performed by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass render passes>.
-- In effect there is an implicit execution dependency from each such
-- layout transition to all layout transitions previously submitted to the
-- same queue.
--
-- The image layout of each image subresource of a depth\/stencil image
-- created with
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
-- is dependent on the last sample locations used to render to the image
-- subresource as a depth\/stencil attachment, thus when the
-- 'Graphics.Vulkan.Core10.Handles.Image' member of a 'ImageMemoryBarrier'
-- is an image created with this flag the application /can/ include a
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
-- structure in the @pNext@ chain of 'ImageMemoryBarrier' to specify the
-- sample locations to use during the image layout transition.
--
-- If the
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
-- structure included in the @pNext@ chain of 'ImageMemoryBarrier' does not
-- match the sample location state last used to render to the image
-- subresource range specified by @subresourceRange@ or if no
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
-- structure is included in the @pNext@ chain of 'ImageMemoryBarrier', then
-- the contents of the given image subresource range becomes undefined as
-- if @oldLayout@ would equal
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'.
--
-- If 'Graphics.Vulkan.Core10.Handles.Image' has a multi-planar format and
-- the image is /disjoint/, then including
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
-- in the @aspectMask@ member of @subresourceRange@ is equivalent to
-- including
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- and (for three-plane formats only)
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   @oldLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     the current layout of the image subresources affected by the barrier
--
-- -   @newLayout@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     at least one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@
--     /must/ be 'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     and one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', the
--     other /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED' or a
--     special queue family reserved for external memory transfers, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @srcQueueFamilyIndex@ is
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED',
--     @dstQueueFamilyIndex@ /must/ also be
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @srcQueueFamilyIndex@ is not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', it
--     /must/ be a valid queue family or a special queue family reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE'
--     and @dstQueueFamilyIndex@ is not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', it
--     /must/ be a valid queue family or a special queue family reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with a sharing
--     mode of
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE',
--     and @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED', at least
--     one of them /must/ be the same as the family of the queue that will
--     execute this barrier
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If @subresourceRange.levelCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If @subresourceRange.layerCount@ is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has a depth\/stencil
--     format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     and
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has a depth\/stencil
--     format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     and
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has a single-plane color
--     format or is not /disjoint/, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has a multi-planar format
--     and the image is /disjoint/, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either at least one of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT';
--     or /must/ include
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' has a multi-planar format
--     with only two planes, then the @aspectMask@ member of
--     @subresourceRange@ /must/ not include
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     set
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--     then 'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created
--     with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @oldLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @newLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Image' handle
--
-- -   @subresourceRange@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange' structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data ImageMemoryBarrier (es :: [Type]) = ImageMemoryBarrier
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @srcAccessMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying
    -- a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying
    -- a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @oldLayout@ is the old layout in an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | 'Graphics.Vulkan.Core10.Handles.Image' is a handle to the image affected
    -- by this barrier.
    image :: Image
  , -- | @subresourceRange@ describes the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views image subresource range>
    -- within 'Graphics.Vulkan.Core10.Handles.Image' that is affected by this
    -- barrier.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance Extensible ImageMemoryBarrier where
  extensibleType = STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  setNext x next = x{next = next}
  getNext ImageMemoryBarrier{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryBarrier e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SampleLocationsInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (ImageMemoryBarrier es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageMemoryBarrier{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (oldLayout)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (newLayout)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (srcQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (dstQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (image)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (subresourceRange) . ($ ())
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr AccessFlags)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr AccessFlags)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Image)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (zero) . ($ ())
    lift $ f

instance PeekChain es => FromCStruct (ImageMemoryBarrier es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcAccessMask <- peek @AccessFlags ((p `plusPtr` 16 :: Ptr AccessFlags))
    dstAccessMask <- peek @AccessFlags ((p `plusPtr` 20 :: Ptr AccessFlags))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 28 :: Ptr ImageLayout))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    image <- peek @Image ((p `plusPtr` 40 :: Ptr Image))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 48 :: Ptr ImageSubresourceRange))
    pure $ ImageMemoryBarrier
             next srcAccessMask dstAccessMask oldLayout newLayout srcQueueFamilyIndex dstQueueFamilyIndex image subresourceRange

instance es ~ '[] => Zero (ImageMemoryBarrier es) where
  zero = ImageMemoryBarrier
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDrawIndirectCommand - Structure specifying a draw indirect command
--
-- = Description
--
-- The members of 'DrawIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDraw'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'
data DrawIndirectCommand = DrawIndirectCommand
  { -- | @vertexCount@ is the number of vertices to draw.
    vertexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
    instanceCount :: Word32
  , -- | @firstVertex@ is the index of the first vertex to draw.
    firstVertex :: Word32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
    firstInstance :: Word32
  }
  deriving (Typeable)
deriving instance Show DrawIndirectCommand

instance ToCStruct DrawIndirectCommand where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndirectCommand where
  peekCStruct p = do
    vertexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    firstInstance <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ DrawIndirectCommand
             vertexCount instanceCount firstVertex firstInstance

instance Storable DrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand
           zero
           zero
           zero
           zero


-- | VkDrawIndexedIndirectCommand - Structure specifying a draw indexed
-- indirect command
--
-- = Description
--
-- The members of 'DrawIndexedIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input>
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType', where the index
--     buffer, 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType', and
--     @offset@ are specified via
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- | @indexCount@ is the number of vertices to draw.
    indexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
    instanceCount :: Word32
  , -- | @firstIndex@ is the base index within the index buffer.
    firstIndex :: Word32
  , -- | @vertexOffset@ is the value added to the vertex index before indexing
    -- into the vertex buffer.
    vertexOffset :: Int32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
    firstInstance :: Word32
  }
  deriving (Typeable)
deriving instance Show DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand where
  withCStruct x f = allocaBytesAligned 20 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndexedIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstIndex)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (vertexOffset)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndexedIndirectCommand where
  peekCStruct p = do
    indexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstIndex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    vertexOffset <- peek @Int32 ((p `plusPtr` 12 :: Ptr Int32))
    firstInstance <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DrawIndexedIndirectCommand
             indexCount instanceCount firstIndex vertexOffset firstInstance

instance Storable DrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand
           zero
           zero
           zero
           zero
           zero


-- | VkDispatchIndirectCommand - Structure specifying a dispatch indirect
-- command
--
-- = Description
--
-- The members of 'DispatchIndirectCommand' have the same meaning as the
-- corresponding parameters of
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatch'.
--
-- == Valid Usage
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- | @x@ /must/ be less than or equal to
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
    x :: Word32
  , -- | @y@ /must/ be less than or equal to
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
    y :: Word32
  , -- | @z@ /must/ be less than or equal to
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
    z :: Word32
  }
  deriving (Typeable)
deriving instance Show DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DispatchIndirectCommand where
  peekCStruct p = do
    x <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    y <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    z <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DispatchIndirectCommand
             x y z

instance Storable DispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand
           zero
           zero
           zero

