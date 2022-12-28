{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module Vulkan.Core10.OtherTypes  ( MemoryBarrier(..)
                                 , BufferMemoryBarrier(..)
                                 , ImageMemoryBarrier(..)
                                 , PipelineCacheHeaderVersionOne(..)
                                 , DrawIndirectCommand(..)
                                 , DrawIndexedIndirectCommand(..)
                                 , DispatchIndirectCommand(..)
                                 , BaseOutStructure(..)
                                 , BaseInStructure(..)
                                 , ObjectType(..)
                                 , VendorId(..)
                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER))
import Vulkan.CStruct.Extends (BaseInStructure(..))
import Vulkan.CStruct.Extends (BaseOutStructure(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Enums.VendorId (VendorId(..))
-- | VkMemoryBarrier - Structure specifying a global memory barrier
--
-- = Description
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data MemoryBarrier = MemoryBarrier
  { -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    --
    -- #VUID-VkMemoryBarrier-srcAccessMask-parameter# @srcAccessMask@ /must/ be
    -- a valid combination of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    --
    -- #VUID-VkMemoryBarrier-dstAccessMask-parameter# @dstAccessMask@ /must/ be
    -- a valid combination of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' values
    dstAccessMask :: AccessFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryBarrier)
#endif
deriving instance Show MemoryBarrier

instance ToCStruct MemoryBarrier where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT', a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible memory domain operation>
-- is performed where available memory in the host domain is also made
-- available to the device domain.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT' or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT', a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible memory domain operation>
-- is performed where available memory in the device domain is also made
-- available to the host domain.
--
-- Note
--
-- When
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- is used, available memory in host domain is automatically made visible
-- to host domain, and any host write is automatically made available to
-- host domain.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second access scope includes no
-- access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified buffer range, and the first access scope includes no
-- access, as if @srcAccessMask@ was @0@.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferMemoryBarrier-offset-01187# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-VkBufferMemoryBarrier-size-01188# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkBufferMemoryBarrier-size-01189# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less than
--     or equal to than the size of @buffer@ minus @offset@
--
-- -   #VUID-VkBufferMemoryBarrier-buffer-01931# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBufferMemoryBarrier-srcQueueFamilyIndex-04087# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one /must/ not be a special queue family reserved for external
--     memory ownership transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkBufferMemoryBarrier-buffer-04088# If @buffer@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal, and
--     one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is one of the
--     special queue family values reserved for external memory transfers,
--     the other /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   #VUID-VkBufferMemoryBarrier-buffer-04089# If @buffer@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be valid
--     queue families, or one of the special queue family values reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkBufferMemoryBarrier-synchronization2-03853# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, and @buffer@ was created with a sharing mode
--     of 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', at
--     least one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/
--     be 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferMemoryBarrier-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER'
--
-- -   #VUID-VkBufferMemoryBarrier-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkBufferMemoryBarrier-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @buffer@ is a handle to the buffer whose backing memory is affected by
    -- the barrier.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes into the backing memory for @buffer@;
    -- this is relative to the base offset as bound to the buffer (see
    -- 'Vulkan.Core10.MemoryManagement.bindBufferMemory').
    offset :: DeviceSize
  , -- | @size@ is a size in bytes of the affected area of backing memory for
    -- @buffer@, or 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to use the range
    -- from @offset@ to the end of the buffer.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferMemoryBarrier)
#endif
deriving instance Show BufferMemoryBarrier

instance ToCStruct BufferMemoryBarrier where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
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
             srcAccessMask
             dstAccessMask
             srcQueueFamilyIndex
             dstQueueFamilyIndex
             buffer
             offset
             size

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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT', memory
-- writes performed by that access type are also made visible, as that
-- access type is not performed through a resource.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_WRITE_BIT' or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_HOST_READ_BIT', available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second access scope
-- includes no access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first access scope
-- includes no access, as if @srcAccessMask@ was @0@.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
-- feature is not enabled or @oldLayout@ is not equal to @newLayout@,
-- @oldLayout@ and @newLayout@ define an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range.
--
-- Note
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
-- feature is enabled, when the old and new layout are equal, the layout
-- values are ignored - data is preserved no matter what values are
-- specified, or what layout the image is currently in.
--
-- If @image@ has a multi-planar format and the image is /disjoint/, then
-- including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' in the
-- @aspectMask@ member of @subresourceRange@ is equivalent to including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', and
-- (for three-plane formats only)
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageMemoryBarrier-subresourceRange-01486#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkImageMemoryBarrier-subresourceRange-01724# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier-subresourceRange-01488#
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier-subresourceRange-01725# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier-image-01932# If @image@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01208# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01209# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01210# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01211# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01212# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01213# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01197# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @oldLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources affected by the barrier
--
-- -   #VUID-VkImageMemoryBarrier-newLayout-01198# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01658# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-01659# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-04065# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-04066# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-04067# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-04068# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier-synchronization2-06911# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @layout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-03938# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL',
--     @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-03939# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL',
--     @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-02088# If @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     set
--
-- -   #VUID-VkImageMemoryBarrier-image-01671# If @image@ has a
--     single-plane color format or is not /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-image-01672# If @image@ has a
--     multi-planar format and the image is /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ include either at
--     least one of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT';
--     or /must/ include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-image-01673# If @image@ has a
--     multi-planar format with only two planes, then the @aspectMask@
--     member of @subresourceRange@ /must/ not include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-image-03319# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-image-03320# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-04070# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one /must/ not be a special queue family reserved for external
--     memory ownership transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkImageMemoryBarrier-image-04071# If @image@ was created with
--     a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal, and
--     one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is one of the
--     special queue family values reserved for external memory transfers,
--     the other /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   #VUID-VkImageMemoryBarrier-image-04072# If @image@ was created with
--     a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be valid
--     queue families, or one of the special queue family values reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkImageMemoryBarrier-srcQueueFamilyIndex-07006# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then @image@ /must/ have been created with either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage bits, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage bits, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     usage bit
--
-- -   #VUID-VkImageMemoryBarrier-attachmentFeedbackLoopLayout-07313# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   #VUID-VkImageMemoryBarrier-synchronization2-03857# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, and @image@ was created with a sharing mode
--     of 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', at
--     least one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/
--     be 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageMemoryBarrier-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER'
--
-- -   #VUID-VkImageMemoryBarrier-pNext-pNext# @pNext@ /must/ be @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
--
-- -   #VUID-VkImageMemoryBarrier-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageMemoryBarrier-oldLayout-parameter# @oldLayout@ /must/
--     be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkImageMemoryBarrier-newLayout-parameter# @newLayout@ /must/
--     be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkImageMemoryBarrier-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkImageMemoryBarrier-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
data ImageMemoryBarrier (es :: [Type]) = ImageMemoryBarrier
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks source access mask>.
    srcAccessMask :: AccessFlags
  , -- | @dstAccessMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' specifying a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
    dstAccessMask :: AccessFlags
  , -- | @oldLayout@ is the old layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @image@ is a handle to the image affected by this barrier.
    image :: Image
  , -- | @subresourceRange@ describes the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views image subresource range>
    -- within @image@ that is affected by this barrier.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageMemoryBarrier (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageMemoryBarrier es)

instance Extensible ImageMemoryBarrier where
  extensibleTypeName = "ImageMemoryBarrier"
  setNext ImageMemoryBarrier{..} next' = ImageMemoryBarrier{next = next', ..}
  getNext ImageMemoryBarrier{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryBarrier e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SampleLocationsInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss ImageMemoryBarrier es
         , PokeChain es ) => ToCStruct (ImageMemoryBarrier es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
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
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (subresourceRange)
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
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance ( Extendss ImageMemoryBarrier es
         , PeekChain es ) => FromCStruct (ImageMemoryBarrier es) where
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
             next
             srcAccessMask
             dstAccessMask
             oldLayout
             newLayout
             srcQueueFamilyIndex
             dstQueueFamilyIndex
             image
             subresourceRange

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


-- | VkPipelineCacheHeaderVersionOne - Structure describing the layout of the
-- pipeline cache header
--
-- = Description
--
-- Unlike most structures declared by the Vulkan API, all fields of this
-- structure are written with the least significant byte first, regardless
-- of host byte-order.
--
-- The C language specification does not define the packing of structure
-- members. This layout assumes tight structure member packing, with
-- members laid out in the order listed in the structure, and the intended
-- size of the structure is 32 bytes. If a compiler produces code that
-- diverges from that pattern, applications /must/ employ another method to
-- set values at the correct offsets.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
data PipelineCacheHeaderVersionOne = PipelineCacheHeaderVersionOne
  { -- | @headerSize@ is the length in bytes of the pipeline cache header.
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerSize-04967# @headerSize@
    -- /must/ be 32
    headerSize :: Word32
  , -- | @headerVersion@ is a
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
    -- enum value specifying the version of the header. A consumer of the
    -- pipeline cache /should/ use the cache version to interpret the remainder
    -- of the cache header.
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerVersion-04968#
    -- @headerVersion@ /must/ be
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_ONE'
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerVersion-parameter#
    -- @headerVersion@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
    -- value
    headerVersion :: PipelineCacheHeaderVersion
  , -- | @vendorID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@vendorID@
    -- of the implementation.
    vendorID :: Word32
  , -- | @deviceID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@deviceID@
    -- of the implementation.
    deviceID :: Word32
  , -- | @pipelineCacheUUID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@
    -- of the implementation.
    pipelineCacheUUID :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheHeaderVersionOne)
#endif
deriving instance Show PipelineCacheHeaderVersionOne

instance ToCStruct PipelineCacheHeaderVersionOne where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheHeaderVersionOne{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (headerSize)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (headerVersion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (deviceID)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineCacheUUID)
    f
  cStructSize = 32
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PipelineCacheHeaderVersionOne where
  peekCStruct p = do
    headerSize <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    headerVersion <- peek @PipelineCacheHeaderVersion ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion))
    vendorID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pipelineCacheUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PipelineCacheHeaderVersionOne
             headerSize headerVersion vendorID deviceID pipelineCacheUUID

instance Storable PipelineCacheHeaderVersionOne where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheHeaderVersionOne where
  zero = PipelineCacheHeaderVersionOne
           zero
           zero
           zero
           zero
           mempty


-- | VkDrawIndirectCommand - Structure specifying a indirect drawing command
--
-- = Description
--
-- The members of 'DrawIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDraw'.
--
-- == Valid Usage
--
-- -   #VUID-VkDrawIndirectCommand-None-00500# For a given vertex buffer
--     binding, any attribute data fetched /must/ be entirely contained
--     within the corresponding vertex buffer binding, as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fxvertex-input>
--
-- -   #VUID-VkDrawIndirectCommand-firstInstance-00501# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndirectCommand)
#endif
deriving instance Show DrawIndirectCommand

instance ToCStruct DrawIndirectCommand where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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


-- | VkDrawIndexedIndirectCommand - Structure specifying a indexed indirect
-- drawing command
--
-- = Description
--
-- The members of 'DrawIndexedIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   #VUID-VkDrawIndexedIndirectCommand-None-00552# For a given vertex
--     buffer binding, any attribute data fetched /must/ be entirely
--     contained within the corresponding vertex buffer binding, as
--     described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fxvertex-input>
--
-- -   #VUID-VkDrawIndexedIndirectCommand-indexSize-00553# (@indexSize@ ×
--     (@firstIndex@ + @indexCount@) + @offset@) /must/ be less than or
--     equal to the size of the bound index buffer, with @indexSize@ being
--     based on the type specified by @indexType@, where the index buffer,
--     @indexType@, and @offset@ are specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'
--
-- -   #VUID-VkDrawIndexedIndirectCommand-firstInstance-00554# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndexedIndirectCommand)
#endif
deriving instance Show DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand where
  withCStruct x f = allocaBytes 20 $ \p -> pokeCStruct p x (f p)
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


-- | VkDispatchIndirectCommand - Structure specifying a indirect dispatching
-- command
--
-- = Description
--
-- The members of 'DispatchIndirectCommand' have the same meaning as the
-- corresponding parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatch'.
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- | @x@ is the number of local workgroups to dispatch in the X dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-x-00417# @x@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
    x :: Word32
  , -- | @y@ is the number of local workgroups to dispatch in the Y dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-y-00418# @y@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
    y :: Word32
  , -- | @z@ is the number of local workgroups to dispatch in the Z dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-z-00419# @z@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
    z :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchIndirectCommand)
#endif
deriving instance Show DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
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

