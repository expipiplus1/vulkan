{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_acquire_unmodified - device extension
--
-- == VK_EXT_external_memory_acquire_unmodified
--
-- [__Name String__]
--     @VK_EXT_external_memory_acquire_unmodified@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     454
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--
-- [__Contact__]
--
--     -   Lina Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_external_memory_acquire_unmodified] @versalinyaa%0A*Here describe the issue or question you have about the VK_EXT_external_memory_acquire_unmodified extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_external_memory_acquire_unmodified.adoc VK_EXT_external_memory_acquire_unmodified>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-09
--
-- [__Contributors__]
--
--     -   Lina Versace, Google
--
--     -   Chia-I Wu, Google
--
--     -   James Jones, NVIDIA
--
--     -   Yiwei Zhang, Google
--
-- == Description
--
-- A memory barrier /may/ have a performance penalty when acquiring
-- ownership of a subresource range from an external queue family. This
-- extension provides API that /may/ reduce the performance penalty if
-- ownership of the subresource range was previously released to the
-- external queue family and if the resource’s memory has remained
-- unmodified between the release and acquire operations.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.BufferMemoryBarrier2',
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2':
--
--     -   'ExternalMemoryAcquireUnmodifiedEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT'
--
-- == Version History
--
-- -   Revision 1, 2023-03-09 (Lina Versace)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ExternalMemoryAcquireUnmodifiedEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_external_memory_acquire_unmodified Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified  ( ExternalMemoryAcquireUnmodifiedEXT(..)
                                                                    , EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION
                                                                    , pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION
                                                                    , EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME
                                                                    , pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME
                                                                    ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT))
-- | VkExternalMemoryAcquireUnmodifiedEXT - Structure specifying that
-- external memory has remained unmodified since releasing ownership
--
-- = Description
--
-- If the application releases ownership of the subresource range to one of
-- the special queue families reserved for external memory ownership
-- transfers with a memory barrier structure, and later re-acquires
-- ownership from the same queue family with a memory barrier structure,
-- and if no range of 'Vulkan.Core10.Handles.DeviceMemory' bound to the
-- resource was modified at any time between the /release operation/ and
-- the /acquire operation/, then the application /should/ add a
-- 'ExternalMemoryAcquireUnmodifiedEXT' structure to the @pNext@ chain of
-- the /acquire operation/\'s memory barrier structure because this /may/
-- reduce the performance penalty.
--
-- This struct is ignored if @acquireUnmodifiedMemory@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'. In particular,
-- 'Vulkan.Core10.FundamentalTypes.FALSE' does /not/ specify that memory
-- was modified.
--
-- This struct is ignored if the memory barrier’s @srcQueueFamilyIndex@ is
-- not a special queue family reserved for external memory ownership
-- transfers.
--
-- Note
--
-- The method by which the application determines whether memory was
-- modified between the /release operation/ and /acquire operation/ is
-- outside the scope of Vulkan.
--
-- For any Vulkan operation that accesses a resource, the application
-- /must/ not assume the implementation accesses the resource’s memory as
-- read-only, even for /apparently/ read-only operations such as transfer
-- commands and shader reads.
--
-- The validity of
-- 'ExternalMemoryAcquireUnmodifiedEXT'::@acquireUnmodifiedMemory@ is
-- independent of memory ranges outside the ranges of
-- 'Vulkan.Core10.Handles.DeviceMemory' bound to the resource. In
-- particular, it is independent of any implementation-private memory
-- associated with the resource.
--
-- == Valid Usage
--
-- -   #VUID-VkExternalMemoryAcquireUnmodifiedEXT-acquireUnmodifiedMemory-08922#
--     If @acquireUnmodifiedMemory@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', and the memory barrier’s
--     @srcQueueFamilyIndex@ is a special queue family reserved for
--     external memory ownership transfers (as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers>),
--     then each range of 'Vulkan.Core10.Handles.DeviceMemory' bound to the
--     resource /must/ have remained unmodified during all time since the
--     resource’s most recent release of ownership to the queue family.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExternalMemoryAcquireUnmodifiedEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_acquire_unmodified VK_EXT_external_memory_acquire_unmodified>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExternalMemoryAcquireUnmodifiedEXT = ExternalMemoryAcquireUnmodifiedEXT
  { -- | @acquireUnmodifiedMemory@ specifies, if
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', that no range of
    -- 'Vulkan.Core10.Handles.DeviceMemory' bound to the resource of the memory
    -- barrier’s subresource range was modified at any time since the
    -- resource’s most recent release of ownership to the queue family
    -- specified by the memory barrier’s @srcQueueFamilyIndex@. If
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', it specifies nothing.
    acquireUnmodifiedMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryAcquireUnmodifiedEXT)
#endif
deriving instance Show ExternalMemoryAcquireUnmodifiedEXT

instance ToCStruct ExternalMemoryAcquireUnmodifiedEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryAcquireUnmodifiedEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (acquireUnmodifiedMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ExternalMemoryAcquireUnmodifiedEXT where
  peekCStruct p = do
    acquireUnmodifiedMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ ExternalMemoryAcquireUnmodifiedEXT
             (bool32ToBool acquireUnmodifiedMemory)

instance Storable ExternalMemoryAcquireUnmodifiedEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryAcquireUnmodifiedEXT where
  zero = ExternalMemoryAcquireUnmodifiedEXT
           zero


type EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION = 1


type EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME = "VK_EXT_external_memory_acquire_unmodified"

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME = "VK_EXT_external_memory_acquire_unmodified"

