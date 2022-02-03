{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_texel_buffer_alignment"
module Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment  ( PhysicalDeviceTexelBufferAlignmentProperties(..)
                                                                  , StructureType(..)
                                                                  ) where

import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceTexelBufferAlignmentProperties - Structure describing
-- the texel buffer alignment requirements supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceTexelBufferAlignmentProperties' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- If the single texel alignment property is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', then the buffer view’s offset
-- /must/ be aligned to the corresponding byte alignment value. If the
-- single texel alignment property is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', then the buffer view’s offset
-- /must/ be aligned to the lesser of the corresponding byte alignment
-- value or the size of a single texel, based on
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'::@format@. If the size
-- of a single texel is a multiple of three bytes, then the size of a
-- single component of the format is used instead.
--
-- These limits /must/ not advertise a larger alignment than the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-required required>
-- maximum minimum value of
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@,
-- for any format that supports use as a texel buffer.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment VK_EXT_texel_buffer_alignment>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTexelBufferAlignmentProperties = PhysicalDeviceTexelBufferAlignmentProperties
  { -- | #extension-limits-storageTexelBufferOffsetAlignmentBytes#
    -- @storageTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a storage texel buffer of any format. The value /must/ be
    -- a power of two.
    storageTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #extension-limits-storageTexelBufferOffsetSingleTexelAlignment#
    -- @storageTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a storage texel buffer of any format.
    -- The value /must/ be a power of two.
    storageTexelBufferOffsetSingleTexelAlignment :: Bool
  , -- | #extension-limits-uniformTexelBufferOffsetAlignmentBytes#
    -- @uniformTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a uniform texel buffer of any format. The value /must/ be
    -- a power of two.
    uniformTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #extension-limits-uniformTexelBufferOffsetSingleTexelAlignment#
    -- @uniformTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a uniform texel buffer of any format.
    -- The value /must/ be a power of two.
    uniformTexelBufferOffsetSingleTexelAlignment :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTexelBufferAlignmentProperties)
#endif
deriving instance Show PhysicalDeviceTexelBufferAlignmentProperties

instance ToCStruct PhysicalDeviceTexelBufferAlignmentProperties where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTexelBufferAlignmentProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (storageTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storageTexelBufferOffsetSingleTexelAlignment))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (uniformTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (uniformTexelBufferOffsetSingleTexelAlignment))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTexelBufferAlignmentProperties where
  peekCStruct p = do
    storageTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    storageTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    uniformTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    uniformTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceTexelBufferAlignmentProperties
             storageTexelBufferOffsetAlignmentBytes (bool32ToBool storageTexelBufferOffsetSingleTexelAlignment) uniformTexelBufferOffsetAlignmentBytes (bool32ToBool uniformTexelBufferOffsetSingleTexelAlignment)

instance Storable PhysicalDeviceTexelBufferAlignmentProperties where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTexelBufferAlignmentProperties where
  zero = PhysicalDeviceTexelBufferAlignmentProperties
           zero
           zero
           zero
           zero

