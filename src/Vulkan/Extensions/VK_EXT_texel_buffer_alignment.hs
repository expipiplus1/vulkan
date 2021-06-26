{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texel_buffer_alignment - device extension
--
-- == VK_EXT_texel_buffer_alignment
--
-- [__Name String__]
--     @VK_EXT_texel_buffer_alignment@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     282
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_texel_buffer_alignment:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds more expressive alignment requirements for uniform
-- and storage texel buffers. Some implementations have single texel
-- alignment requirements that can’t be expressed via
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTexelBufferAlignmentFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME'
--
-- -   'EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-06-06 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  ( PhysicalDeviceTexelBufferAlignmentFeaturesEXT(..)
                                                        , PhysicalDeviceTexelBufferAlignmentPropertiesEXT(..)
                                                        , EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION
                                                        , pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION
                                                        , EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME
                                                        , pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT))
-- | VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT - Structure describing
-- the texel buffer alignment features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTexelBufferAlignmentFeaturesEXT = PhysicalDeviceTexelBufferAlignmentFeaturesEXT
  { -- | #features-texelBufferAlignment# @texelBufferAlignment@ indicates whether
    -- the implementation uses more specific alignment requirements advertised
    -- in 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT' rather than
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@.
    texelBufferAlignment :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTexelBufferAlignmentFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (texelBufferAlignment))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  peekCStruct p = do
    texelBufferAlignment <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTexelBufferAlignmentFeaturesEXT
             (bool32ToBool texelBufferAlignment)

instance Storable PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  zero = PhysicalDeviceTexelBufferAlignmentFeaturesEXT
           zero


-- | VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT - Structure describing
-- the texel buffer alignment requirements supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-required required>
-- maximum minimum value of
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@,
-- for any format that supports use as a texel buffer.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTexelBufferAlignmentPropertiesEXT = PhysicalDeviceTexelBufferAlignmentPropertiesEXT
  { -- | #limits-storageTexelBufferOffsetAlignmentBytes#
    -- @storageTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a storage texel buffer of any format.
    storageTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #limits-storageTexelBufferOffsetSingleTexelAlignment#
    -- @storageTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a storage texel buffer of any format.
    storageTexelBufferOffsetSingleTexelAlignment :: Bool
  , -- | #limits-uniformTexelBufferOffsetAlignmentBytes#
    -- @uniformTexelBufferOffsetAlignmentBytes@ is a byte alignment that is
    -- sufficient for a uniform texel buffer of any format.
    uniformTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- | #limits-uniformTexelBufferOffsetSingleTexelAlignment#
    -- @uniformTexelBufferOffsetSingleTexelAlignment@ indicates whether single
    -- texel alignment is sufficient for a uniform texel buffer of any format.
    uniformTexelBufferOffsetSingleTexelAlignment :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTexelBufferAlignmentPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTexelBufferAlignmentPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (storageTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storageTexelBufferOffsetSingleTexelAlignment))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (uniformTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (uniformTexelBufferOffsetSingleTexelAlignment))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  peekCStruct p = do
    storageTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    storageTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    uniformTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    uniformTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceTexelBufferAlignmentPropertiesEXT
             storageTexelBufferOffsetAlignmentBytes (bool32ToBool storageTexelBufferOffsetSingleTexelAlignment) uniformTexelBufferOffsetAlignmentBytes (bool32ToBool uniformTexelBufferOffsetSingleTexelAlignment)

instance Storable PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  zero = PhysicalDeviceTexelBufferAlignmentPropertiesEXT
           zero
           zero
           zero
           zero


type EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1


type EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

