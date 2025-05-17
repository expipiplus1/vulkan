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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_texel_buffer_alignment] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_texel_buffer_alignment extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds more expressive alignment requirements for uniform
-- and storage texel buffers. Some implementations have single texel
-- alignment requirements that cannot be expressed via
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. However, only the properties structure is promoted.
-- The feature structure is not promoted and @texelBufferAlignment@ is
-- enabled if using a Vulkan 1.3 instance. The original type name is still
-- available as an alias of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2019-06-06 (Jeff Bolz)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT
                                                        , PhysicalDeviceTexelBufferAlignmentFeaturesEXT(..)
                                                        , PhysicalDeviceTexelBufferAlignmentPropertiesEXT
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
import Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentProperties)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES


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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment VK_EXT_texel_buffer_alignment>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTexelBufferAlignmentFeaturesEXT = PhysicalDeviceTexelBufferAlignmentFeaturesEXT
  { -- | #features-texelBufferAlignment# @texelBufferAlignment@ indicates whether
    -- the implementation uses more specific alignment requirements advertised
    -- in
    -- 'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties'
    -- rather than
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


-- No documentation found for TopLevel "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT"
type PhysicalDeviceTexelBufferAlignmentPropertiesEXT = PhysicalDeviceTexelBufferAlignmentProperties


type EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1


type EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

