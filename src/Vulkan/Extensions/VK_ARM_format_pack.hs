{-# language CPP #-}
-- | = Name
--
-- VK_ARM_format_pack - device extension
--
-- = VK_ARM_format_pack
--
-- [__Name String__]
--     @VK_ARM_format_pack@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     610
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_format_pack] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_format_pack extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-24
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Lisa Wu, Arm
--
--     -   Oivind Boge, Arm
--
-- == Description
--
-- This extension adds support for additional 1-, 2- and 4-component
-- formats with 10, 12, or 14 bits of components in 16-bit containers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFormatPackFeaturesARM'
--
-- == New Enum Constants
--
-- -   'ARM_FORMAT_PACK_EXTENSION_NAME'
--
-- -   'ARM_FORMAT_PACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G14X2_B14X2R14X2_2PLANE_420_UNORM_3PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G14X2_B14X2R14X2_2PLANE_422_UNORM_3PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UINT_4PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6_UINT_2PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R10X6_UINT_PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R12X4G12X4B12X4A12X4_UINT_4PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R12X4G12X4_UINT_2PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R12X4_UINT_PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2B14X2A14X2_UINT_4PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2B14X2A14X2_UNORM_4PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2_UINT_2PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2_UNORM_2PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2_UINT_PACK16_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R14X2_UNORM_PACK16_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM'
--
-- === What do we call this extension?
--
-- __RESOLVED__
--
-- Many existing extension have the format in the name, but in this case we
-- want to expose multiple formats.
--
-- We will describe this set of formats as a “pack”.
--
-- === Compatibility classes
--
-- [__RESOLVED__]
--     Should these additional formats be in the same compatibility class
--     as any other formats? For single-plane formats, we put formats with
--     the same number of bits (but different types) in the same class.
--     Each multi-plane or subsampled format gets its own compatibility
--     class. This is consistent with how existing formats are handled.
--
-- === Format feature requirements
--
-- [__RESOLVED__]
--     The format feature queries could be used to determine what is
--     supported on any given implementation, but it may be useful to
--     establish a baseline requirement in the specification. For that
--     purpose, we require a set of format features - sufficient to enable
--     texture operations - to be supported for the added unsigned integer
--     single-plane formats. Other formats and format features are
--     optional.
--
-- == Version History
--
-- -   Revision 1, 2025-03-24
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_format_pack Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_format_pack  ( PhysicalDeviceFormatPackFeaturesARM(..)
                                             , ARM_FORMAT_PACK_SPEC_VERSION
                                             , pattern ARM_FORMAT_PACK_SPEC_VERSION
                                             , ARM_FORMAT_PACK_EXTENSION_NAME
                                             , pattern ARM_FORMAT_PACK_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM))
-- | VkPhysicalDeviceFormatPackFeaturesARM - Structure describing whether the
-- additional formats feature is supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceFormatPackFeaturesARM' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceFormatPackFeaturesARM', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_format_pack VK_ARM_format_pack>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFormatPackFeaturesARM = PhysicalDeviceFormatPackFeaturesARM
  { -- | #features-formatPack# @formatPack@ indicates that the implementation
    -- /must/ support using a 'Vulkan.Core10.Enums.Format.Format' of
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6_UINT_PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6_UINT_2PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UINT_4PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R12X4_UINT_PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R12X4G12X4_UINT_2PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R12X4G12X4B12X4A12X4_UINT_4PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R14X2_UINT_PACK16_ARM',
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2_UINT_2PACK16_ARM', and
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R14X2G14X2B14X2A14X2_UINT_4PACK16_ARM',
    -- with at least the following
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
    formatPack :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFormatPackFeaturesARM)
#endif
deriving instance Show PhysicalDeviceFormatPackFeaturesARM

instance ToCStruct PhysicalDeviceFormatPackFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFormatPackFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (formatPack))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FORMAT_PACK_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFormatPackFeaturesARM where
  peekCStruct p = do
    formatPack <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFormatPackFeaturesARM
             (bool32ToBool formatPack)

instance Storable PhysicalDeviceFormatPackFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFormatPackFeaturesARM where
  zero = PhysicalDeviceFormatPackFeaturesARM
           zero


type ARM_FORMAT_PACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_FORMAT_PACK_SPEC_VERSION"
pattern ARM_FORMAT_PACK_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_FORMAT_PACK_SPEC_VERSION = 1


type ARM_FORMAT_PACK_EXTENSION_NAME = "VK_ARM_format_pack"

-- No documentation found for TopLevel "VK_ARM_FORMAT_PACK_EXTENSION_NAME"
pattern ARM_FORMAT_PACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_FORMAT_PACK_EXTENSION_NAME = "VK_ARM_format_pack"

