{-# language CPP #-}
-- | = Name
--
-- VK_EXT_rgba10x6_formats - device extension
--
-- == VK_EXT_rgba10x6_formats
--
-- [__Name String__]
--     @VK_EXT_rgba10x6_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     345
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_rgba10x6_formats] @janharaldfredriksen-arm%0A<<Here describe the issue or question you have about the VK_EXT_rgba10x6_formats extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- This extension enables the
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16'
-- format to be used without a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- enabled.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRGBA10X6FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_RGBA10X6_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_RGBA10X6_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should we reuse the existing format enumeration or introduce a new
-- one?
--
-- __RESOLVED__: We reuse an existing format enumeration,
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16',
-- that was previously exclusively used for YCbCr and therefore had a set
-- of limitations related to that usage. The alternative was to introduce a
-- new format token with exactly the same bit representation as the
-- existing token, but without the limitations.
--
-- 2) Should we only introduce
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16'
-- or also 1-3 component variations?
--
-- __RESOLVED__: Only the 4-component format is introduced because the 1-
-- and 2- component variations are already not exclusive to YCbCr, and the
-- 3-component variation is not a good match for hardware capabilities.
--
-- == Version History
--
-- -   Revision 1, 2021-09-29 (Jan-Harald Fredriksen)
--
--     -   Initial EXT version
--
-- == See Also
--
-- 'PhysicalDeviceRGBA10X6FormatsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_rgba10x6_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_rgba10x6_formats  ( PhysicalDeviceRGBA10X6FormatsFeaturesEXT(..)
                                                  , EXT_RGBA10X6_FORMATS_SPEC_VERSION
                                                  , pattern EXT_RGBA10X6_FORMATS_SPEC_VERSION
                                                  , EXT_RGBA10X6_FORMATS_EXTENSION_NAME
                                                  , pattern EXT_RGBA10X6_FORMATS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT))
-- | VkPhysicalDeviceRGBA10X6FormatsFeaturesEXT - Structure describing
-- whether rendering to VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
-- formats can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceRGBA10X6FormatsFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRGBA10X6FormatsFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_rgba10x6_formats VK_EXT_rgba10x6_formats>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRGBA10X6FormatsFeaturesEXT = PhysicalDeviceRGBA10X6FormatsFeaturesEXT
  { -- | #features-formatRgba10x6WithoutYCbCrSampler#
    -- @formatRgba10x6WithoutYCbCrSampler@ indicates that
    -- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16'
    -- /can/ be used with a 'Vulkan.Core10.Handles.ImageView' with
    -- @subresourceRange.aspectMask@ equal to
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' without
    -- a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
    -- enabled.
    formatRgba10x6WithoutYCbCrSampler :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRGBA10X6FormatsFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceRGBA10X6FormatsFeaturesEXT

instance ToCStruct PhysicalDeviceRGBA10X6FormatsFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRGBA10X6FormatsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (formatRgba10x6WithoutYCbCrSampler))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRGBA10X6FormatsFeaturesEXT where
  peekCStruct p = do
    formatRgba10x6WithoutYCbCrSampler <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceRGBA10X6FormatsFeaturesEXT
             (bool32ToBool formatRgba10x6WithoutYCbCrSampler)

instance Storable PhysicalDeviceRGBA10X6FormatsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRGBA10X6FormatsFeaturesEXT where
  zero = PhysicalDeviceRGBA10X6FormatsFeaturesEXT
           zero


type EXT_RGBA10X6_FORMATS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_RGBA10X6_FORMATS_SPEC_VERSION"
pattern EXT_RGBA10X6_FORMATS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_RGBA10X6_FORMATS_SPEC_VERSION = 1


type EXT_RGBA10X6_FORMATS_EXTENSION_NAME = "VK_EXT_rgba10x6_formats"

-- No documentation found for TopLevel "VK_EXT_RGBA10X6_FORMATS_EXTENSION_NAME"
pattern EXT_RGBA10X6_FORMATS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_RGBA10X6_FORMATS_EXTENSION_NAME = "VK_EXT_rgba10x6_formats"

