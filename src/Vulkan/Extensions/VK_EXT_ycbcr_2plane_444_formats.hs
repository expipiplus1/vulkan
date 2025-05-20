{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ycbcr_2plane_444_formats - device extension
--
-- == VK_EXT_ycbcr_2plane_444_formats
--
-- [__Name String__]
--     @VK_EXT_ycbcr_2plane_444_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     331
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
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
--     -   Tony Zlatinski
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ycbcr_2plane_444_formats] @tzlatinski%0A*Here describe the issue or question you have about the VK_EXT_ycbcr_2plane_444_formats extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Ping Liu, Intel
--
-- == Description
--
-- This extension adds some Y′CBCR formats that are in common use for video
-- encode and decode, but were not part of the
-- @VK_KHR_sampler_ycbcr_conversion@ extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT'
--
--     -   'FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- This extension has been partially promoted. The format enumerants
-- introduced by the extension are included in core Vulkan 1.3, with the
-- EXT suffix omitted. However, runtime support for these formats is
-- optional in core Vulkan 1.3, while if this extension is supported,
-- runtime support is mandatory. The feature structure is not promoted. The
-- original enum names are still available as aliases of the core
-- functionality.
--
-- == Version History
--
-- -   Revision 1, 2020-03-08 (Piers Daniell)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_ycbcr_2plane_444_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats  ( pattern FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT
                                                          , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT
                                                          , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT
                                                          , pattern FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT
                                                          , PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT(..)
                                                          , EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION
                                                          , pattern EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION
                                                          , EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME
                                                          , pattern EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16R16_2PLANE_444_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8R8_2PLANE_444_UNORM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT))
-- No documentation found for TopLevel "VK_FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT"
pattern FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT = FORMAT_G8_B8R8_2PLANE_444_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT"
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT = FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT"
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT = FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT"
pattern FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT = FORMAT_G16_B16R16_2PLANE_444_UNORM


-- | VkPhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT - Structure describing
-- whether the implementation supports additional 2-plane 444 Y′CBCR
-- formats
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- Note
--
-- Although the formats defined by the @VK_EXT_ycbcr_2plane_444_formats@
-- were promoted to Vulkan 1.3 as optional formats, the
-- 'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT' structure was not
-- promoted to Vulkan 1.3.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ycbcr_2plane_444_formats VK_EXT_ycbcr_2plane_444_formats>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT = PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT
  { -- | #features-ycbcr2plane444Formats# @ycbcr2plane444Formats@ indicates that
    -- the implementation supports the following 2-plane 444 Y′CBCR formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_444_UNORM'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_G16_B16R16_2PLANE_444_UNORM'
    ycbcr2plane444Formats :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (ycbcr2plane444Formats))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT where
  peekCStruct p = do
    ycbcr2plane444Formats <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT
             (bool32ToBool ycbcr2plane444Formats)

instance Storable PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT where
  zero = PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT
           zero


type EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION"
pattern EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION = 1


type EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME = "VK_EXT_ycbcr_2plane_444_formats"

-- No documentation found for TopLevel "VK_EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME"
pattern EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME = "VK_EXT_ycbcr_2plane_444_formats"

