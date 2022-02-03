{-# language CPP #-}
-- | = Name
--
-- VK_EXT_4444_formats - device extension
--
-- == VK_EXT_4444_formats
--
-- [__Name String__]
--     @VK_EXT_4444_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     341
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
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_4444_formats] @Joshua-Ashton%0A<<Here describe the issue or question you have about the VK_EXT_4444_formats extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-28
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension defines the 'FORMAT_A4R4G4B4_UNORM_PACK16_EXT' and
-- 'FORMAT_A4B4G4R4_UNORM_PACK16_EXT' formats which are defined in other
-- current graphics APIs.
--
-- This extension may be useful for building translation layers for those
-- APIs or for porting applications that use these formats without having
-- to resort to swizzles.
--
-- When VK_EXT_custom_border_color is used, these formats are not subject
-- to the same restrictions for border color without format as with
-- VK_FORMAT_B4G4R4A4_UNORM_PACK16.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevice4444FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_4444_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_4444_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'FORMAT_A4B4G4R4_UNORM_PACK16_EXT'
--
--     -   'FORMAT_A4R4G4B4_UNORM_PACK16_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT'
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
-- -   Revision 1, 2020-07-04 (Joshua Ashton)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevice4444FormatsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_4444_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_4444_formats  ( pattern FORMAT_A4R4G4B4_UNORM_PACK16_EXT
                                              , pattern FORMAT_A4B4G4R4_UNORM_PACK16_EXT
                                              , PhysicalDevice4444FormatsFeaturesEXT(..)
                                              , EXT_4444_FORMATS_SPEC_VERSION
                                              , pattern EXT_4444_FORMATS_SPEC_VERSION
                                              , EXT_4444_FORMATS_EXTENSION_NAME
                                              , pattern EXT_4444_FORMATS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.Format (Format(FORMAT_A4B4G4R4_UNORM_PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_A4R4G4B4_UNORM_PACK16))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT))
-- No documentation found for TopLevel "VK_FORMAT_A4R4G4B4_UNORM_PACK16_EXT"
pattern FORMAT_A4R4G4B4_UNORM_PACK16_EXT = FORMAT_A4R4G4B4_UNORM_PACK16


-- No documentation found for TopLevel "VK_FORMAT_A4B4G4R4_UNORM_PACK16_EXT"
pattern FORMAT_A4B4G4R4_UNORM_PACK16_EXT = FORMAT_A4B4G4R4_UNORM_PACK16


-- | VkPhysicalDevice4444FormatsFeaturesEXT - Structure describing additional
-- 4444 formats supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevice4444FormatsFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevice4444FormatsFeaturesEXT' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- Note
--
-- Although the formats defined by the @VK_EXT_4444_formats@ extension were
-- promoted to Vulkan 1.3 as optional formats, the
-- 'PhysicalDevice4444FormatsFeaturesEXT' structure was not promoted to
-- Vulkan 1.3.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_4444_formats VK_EXT_4444_formats>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevice4444FormatsFeaturesEXT = PhysicalDevice4444FormatsFeaturesEXT
  { -- | #features-formatA4R4G4B4# @formatA4R4G4B4@ indicates that the
    -- implementation /must/ support using a
    -- 'Vulkan.Core10.Enums.Format.Format' of
    -- 'FORMAT_A4R4G4B4_UNORM_PACK16_EXT' with at least the following
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    formatA4R4G4B4 :: Bool
  , -- | #features-formatA4B4G4R4# @formatA4B4G4R4@ indicates that the
    -- implementation /must/ support using a
    -- 'Vulkan.Core10.Enums.Format.Format' of
    -- 'FORMAT_A4B4G4R4_UNORM_PACK16_EXT' with at least the following
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    --
    -- -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    formatA4B4G4R4 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevice4444FormatsFeaturesEXT)
#endif
deriving instance Show PhysicalDevice4444FormatsFeaturesEXT

instance ToCStruct PhysicalDevice4444FormatsFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevice4444FormatsFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (formatA4R4G4B4))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (formatA4B4G4R4))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevice4444FormatsFeaturesEXT where
  peekCStruct p = do
    formatA4R4G4B4 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    formatA4B4G4R4 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevice4444FormatsFeaturesEXT
             (bool32ToBool formatA4R4G4B4) (bool32ToBool formatA4B4G4R4)

instance Storable PhysicalDevice4444FormatsFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevice4444FormatsFeaturesEXT where
  zero = PhysicalDevice4444FormatsFeaturesEXT
           zero
           zero


type EXT_4444_FORMATS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_4444_FORMATS_SPEC_VERSION"
pattern EXT_4444_FORMATS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_4444_FORMATS_SPEC_VERSION = 1


type EXT_4444_FORMATS_EXTENSION_NAME = "VK_EXT_4444_formats"

-- No documentation found for TopLevel "VK_EXT_4444_FORMATS_EXTENSION_NAME"
pattern EXT_4444_FORMATS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_4444_FORMATS_EXTENSION_NAME = "VK_EXT_4444_formats"

