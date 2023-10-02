{-# language CPP #-}
-- | = Name
--
-- VK_EXT_legacy_dithering - device extension
--
-- == VK_EXT_legacy_dithering
--
-- [__Name String__]
--     @VK_EXT_legacy_dithering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     466
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_legacy_dithering] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_legacy_dithering extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_legacy_dithering.adoc VK_EXT_legacy_dithering>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-03-31
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension exposes a hardware feature used by some vendors to
-- implement OpenGL’s dithering. The purpose of this extension is to
-- support layering OpenGL over Vulkan, by allowing the layer to take
-- advantage of the same hardware feature and provide equivalent dithering
-- to OpenGL applications.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLegacyDitheringFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LEGACY_DITHERING_EXTENSION_NAME'
--
-- -   'EXT_LEGACY_DITHERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_ENABLE_LEGACY_DITHERING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_ENABLE_LEGACY_DITHERING_BIT_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-31 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- == Issues
--
-- 1) In OpenGL, the dither state can change dynamically. Should this
-- extension add a pipeline state for dither?
--
-- __RESOLVED__: No. Changing dither state is rarely, if ever, done during
-- rendering. Every surveyed Android application either entirely disables
-- dither, explicitly enables it, or uses the default state (which is
-- enabled). Additionally, on some hardware dither can only be specified in
-- a render pass granularity, so a change in dither state would necessarily
-- need to cause a render pass break. This extension considers dynamic
-- changes in OpenGL dither state a theoretical situation, and expects the
-- layer to break the render pass in such a situation without any practical
-- downsides.
--
-- == See Also
--
-- 'PhysicalDeviceLegacyDitheringFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_legacy_dithering Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_legacy_dithering  ( PhysicalDeviceLegacyDitheringFeaturesEXT(..)
                                                  , EXT_LEGACY_DITHERING_SPEC_VERSION
                                                  , pattern EXT_LEGACY_DITHERING_SPEC_VERSION
                                                  , EXT_LEGACY_DITHERING_EXTENSION_NAME
                                                  , pattern EXT_LEGACY_DITHERING_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT))
-- | VkPhysicalDeviceLegacyDitheringFeaturesEXT - Structure describing
-- support for legacy dithering
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceLegacyDitheringFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceLegacyDitheringFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_legacy_dithering VK_EXT_legacy_dithering>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceLegacyDitheringFeaturesEXT = PhysicalDeviceLegacyDitheringFeaturesEXT
  { -- | #features-legacyDithering# @legacyDithering@ indicates whether the
    -- implementation supports
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-legacy-dithering Legacy Dithering>.
    legacyDithering :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceLegacyDitheringFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceLegacyDitheringFeaturesEXT

instance ToCStruct PhysicalDeviceLegacyDitheringFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceLegacyDitheringFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (legacyDithering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceLegacyDitheringFeaturesEXT where
  peekCStruct p = do
    legacyDithering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceLegacyDitheringFeaturesEXT
             (bool32ToBool legacyDithering)

instance Storable PhysicalDeviceLegacyDitheringFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceLegacyDitheringFeaturesEXT where
  zero = PhysicalDeviceLegacyDitheringFeaturesEXT
           zero


type EXT_LEGACY_DITHERING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LEGACY_DITHERING_SPEC_VERSION"
pattern EXT_LEGACY_DITHERING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LEGACY_DITHERING_SPEC_VERSION = 1


type EXT_LEGACY_DITHERING_EXTENSION_NAME = "VK_EXT_legacy_dithering"

-- No documentation found for TopLevel "VK_EXT_LEGACY_DITHERING_EXTENSION_NAME"
pattern EXT_LEGACY_DITHERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LEGACY_DITHERING_EXTENSION_NAME = "VK_EXT_legacy_dithering"

