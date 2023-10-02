{-# language CPP #-}
-- | = Name
--
-- VK_EXT_border_color_swizzle - device extension
--
-- == VK_EXT_border_color_swizzle
--
-- [__Name String__]
--     @VK_EXT_border_color_swizzle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     412
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_border_color_swizzle] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_border_color_swizzle extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ricardo Garcia, Igalia
--
--     -   Shahbaz Youssefi, Google
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- After the publication of VK_EXT_custom_border_color, it was discovered
-- that some implementations had undefined behavior when combining a
-- sampler that uses a custom border color with image views whose component
-- mapping is not the identity mapping.
--
-- Since VK_EXT_custom_border_color has already shipped, this new extension
-- VK_EXT_border_color_swizzle was created to define the interaction
-- between custom border colors and non-identity image view swizzles, and
-- provide a work-around for implementations that must pre-swizzle the
-- sampler border color to match the image view component mapping it is
-- combined with.
--
-- This extension also defines the behavior between samplers with an opaque
-- black border color and image views with a non-identity component
-- swizzle, which was previously left undefined.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBorderColorSwizzleFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerBorderColorComponentMappingCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME'
--
-- -   'EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-10-12 (Piers Daniell)
--
--     -   Internal revisions.
--
-- == See Also
--
-- 'PhysicalDeviceBorderColorSwizzleFeaturesEXT',
-- 'SamplerBorderColorComponentMappingCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_border_color_swizzle Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_border_color_swizzle  ( SamplerBorderColorComponentMappingCreateInfoEXT(..)
                                                      , PhysicalDeviceBorderColorSwizzleFeaturesEXT(..)
                                                      , EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION
                                                      , pattern EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION
                                                      , EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME
                                                      , pattern EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME
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
import Vulkan.Core10.ImageView (ComponentMapping)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT))
-- | VkSamplerBorderColorComponentMappingCreateInfoEXT - Structure specifying
-- the component mapping of the border color
--
-- = Description
--
-- The 'Vulkan.Core10.ImageView.ComponentMapping' @components@ member
-- describes a remapping from components of the border color to components
-- of the vector returned by shader image instructions when the border
-- color is used.
--
-- == Valid Usage
--
-- -   #VUID-VkSamplerBorderColorComponentMappingCreateInfoEXT-borderColorSwizzle-06437#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-borderColorSwizzle borderColorSwizzle>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSamplerBorderColorComponentMappingCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT'
--
-- -   #VUID-VkSamplerBorderColorComponentMappingCreateInfoEXT-components-parameter#
--     @components@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ComponentMapping' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_border_color_swizzle VK_EXT_border_color_swizzle>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.ImageView.ComponentMapping',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerBorderColorComponentMappingCreateInfoEXT = SamplerBorderColorComponentMappingCreateInfoEXT
  { -- | @components@ is a 'Vulkan.Core10.ImageView.ComponentMapping' structure
    -- specifying a remapping of the border color components.
    components :: ComponentMapping
  , -- | @srgb@ indicates that the sampler will be combined with an image view
    -- that has an image format which is sRGB encoded.
    srgb :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerBorderColorComponentMappingCreateInfoEXT)
#endif
deriving instance Show SamplerBorderColorComponentMappingCreateInfoEXT

instance ToCStruct SamplerBorderColorComponentMappingCreateInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerBorderColorComponentMappingCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ComponentMapping)) (components)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (srgb))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ComponentMapping)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SamplerBorderColorComponentMappingCreateInfoEXT where
  peekCStruct p = do
    components <- peekCStruct @ComponentMapping ((p `plusPtr` 16 :: Ptr ComponentMapping))
    srgb <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ SamplerBorderColorComponentMappingCreateInfoEXT
             components (bool32ToBool srgb)

instance Storable SamplerBorderColorComponentMappingCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerBorderColorComponentMappingCreateInfoEXT where
  zero = SamplerBorderColorComponentMappingCreateInfoEXT
           zero
           zero


-- | VkPhysicalDeviceBorderColorSwizzleFeaturesEXT - Structure describing
-- whether samplers with custom border colors require the component swizzle
-- specified in order to have defined behavior
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceBorderColorSwizzleFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceBorderColorSwizzleFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_border_color_swizzle VK_EXT_border_color_swizzle>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBorderColorSwizzleFeaturesEXT = PhysicalDeviceBorderColorSwizzleFeaturesEXT
  { -- | #features-borderColorSwizzle# @borderColorSwizzle@ indicates that
    -- defined values are returned by sampled image operations when used with a
    -- sampler that uses a
    -- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_OPAQUE_BLACK',
    -- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_OPAQUE_BLACK',
    -- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT', or
    -- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
    -- @borderColor@ and an image view that uses a
    -- non-<https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views-identity-mappings identity component mapping>,
    -- when either @borderColorSwizzleFromImage@ is enabled or the
    -- 'SamplerBorderColorComponentMappingCreateInfoEXT' is specified.
    borderColorSwizzle :: Bool
  , -- | #features-borderColorSwizzleFromImage# @borderColorSwizzleFromImage@
    -- indicates that the implementation will return the correct border color
    -- values from sampled image operations under the conditions expressed
    -- above, without the application having to specify the border color
    -- component mapping when creating the sampler object. If this feature bit
    -- is not set, applications /can/ chain a
    -- 'SamplerBorderColorComponentMappingCreateInfoEXT' structure when
    -- creating samplers for use with image views that do not have an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views-identity-mappings identity swizzle>
    -- and, when those samplers are combined with image views using the same
    -- component mapping, sampled image operations that use opaque black or
    -- custom border colors will return the correct border color values.
    borderColorSwizzleFromImage :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBorderColorSwizzleFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceBorderColorSwizzleFeaturesEXT

instance ToCStruct PhysicalDeviceBorderColorSwizzleFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBorderColorSwizzleFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (borderColorSwizzle))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (borderColorSwizzleFromImage))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBorderColorSwizzleFeaturesEXT where
  peekCStruct p = do
    borderColorSwizzle <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    borderColorSwizzleFromImage <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceBorderColorSwizzleFeaturesEXT
             (bool32ToBool borderColorSwizzle)
             (bool32ToBool borderColorSwizzleFromImage)

instance Storable PhysicalDeviceBorderColorSwizzleFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBorderColorSwizzleFeaturesEXT where
  zero = PhysicalDeviceBorderColorSwizzleFeaturesEXT
           zero
           zero


type EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION"
pattern EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION = 1


type EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME = "VK_EXT_border_color_swizzle"

-- No documentation found for TopLevel "VK_EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME"
pattern EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME = "VK_EXT_border_color_swizzle"

