{-# language CPP #-}
-- | = Name
--
-- VK_EXT_astc_decode_mode - device extension
--
-- == VK_EXT_astc_decode_mode
--
-- [__Name String__]
--     @VK_EXT_astc_decode_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     68
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
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_astc_decode_mode:%20&body=@janharaldfredriksen-arm%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-07
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- The existing specification requires that low dynamic range (LDR) ASTC
-- textures are decompressed to FP16 values per component. In many cases,
-- decompressing LDR textures to a lower precision intermediate result
-- gives acceptable image quality. Source material for LDR textures is
-- typically authored as 8-bit UNORM values, so decoding to FP16 values
-- adds little value. On the other hand, reducing precision of the decoded
-- result reduces the size of the decompressed data, potentially improving
-- texture cache performance and saving power.
--
-- The goal of this extension is to enable this efficiency gain on existing
-- ASTC texture data. This is achieved by giving the application the
-- ability to select the intermediate decoding precision.
--
-- Three decoding options are provided:
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16A16_SFLOAT'
--     precision: This is the default, and matches the required behavior in
--     the core API.
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM'
--     precision: This is provided as an option in LDR mode.
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32'
--     precision: This is provided as an option in both LDR and HDR mode.
--     In this mode, negative values cannot be represented and are clamped
--     to zero. The alpha component is ignored, and the results are as if
--     alpha was 1.0. This decode mode is optional and support can be
--     queried via the physical device properties.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewASTCDecodeModeEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceASTCDecodeFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ASTC_DECODE_MODE_EXTENSION_NAME'
--
-- -   'EXT_ASTC_DECODE_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT'
--
-- == Issues
--
-- 1) Are implementations allowed to decode at a higher precision than what
-- is requested?
--
-- > RESOLUTION: No.
-- > If we allow this, then this extension could be exposed on all
-- > implementations that support ASTC.
-- > But developers would have no way of knowing what precision was actually
-- > used, and thus whether the image quality is sufficient at reduced
-- > precision.
--
-- 2) Should the decode mode be image view state and\/or sampler state?
--
-- > RESOLUTION: Image view state only.
-- > Some implementations treat the different decode modes as different
-- > texture formats.
--
-- == Example
--
-- Create an image view that decodes to
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM' precision:
--
-- >     VkImageViewASTCDecodeModeEXT decodeMode =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT, // sType
-- >         NULL, // pNext
-- >         VK_FORMAT_R8G8B8A8_UNORM // decode mode
-- >     };
-- >
-- >     VkImageViewCreateInfo createInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO, // sType
-- >         &decodeMode, // pNext
-- >         // flags, image, viewType set to application-desired values
-- >         VK_FORMAT_ASTC_8x8_UNORM_BLOCK, // format
-- >         // components, subresourceRange set to application-desired values
-- >     };
-- >
-- >     VkImageView imageView;
-- >     VkResult result = vkCreateImageView(
-- >         device,
-- >         &createInfo,
-- >         NULL,
-- >         &imageView);
--
-- == Version History
--
-- -   Revision 1, 2018-08-07 (Jan-Harald Fredriksen)
--
--     -   Initial revision
--
-- = See Also
--
-- 'ImageViewASTCDecodeModeEXT', 'PhysicalDeviceASTCDecodeFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_astc_decode_mode Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_astc_decode_mode  ( ImageViewASTCDecodeModeEXT(..)
                                                  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
                                                  , EXT_ASTC_DECODE_MODE_SPEC_VERSION
                                                  , pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION
                                                  , EXT_ASTC_DECODE_MODE_EXTENSION_NAME
                                                  , pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME
                                                  ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT))
-- | VkImageViewASTCDecodeModeEXT - Structure describing the ASTC decode mode
-- for an image view
--
-- == Valid Usage
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-decodeMode-02230# @decodeMode@
--     /must/ be one of
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16A16_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM', or
--     'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32'
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-decodeMode-02231# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-astc-decodeModeSharedExponent decodeModeSharedExponent>
--     feature is not enabled, @decodeMode@ /must/ not be
--     'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32'
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-decodeMode-02232# If @decodeMode@
--     is 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM' the image view
--     /must/ not include blocks using any of the ASTC HDR modes
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-format-04084# @format@ of the
--     image view /must/ be one of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC Compressed Image Formats>
--
-- If @format@ uses sRGB encoding then the @decodeMode@ has no effect.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT'
--
-- -   #VUID-VkImageViewASTCDecodeModeEXT-decodeMode-parameter#
--     @decodeMode@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageViewASTCDecodeModeEXT = ImageViewASTCDecodeModeEXT
  { -- | @decodeMode@ is the intermediate format used to decode ASTC compressed
    -- formats.
    decodeMode :: Format }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewASTCDecodeModeEXT)
#endif
deriving instance Show ImageViewASTCDecodeModeEXT

instance ToCStruct ImageViewASTCDecodeModeEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewASTCDecodeModeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (decodeMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    f

instance FromCStruct ImageViewASTCDecodeModeEXT where
  peekCStruct p = do
    decodeMode <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    pure $ ImageViewASTCDecodeModeEXT
             decodeMode

instance Storable ImageViewASTCDecodeModeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewASTCDecodeModeEXT where
  zero = ImageViewASTCDecodeModeEXT
           zero


-- | VkPhysicalDeviceASTCDecodeFeaturesEXT - Structure describing ASTC decode
-- mode features
--
-- = Members
--
-- The members of the 'PhysicalDeviceASTCDecodeFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceASTCDecodeFeaturesEXT' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceASTCDecodeFeaturesEXT' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.createDevice' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceASTCDecodeFeaturesEXT = PhysicalDeviceASTCDecodeFeaturesEXT
  { -- | #features-astc-decodeModeSharedExponent# @decodeModeSharedExponent@
    -- indicates whether the implementation supports decoding ASTC compressed
    -- formats to 'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32'
    -- internal precision.
    decodeModeSharedExponent :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceASTCDecodeFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceASTCDecodeFeaturesEXT

instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceASTCDecodeFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (decodeModeSharedExponent))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceASTCDecodeFeaturesEXT where
  peekCStruct p = do
    decodeModeSharedExponent <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceASTCDecodeFeaturesEXT
             (bool32ToBool decodeModeSharedExponent)

instance Storable PhysicalDeviceASTCDecodeFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceASTCDecodeFeaturesEXT where
  zero = PhysicalDeviceASTCDecodeFeaturesEXT
           zero


type EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION"
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1


type EXT_ASTC_DECODE_MODE_EXTENSION_NAME = "VK_EXT_astc_decode_mode"

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME"
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME = "VK_EXT_astc_decode_mode"

