{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texture_compression_astc_hdr - device extension
--
-- = Registered Extension Number
--
-- 67
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-28
--
-- [__IP Status__]
--     No known issues.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension adds support for textures compressed using the Adaptive
-- Scalable Texture Compression (ASTC) High Dynamic Range (HDR) profile.
--
-- When this extension is enabled, the HDR profile is supported for all
-- ASTC formats listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC Compressed Image Formats>.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME'
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should we add a feature or limit for this functionality?
--
-- Yes. It is consistent with the ASTC LDR support to add a feature like
-- textureCompressionASTC_HDR.
--
-- The feature is strictly speaking redundant as long as this is just an
-- extension; it would be sufficient to just enable the extension. But
-- adding the feature is more forward-looking if wanted to make this an
-- optional core feature in the future.
--
-- 2) Should we introduce new format enums for HDR?
--
-- Yes. Vulkan 1.0 describes the ASTC format enums as UNORM, e.g.
-- 'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_UNORM_BLOCK', so it’s
-- confusing to make these contain HDR data. Note that the OpenGL (ES)
-- extensions did not make this distinction because a single ASTC HDR
-- texture may contain both unorm and float blocks. Implementations /may/
-- not be able to distinguish between LDR and HDR ASTC textures internally
-- and just treat them as the same format, i.e. if this extension is
-- supported then sampling from a
-- 'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_UNORM_BLOCK' image format
-- /may/ return HDR results. Applications /can/ get predictable results by
-- using the appropriate image format.
--
-- == Version History
--
-- -   Revision 1, 2019-05-28 (Jan-Harald Fredriksen)
--
--     -   Initial version
--
-- = See Also
--
-- 'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texture_compression_astc_hdr Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr  ( PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT(..)
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT))
-- | VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT - Structure
-- describing ASTC HDR features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.createDevice' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
  { -- | #features-textureCompressionASTC_HDR# @textureCompressionASTC_HDR@
    -- indicates whether all of the ASTC HDR compressed texture formats are
    -- supported. If this feature is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT'
    --
    -- To query for additional properties, or if the feature is not enabled,
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
    -- and
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties'
    -- /can/ be used to check for supported properties of individual formats as
    -- normal.
    textureCompressionASTC_HDR :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT

instance ToCStruct PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_HDR))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  peekCStruct p = do
    textureCompressionASTC_HDR <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
             (bool32ToBool textureCompressionASTC_HDR)

instance Storable PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT where
  zero = PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
           zero


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

