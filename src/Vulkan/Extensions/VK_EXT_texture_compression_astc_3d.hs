{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texture_compression_astc_3d - device extension
--
-- = VK_EXT_texture_compression_astc_3d
--
-- [__Name String__]
--     @VK_EXT_texture_compression_astc_3d@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     289
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_texture_compression_astc_3d] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_texture_compression_astc_3d extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_texture_compression_astc_3d.adoc VK_EXT_texture_compression_astc_3d>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-09
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
-- This extension adds support for 3D textures compressed using the
-- Adaptive Scalable Texture Compression (ASTC) format.
--
-- These formats are compressed in 3D. As such, each slice will contain
-- data for a Width x Height x Depth block of the texture. All transfer
-- operations are done at the granularity of block dimensions.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTextureCompressionASTC3DFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME'
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_UNORM_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_SFLOAT_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_SRGB_BLOCK_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_UNORM_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-06-09 (Jan-Harald Fredriksen)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_texture_compression_astc_3d Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texture_compression_astc_3d  ( PhysicalDeviceTextureCompressionASTC3DFeaturesEXT(..)
                                                             , EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION
                                                             , pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION
                                                             , EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME
                                                             , pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT))
-- | VkPhysicalDeviceTextureCompressionASTC3DFeaturesEXT - Structure
-- describing ASTC 3D features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the @VkPhysicalDeviceTextureCompressionASTC3DFeatures@ structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- @VkPhysicalDeviceTextureCompressionASTC3DFeatures@, it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texture_compression_astc_3d VK_EXT_texture_compression_astc_3d>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTextureCompressionASTC3DFeaturesEXT = PhysicalDeviceTextureCompressionASTC3DFeaturesEXT
  { -- | #features-textureCompressionASTC_3D# @textureCompressionASTC_3D@
    -- indicates whether all of the ASTC 3D compressed texture formats are
    -- supported. If this feature is enabled, then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_3x3x3_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x3x3_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x3_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4x4_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4x4_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x4_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x5_SFLOAT_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_UNORM_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_SRGB_BLOCK_EXT'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6x6_SFLOAT_BLOCK_EXT'
    textureCompressionASTC_3D :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTextureCompressionASTC3DFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTextureCompressionASTC3DFeaturesEXT

instance ToCStruct PhysicalDeviceTextureCompressionASTC3DFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTextureCompressionASTC3DFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_3D))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTextureCompressionASTC3DFeaturesEXT where
  peekCStruct p = do
    textureCompressionASTC_3D <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTextureCompressionASTC3DFeaturesEXT
             (bool32ToBool textureCompressionASTC_3D)

instance Storable PhysicalDeviceTextureCompressionASTC3DFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTextureCompressionASTC3DFeaturesEXT where
  zero = PhysicalDeviceTextureCompressionASTC3DFeaturesEXT
           zero


type EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION"
pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_SPEC_VERSION = 1


type EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME = "VK_EXT_texture_compression_astc_3d"

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME"
pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_3D_EXTENSION_NAME = "VK_EXT_texture_compression_astc_3d"

