{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_texture_compression_astc_hdr"
module Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr  ( PhysicalDeviceTextureCompressionASTCHDRFeatures(..)
                                                                        , Format(..)
                                                                        , StructureType(..)
                                                                        ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES))
import Vulkan.Core10.Enums.Format (Format(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceTextureCompressionASTCHDRFeatures - Structure describing
-- ASTC HDR features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceTextureCompressionASTCHDRFeatures' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceTextureCompressionASTCHDRFeatures' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texture_compression_astc_hdr VK_EXT_texture_compression_astc_hdr>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTextureCompressionASTCHDRFeatures = PhysicalDeviceTextureCompressionASTCHDRFeatures
  { -- | #extension-features-textureCompressionASTC_HDR#
    -- @textureCompressionASTC_HDR@ indicates whether all of the ASTC HDR
    -- compressed texture formats are supported. If this feature is enabled,
    -- then the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
    -- and
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
    -- features /must/ be supported in @optimalTilingFeatures@ for the
    -- following formats:
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x4_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_5x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_6x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_8x8_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x5_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x6_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x8_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_10x10_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x10_SFLOAT_BLOCK'
    --
    -- -   'Vulkan.Core10.Enums.Format.FORMAT_ASTC_12x12_SFLOAT_BLOCK'
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
deriving instance Generic (PhysicalDeviceTextureCompressionASTCHDRFeatures)
#endif
deriving instance Show PhysicalDeviceTextureCompressionASTCHDRFeatures

instance ToCStruct PhysicalDeviceTextureCompressionASTCHDRFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTextureCompressionASTCHDRFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (textureCompressionASTC_HDR))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTextureCompressionASTCHDRFeatures where
  peekCStruct p = do
    textureCompressionASTC_HDR <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTextureCompressionASTCHDRFeatures
             (bool32ToBool textureCompressionASTC_HDR)

instance Storable PhysicalDeviceTextureCompressionASTCHDRFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTextureCompressionASTCHDRFeatures where
  zero = PhysicalDeviceTextureCompressionASTCHDRFeatures
           zero

