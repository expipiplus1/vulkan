{-# language CPP #-}
-- | = Name
--
-- VK_NV_corner_sampled_image - device extension
--
-- == VK_NV_corner_sampled_image
--
-- [__Name String__]
--     @VK_NV_corner_sampled_image@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     51
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_corner_sampled_image] @dgkoch%0A<<Here describe the issue or question you have about the VK_NV_corner_sampled_image extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-13
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Chris Lentini, NVIDIA
--
-- == Description
--
-- This extension adds support for a new image organization, which this
-- extension refers to as “corner-sampled” images. A corner-sampled image
-- differs from a conventional image in the following ways:
--
-- -   Texels are centered on integer coordinates. See
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-unnormalized-to-integer Unnormalized Texel Coordinate Operations>
--
-- -   Normalized coordinates are scaled using coord × (dim - 1) rather
--     than coord × dim, where dim is the size of one dimension of the
--     image. See
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-normalized-to-unnormalized normalized texel coordinate transform>.
--
-- -   Partial derivatives are scaled using coord × (dim - 1) rather than
--     coord × dim. See
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-scale-factor Scale Factor Operation>.
--
-- -   Calculation of the next higher lod size goes according to ⌈dim \/ 2⌉
--     rather than ⌊dim \/ 2⌋. See
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>.
--
-- -   The minimum level size is 2x2 for 2D images and 2x2x2 for 3D images.
--     See
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-miplevel-sizing Image Miplevel Sizing>.
--
-- This image organization is designed to facilitate a system like Ptex
-- with separate textures for each face of a subdivision or polygon mesh.
-- Placing sample locations at pixel corners allows applications to
-- maintain continuity between adjacent patches by duplicating values along
-- shared edges. Additionally, using the modified mipmapping logic along
-- with texture dimensions of the form 2n+1 allows continuity across shared
-- edges even if the adjacent patches use different level-of-detail values.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCornerSampledImageFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME'
--
-- -   'NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV'
--
-- == Issues
--
-- 1.  What should this extension be named?
--
--     __DISCUSSION__: While naming this extension, we chose the most
--     distinctive aspect of the image organization and referred to such
--     images as “corner-sampled images”. As a result, we decided to name
--     the extension NV_corner_sampled_image.
--
-- 2.  Do we need a format feature flag so formats can advertise if they
--     support corner-sampling?
--
--     __DISCUSSION__: Currently NVIDIA supports this for all 2D and 3D
--     formats, but not for cube maps or depth-stencil formats. A format
--     feature might be useful if other vendors would only support this on
--     some formats.
--
-- 3.  Do integer texel coordinates have a different range for
--     corner-sampled images?
--
--     __RESOLVED__: No, these are unchanged.
--
-- 4.  Do unnormalized sampler coordinates work with corner-sampled images?
--     Are there any functional differences?
--
--     __RESOLVED__: Yes. Unnormalized coordinates are treated as already
--     scaled for corner-sample usage.
--
-- 5.  Should we have a diagram in the “Image Operations” chapter
--     demonstrating different texel sampling locations?
--
--     __UNRESOLVED__: Probably, but later.
--
-- == Version History
--
-- -   Revision 1, 2018-08-14 (Daniel Koch)
--
--     -   Internal revisions
--
-- -   Revision 2, 2018-08-14 (Daniel Koch)
--
--     -   ???
--
-- == See Also
--
-- 'PhysicalDeviceCornerSampledImageFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_corner_sampled_image Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_corner_sampled_image  ( PhysicalDeviceCornerSampledImageFeaturesNV(..)
                                                     , NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
                                                     , pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
                                                     , NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
                                                     , pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV))
-- | VkPhysicalDeviceCornerSampledImageFeaturesNV - Structure describing
-- corner sampled image features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceCornerSampledImageFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceCornerSampledImageFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_corner_sampled_image VK_NV_corner_sampled_image>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- | #features-cornersampledimage# @cornerSampledImage@ specifies whether
    -- images can be created with a
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'.
    -- See
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-images-corner-sampled Corner-Sampled Images>.
    cornerSampledImage :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCornerSampledImageFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCornerSampledImageFeaturesNV

instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCornerSampledImageFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cornerSampledImage))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCornerSampledImageFeaturesNV where
  peekCStruct p = do
    cornerSampledImage <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCornerSampledImageFeaturesNV
             (bool32ToBool cornerSampledImage)

instance Storable PhysicalDeviceCornerSampledImageFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCornerSampledImageFeaturesNV where
  zero = PhysicalDeviceCornerSampledImageFeaturesNV
           zero


type NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION"
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = 2


type NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = "VK_NV_corner_sampled_image"

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME"
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = "VK_NV_corner_sampled_image"

