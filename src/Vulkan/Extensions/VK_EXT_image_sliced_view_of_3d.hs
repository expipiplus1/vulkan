{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_sliced_view_of_3d - device extension
--
-- == VK_EXT_image_sliced_view_of_3d
--
-- [__Name String__]
--     @VK_EXT_image_sliced_view_of_3d@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     419
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_sliced_view_of_3d] @zmike%0A*Here describe the issue or question you have about the VK_EXT_image_sliced_view_of_3d extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_image_sliced_view_of_3d.adoc VK_EXT_image_sliced_view_of_3d>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Shahbaz Youssefi, Google
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows creating 3D views of 3D images such that the views
-- contain a subset of the slices in the image, using a Z offset and range,
-- for the purpose of using the views as storage image descriptors. This
-- matches functionality in D3D12 and is primarily intended to support
-- D3D12 emulation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewSlicedCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-10-21 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT',
-- 'ImageViewSlicedCreateInfoEXT',
-- 'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_sliced_view_of_3d Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d  ( ImageViewSlicedCreateInfoEXT(..)
                                                         , PhysicalDeviceImageSlicedViewOf3DFeaturesEXT(..)
                                                         , EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION
                                                         , pattern EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION
                                                         , EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME
                                                         , pattern EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME
                                                         , REMAINING_3D_SLICES_EXT
                                                         , pattern REMAINING_3D_SLICES_EXT
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT))
import Vulkan.Core10.APIConstants (REMAINING_3D_SLICES_EXT)
import Vulkan.Core10.APIConstants (pattern REMAINING_3D_SLICES_EXT)
-- | VkImageViewSlicedCreateInfoEXT - Specify the subset of 3D slices of an
-- image view
--
-- = Description
--
-- When this structure is chained to
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo' the @sliceOffset@ field is
-- treated as a Z-offset for the sliced view and @sliceCount@ specifies the
-- range. Shader accesses using a Z coordinate of 0 will access the depth
-- slice corresponding to @sliceOffset@ in the image, and in a shader, the
-- maximum in-bounds Z coordinate for the view is @sliceCount@ - 1.
--
-- A sliced 3D view /must/ only be used with a single mip level. The slice
-- coordinates are integer coordinates within the
-- @subresourceRange.baseMipLevel@ used to create the image view.
--
-- The effective view depth is equal to @extent.depth@ used to create the
-- @image@ for this view adjusted by @subresourceRange.baseMipLevel@ as
-- specified in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-mip-level-sizing Image Mip Level Sizing>.
--
-- Shader access to this image view is only affected by
-- 'ImageViewSlicedCreateInfoEXT' if it uses a descriptor of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'. For
-- access using any other descriptor type, the contents of
-- 'ImageViewSlicedCreateInfoEXT' are ignored; instead, @sliceOffset@ is
-- treated as being equal to 0, and @sliceCount@ is treated as being equal
-- to 'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-sliceOffset-07867#
--     @sliceOffset@ /must/ be less than the effective view depth as
--     specified in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-mip-level-sizing Image Mip Level Sizing>
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-sliceCount-07868# If
--     @sliceCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT', it /must/ be
--     be non-zero and @sliceOffset@ + @sliceCount@ /must/ be less than or
--     equal to the effective view depth as specified in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-mip-level-sizing Image Mip Level Sizing>
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-image-07869# @image@ /must/
--     have been created with @imageType@ equal to
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-viewType-07909# @viewType@
--     /must/ be 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D'
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-None-07870# The image view
--     /must/ reference exactly 1 mip level
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-None-07871# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-imageSlicedViewOf3D imageSlicedViewOf3D>
--     feature /must/ be enabled on the device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageViewSlicedCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_sliced_view_of_3d VK_EXT_image_sliced_view_of_3d>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageViewSlicedCreateInfoEXT = ImageViewSlicedCreateInfoEXT
  { -- | @sliceOffset@ is the Z-offset for the first 3D slice accessible to the
    -- image view.
    sliceOffset :: Word32
  , -- | @sliceCount@ is the number of 3D slices accessible to the image view.
    sliceCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewSlicedCreateInfoEXT)
#endif
deriving instance Show ImageViewSlicedCreateInfoEXT

instance ToCStruct ImageViewSlicedCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewSlicedCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (sliceOffset)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (sliceCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageViewSlicedCreateInfoEXT where
  peekCStruct p = do
    sliceOffset <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    sliceCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ ImageViewSlicedCreateInfoEXT
             sliceOffset sliceCount

instance Storable ImageViewSlicedCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewSlicedCreateInfoEXT where
  zero = ImageViewSlicedCreateInfoEXT
           zero
           zero


-- | VkPhysicalDeviceImageSlicedViewOf3DFeaturesEXT - Structure describing
-- whether slice-based views of 3D images can be used in storage image
-- descriptors
--
-- = Members
--
-- The members of the 'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_sliced_view_of_3d VK_EXT_image_sliced_view_of_3d>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageSlicedViewOf3DFeaturesEXT = PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
  { -- | #features-imageSlicedViewOf3D# @imageSlicedViewOf3D@ indicates that the
    -- implementation supports using a sliced view of a 3D image in a
    -- descriptor of type
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE' by
    -- using a 'ImageViewSlicedCreateInfoEXT' structure when creating the view.
    imageSlicedViewOf3D :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageSlicedViewOf3DFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageSlicedViewOf3DFeaturesEXT

instance ToCStruct PhysicalDeviceImageSlicedViewOf3DFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageSlicedViewOf3DFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageSlicedViewOf3D))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageSlicedViewOf3DFeaturesEXT where
  peekCStruct p = do
    imageSlicedViewOf3D <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
             (bool32ToBool imageSlicedViewOf3D)

instance Storable PhysicalDeviceImageSlicedViewOf3DFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageSlicedViewOf3DFeaturesEXT where
  zero = PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
           zero


type EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION"
pattern EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION = 1


type EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME = "VK_EXT_image_sliced_view_of_3d"

-- No documentation found for TopLevel "VK_EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME"
pattern EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME = "VK_EXT_image_sliced_view_of_3d"

