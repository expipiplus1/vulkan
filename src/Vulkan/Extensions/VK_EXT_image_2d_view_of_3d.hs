{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_2d_view_of_3d - device extension
--
-- == VK_EXT_image_2d_view_of_3d
--
-- [__Name String__]
--     @VK_EXT_image_2d_view_of_3d@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     394
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
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_2d_view_of_3d] @zmike%0A*Here describe the issue or question you have about the VK_EXT_image_2d_view_of_3d extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Ricardo Garcia, Igalia
--
--     -   Graeme Leese, Broadcom
--
--     -   Ralph Potter, Samsung
--
--     -   Stu Smith, AMD
--
--     -   Shahbaz Youssefi, Google
--
--     -   Alex Walters, Imagination
--
-- == Description
--
-- This extension allows a single slice of a 3D image to be used as a 2D
-- view in image descriptors, matching both the functionality of
-- glBindImageTexture in OpenGL with the @layer@ parameter set to true and
-- 2D view binding provided by the extension EGL_KHR_gl_texture_3D_image.
-- It is primarily intended to support GL emulation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImage2DViewOf3DFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-25 (Mike Blumenkrantz)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceImage2DViewOf3DFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_2d_view_of_3d Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_2d_view_of_3d  ( PhysicalDeviceImage2DViewOf3DFeaturesEXT(..)
                                                     , EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION
                                                     , pattern EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION
                                                     , EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME
                                                     , pattern EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT))
-- | VkPhysicalDeviceImage2DViewOf3DFeaturesEXT - Structure describing
-- whether single-slice 2D views of 3D images can be used in image
-- descriptors
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceImage2DViewOf3DFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImage2DViewOf3DFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_2d_view_of_3d VK_EXT_image_2d_view_of_3d>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImage2DViewOf3DFeaturesEXT = PhysicalDeviceImage2DViewOf3DFeaturesEXT
  { -- | #features-image2DViewOf3D# @image2DViewOf3D@ indicates that the
    -- implementation supports using a 2D view of a 3D image in a descriptor of
    -- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
    -- if the image is created using
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT'.
    image2DViewOf3D :: Bool
  , -- | #features-sampler2DViewOf3D# @sampler2DViewOf3D@ indicates that the
    -- implementation supports using a 2D view of a 3D image in a descriptor of
    -- type 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
    -- or
    -- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
    -- if the image is created using
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT'.
    sampler2DViewOf3D :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImage2DViewOf3DFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImage2DViewOf3DFeaturesEXT

instance ToCStruct PhysicalDeviceImage2DViewOf3DFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImage2DViewOf3DFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (image2DViewOf3D))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sampler2DViewOf3D))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImage2DViewOf3DFeaturesEXT where
  peekCStruct p = do
    image2DViewOf3D <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sampler2DViewOf3D <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceImage2DViewOf3DFeaturesEXT
             (bool32ToBool image2DViewOf3D) (bool32ToBool sampler2DViewOf3D)

instance Storable PhysicalDeviceImage2DViewOf3DFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImage2DViewOf3DFeaturesEXT where
  zero = PhysicalDeviceImage2DViewOf3DFeaturesEXT
           zero
           zero


type EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION"
pattern EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION = 1


type EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME = "VK_EXT_image_2d_view_of_3d"

-- No documentation found for TopLevel "VK_EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME"
pattern EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME = "VK_EXT_image_2d_view_of_3d"

