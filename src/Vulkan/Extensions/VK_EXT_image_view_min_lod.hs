{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_view_min_lod - device extension
--
-- == VK_EXT_image_view_min_lod
--
-- [__Name String__]
--     @VK_EXT_image_view_min_lod@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     392
--
-- [__Revision__]
--     1
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
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_view_min_lod] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_image_view_min_lod extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Samuel Iglesias Gonsalvez, Igalia
--
--     -   Tobias Hector, AMD
--
--     -   Jason Ekstrand, Intel
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension allows applications to clamp the minimum LOD value during
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-image-level-selection Image Level(s) Selection>,
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-gather Texel Gathering>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-integer-coordinate-operations Integer Texel Coordinate Operations>
-- with a given 'Vulkan.Core10.Handles.ImageView' by
-- 'ImageViewMinLodCreateInfoEXT'::@minLod@.
--
-- This extension may be useful to restrict a
-- 'Vulkan.Core10.Handles.ImageView' to only mips which have been uploaded,
-- and the use of fractional @minLod@ can be useful for smoothly
-- introducing new mip levels when using linear mipmap filtering.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewMinLodCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageViewMinLodFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-07-06 (Joshua Ashton)
--
--     -   Initial version
--
-- == See Also
--
-- 'ImageViewMinLodCreateInfoEXT',
-- 'PhysicalDeviceImageViewMinLodFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_view_min_lod Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_view_min_lod  ( PhysicalDeviceImageViewMinLodFeaturesEXT(..)
                                                    , ImageViewMinLodCreateInfoEXT(..)
                                                    , EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION
                                                    , pattern EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION
                                                    , EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME
                                                    , pattern EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME
                                                    ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT))
-- | VkPhysicalDeviceImageViewMinLodFeaturesEXT - Structure describing
-- whether clamping the min lod of a image view is supported by the
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageViewMinLodFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageViewMinLodFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_view_min_lod VK_EXT_image_view_min_lod>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageViewMinLodFeaturesEXT = PhysicalDeviceImageViewMinLodFeaturesEXT
  { -- | #features-minLod# @minLod@ indicates whether the implementation supports
    -- clamping the minimum LOD value during
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-image-level-selection Image Level(s) Selection>,
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-gather Texel Gathering>
    -- and
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-integer-coordinate-operations Integer Texel Coordinate Operations>
    -- with a given 'Vulkan.Core10.Handles.ImageView' by
    -- 'ImageViewMinLodCreateInfoEXT'::@minLod@.
    minLod :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageViewMinLodFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageViewMinLodFeaturesEXT

instance ToCStruct PhysicalDeviceImageViewMinLodFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageViewMinLodFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (minLod))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageViewMinLodFeaturesEXT where
  peekCStruct p = do
    minLod <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageViewMinLodFeaturesEXT
             (bool32ToBool minLod)

instance Storable PhysicalDeviceImageViewMinLodFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageViewMinLodFeaturesEXT where
  zero = PhysicalDeviceImageViewMinLodFeaturesEXT
           zero


-- | VkImageViewMinLodCreateInfoEXT - Structure describing the minimum lod of
-- an image view
--
-- = Description
--
-- If the @pNext@ chain includes a 'ImageViewMinLodCreateInfoEXT'
-- structure, then that structure includes a parameter specifying a value
-- to clamp the minimum LOD value during
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-image-level-selection Image Level(s) Selection>,
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-gather Texel Gathering>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-integer-coordinate-operations Integer Texel Coordinate Operations>.
--
-- If the image view contains 'ImageViewMinLodCreateInfoEXT' and it is used
-- as part of a sampling operation:
--
-- minLodFloatimageView = @minLod@
--
-- otherwise:
--
-- minLodFloatimageView = 0.0
--
-- An integer variant of this parameter is also defined for sampling
-- operations which access integer mipmap levels:
--
-- minLodIntegerimageView = ⌊minLodFloatimageView⌋
--
-- == Valid Usage
--
-- -   #VUID-VkImageViewMinLodCreateInfoEXT-minLod-06455# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-minLod minLod>
--     feature is not enabled, @minLod@ /must/ be @0.0@.
--
-- -   #VUID-VkImageViewMinLodCreateInfoEXT-minLod-06456# @minLod@ /must/
--     be less or equal to the index of the last mipmap level accessible to
--     the view.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageViewMinLodCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_view_min_lod VK_EXT_image_view_min_lod>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageViewMinLodCreateInfoEXT = ImageViewMinLodCreateInfoEXT
  { -- | @minLod@ is the value to clamp the minimum LOD accessible by this
    -- 'Vulkan.Core10.Handles.ImageView'.
    minLod :: Float }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewMinLodCreateInfoEXT)
#endif
deriving instance Show ImageViewMinLodCreateInfoEXT

instance ToCStruct ImageViewMinLodCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewMinLodCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (minLod))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct ImageViewMinLodCreateInfoEXT where
  peekCStruct p = do
    minLod <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    pure $ ImageViewMinLodCreateInfoEXT
             (coerce @CFloat @Float minLod)

instance Storable ImageViewMinLodCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewMinLodCreateInfoEXT where
  zero = ImageViewMinLodCreateInfoEXT
           zero


type EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION"
pattern EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION = 1


type EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME = "VK_EXT_image_view_min_lod"

-- No documentation found for TopLevel "VK_EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME"
pattern EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME = "VK_EXT_image_view_min_lod"

