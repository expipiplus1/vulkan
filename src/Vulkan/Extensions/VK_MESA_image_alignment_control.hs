{-# language CPP #-}
-- | = Name
--
-- VK_MESA_image_alignment_control - device extension
--
-- = VK_MESA_image_alignment_control
--
-- [__Name String__]
--     @VK_MESA_image_alignment_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     576
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_MESA_image_alignment_control] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_MESA_image_alignment_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to request a narrower alignment for
-- images than an implementation would otherwise require. Some
-- implementations internally support multiple image layouts in
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', each with
-- different alignment requirements and performance trade-offs. In some API
-- layering use cases such as D3D12, it is beneficial to be able to control
-- the alignment, since certain alignments for placed resources are
-- guaranteed to be supported, and emulating that expectation requires
-- unnecessary padding of allocations.
--
-- 'ImageAlignmentControlCreateInfoMESA' /can/ be chained to
-- 'Vulkan.Core10.Image.ImageCreateInfo', requesting that the alignment is
-- no more than the provided alignment. If the requested alignment is not
-- supported for a given 'Vulkan.Core10.Image.ImageCreateInfo', a larger
-- alignment /may/ be returned.
--
-- While something similar could be achieved with
-- @VK_EXT_image_drm_format_modifier@ in theory, this is not the intended
-- way to use that extension. Format modifiers are generally used for
-- externally shareable images, and would not be platform portable. It is
-- also a cumbersome API to use just to lower the alignment.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImageAlignmentControlCreateInfoMESA'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageAlignmentControlFeaturesMESA'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageAlignmentControlPropertiesMESA'
--
-- == New Enum Constants
--
-- -   'MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME'
--
-- -   'MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA'
--
-- == Version History
--
-- -   Revision 1, 2024-04-05 (Hans-Kristian Arntzen)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_MESA_image_alignment_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MESA_image_alignment_control  ( PhysicalDeviceImageAlignmentControlFeaturesMESA(..)
                                                          , PhysicalDeviceImageAlignmentControlPropertiesMESA(..)
                                                          , ImageAlignmentControlCreateInfoMESA(..)
                                                          , MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION
                                                          , pattern MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION
                                                          , MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME
                                                          , pattern MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA))
-- | VkPhysicalDeviceImageAlignmentControlFeaturesMESA - Structure describing
-- features supported by VK_MESA_image_alignment_control
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageAlignmentControlFeaturesMESA' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageAlignmentControlFeaturesMESA' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MESA_image_alignment_control VK_MESA_image_alignment_control>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageAlignmentControlFeaturesMESA = PhysicalDeviceImageAlignmentControlFeaturesMESA
  { -- | #features-imageAlignmentControl# @imageAlignmentControl@ specifies that
    -- 'ImageAlignmentControlCreateInfoMESA' /can/ be chained in
    -- 'Vulkan.Core10.Image.ImageCreateInfo'
    imageAlignmentControl :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageAlignmentControlFeaturesMESA)
#endif
deriving instance Show PhysicalDeviceImageAlignmentControlFeaturesMESA

instance ToCStruct PhysicalDeviceImageAlignmentControlFeaturesMESA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageAlignmentControlFeaturesMESA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageAlignmentControl))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageAlignmentControlFeaturesMESA where
  peekCStruct p = do
    imageAlignmentControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageAlignmentControlFeaturesMESA
             (bool32ToBool imageAlignmentControl)

instance Storable PhysicalDeviceImageAlignmentControlFeaturesMESA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageAlignmentControlFeaturesMESA where
  zero = PhysicalDeviceImageAlignmentControlFeaturesMESA
           zero


-- | VkPhysicalDeviceImageAlignmentControlPropertiesMESA - Structure
-- describing supported image alignments for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceImageAlignmentControlPropertiesMESA'
-- structure describe the following:
--
-- = Description
--
-- If the 'PhysicalDeviceImageAlignmentControlPropertiesMESA' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MESA_image_alignment_control VK_MESA_image_alignment_control>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageAlignmentControlPropertiesMESA = PhysicalDeviceImageAlignmentControlPropertiesMESA
  { -- | #limits-supportedImageAlignmentMask# @supportedImageAlignmentMask@ is a
    -- bitwise-or of all potentially supported image alignments for a given
    -- physical device when using
    -- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'. If a given
    -- alignment is supported, the application /can/ request an image to have
    -- that alignment. A given set of image creation parameters /may/ support a
    -- subset of these alignments. To determine if a particular alignment is
    -- supported for a given set of image creation parameters, check
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@alignment@ after
    -- chaining in 'ImageAlignmentControlCreateInfoMESA'.
    supportedImageAlignmentMask :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageAlignmentControlPropertiesMESA)
#endif
deriving instance Show PhysicalDeviceImageAlignmentControlPropertiesMESA

instance ToCStruct PhysicalDeviceImageAlignmentControlPropertiesMESA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageAlignmentControlPropertiesMESA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (supportedImageAlignmentMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceImageAlignmentControlPropertiesMESA where
  peekCStruct p = do
    supportedImageAlignmentMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceImageAlignmentControlPropertiesMESA
             supportedImageAlignmentMask

instance Storable PhysicalDeviceImageAlignmentControlPropertiesMESA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageAlignmentControlPropertiesMESA where
  zero = PhysicalDeviceImageAlignmentControlPropertiesMESA
           zero


-- | VkImageAlignmentControlCreateInfoMESA - Specify image alignment
--
-- = Description
--
-- If @maximumRequestedAlignment@ is not 0, the implementation /should/
-- choose an image memory layout that requires an alignment no larger than
-- @maximumRequestedAlignment@ as reported in
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@alignment@. If
-- such a layout does not exist for the given image creation parameters,
-- the implementation /should/ return the smallest alignment which is
-- supported in 'Vulkan.Core10.MemoryManagement.MemoryRequirements'.
--
-- If an implementation needs to disable image compression for
-- @maximumRequestedAlignment@ to be honored - where a larger alignment
-- would enable image compression - the implementation /should/ not use
-- @maximumRequestedAlignment@, and /should/ return the smallest alignment
-- which does not compromise compression. If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-imageCompressionControl imageCompressionControl>
-- feature is enabled, the application /can/ chain a
-- 'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionControlEXT'
-- with
-- 'Vulkan.Extensions.VK_EXT_image_compression_control.IMAGE_COMPRESSION_DISABLED_EXT'.
-- In this case, image compression considerations /should/ not apply when
-- implementation decides alignment.
--
-- == Valid Usage
--
-- -   #VUID-VkImageAlignmentControlCreateInfoMESA-maximumRequestedAlignment-09655#
--     If @maximumRequestedAlignment@ is not 0, @maximumRequestedAlignment@
--     /must/ be a power of two
--
-- -   #VUID-VkImageAlignmentControlCreateInfoMESA-maximumRequestedAlignment-09656#
--     If @maximumRequestedAlignment@ is not 0, the bitwise-and of
--     @maximumRequestedAlignment@ and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-supportedImageAlignmentMask supportedImageAlignmentMask>
--     /must/ be non-zero
--
-- -   #VUID-VkImageAlignmentControlCreateInfoMESA-imageAlignmentControl-09657#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-imageAlignmentControl imageAlignmentControl>
--     /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageAlignmentControlCreateInfoMESA-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MESA_image_alignment_control VK_MESA_image_alignment_control>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageAlignmentControlCreateInfoMESA = ImageAlignmentControlCreateInfoMESA
  { -- | @maximumRequestedAlignment@ specifies the maximum alignment for the
    -- image.
    maximumRequestedAlignment :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageAlignmentControlCreateInfoMESA)
#endif
deriving instance Show ImageAlignmentControlCreateInfoMESA

instance ToCStruct ImageAlignmentControlCreateInfoMESA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageAlignmentControlCreateInfoMESA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maximumRequestedAlignment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageAlignmentControlCreateInfoMESA where
  peekCStruct p = do
    maximumRequestedAlignment <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ ImageAlignmentControlCreateInfoMESA
             maximumRequestedAlignment

instance Storable ImageAlignmentControlCreateInfoMESA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageAlignmentControlCreateInfoMESA where
  zero = ImageAlignmentControlCreateInfoMESA
           zero


type MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION"
pattern MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION = 1


type MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME = "VK_MESA_image_alignment_control"

-- No documentation found for TopLevel "VK_MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME"
pattern MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME = "VK_MESA_image_alignment_control"

