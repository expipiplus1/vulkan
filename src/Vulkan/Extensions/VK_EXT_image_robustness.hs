{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_robustness - device extension
--
-- == VK_EXT_image_robustness
--
-- [__Name String__]
--     @VK_EXT_image_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     336
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
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_robustness] @gnl21%0A<<Here describe the issue or question you have about the VK_EXT_image_robustness extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Slawomir Cygan, Intel
--
-- == Description
--
-- This extension adds stricter requirements for how out of bounds reads
-- from images are handled. Rather than returning undefined values, most
-- out of bounds reads return R, G, and B values of zero and alpha values
-- of either zero or one. Components not present in the image format may be
-- set to zero or to values based on the format as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageRobustnessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT'
--
-- == Issues
--
-- 1.  How does this extension differ from VK_EXT_robustness2?
--
-- The guarantees provided by this extension are a subset of those provided
-- by the robustImageAccess2 feature of VK_EXT_robustness2. Where this
-- extension allows return values of (0, 0, 0, 0) or (0, 0, 0, 1),
-- robustImageAccess2 requires that a particular value dependent on the
-- image format be returned. This extension provides no guarantees about
-- the values returned for an access to an invalid Lod.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2020-04-27 (Graeme Leese)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceImageRobustnessFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_robustness  ( PhysicalDeviceImageRobustnessFeaturesEXT(..)
                                                  , EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
                                                  , pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT))
-- | VkPhysicalDeviceImageRobustnessFeaturesEXT - Structure describing the
-- out-of-bounds behavior for an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageRobustnessFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageRobustnessFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_robustness VK_EXT_image_robustness>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageRobustnessFeaturesEXT = PhysicalDeviceImageRobustnessFeaturesEXT
  { -- | #features-robustImageAccess# @robustImageAccess@ indicates whether image
    -- accesses are tightly bounds-checked against the dimensions of the image
    -- view.
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-input-validation Invalid texels>
    -- resulting from out of bounds image loads will be replaced as described
    -- in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-replacement Texel Replacement>,
    -- with either (0,0,1) or (0,0,0) values inserted for missing G, B, or A
    -- components based on the format.
    robustImageAccess :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageRobustnessFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceImageRobustnessFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageRobustnessFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustImageAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageRobustnessFeaturesEXT where
  peekCStruct p = do
    robustImageAccess <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageRobustnessFeaturesEXT
             (bool32ToBool robustImageAccess)

instance Storable PhysicalDeviceImageRobustnessFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageRobustnessFeaturesEXT where
  zero = PhysicalDeviceImageRobustnessFeaturesEXT
           zero


type EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

