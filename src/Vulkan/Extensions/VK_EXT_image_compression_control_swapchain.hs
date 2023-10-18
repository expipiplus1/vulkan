{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_compression_control_swapchain - device extension
--
-- == VK_EXT_image_compression_control_swapchain
--
-- [__Name String__]
--     @VK_EXT_image_compression_control_swapchain@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     438
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_compression_control_swapchain] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_image_compression_control_swapchain extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Andrew Garrard, Imagination
--
--     -   Lisa Wu, Arm
--
--     -   Peter Kohaut, Arm
--
--     -   Ian Elliott, Google
--
-- == Description
--
-- This extension enables fixed-rate image compression and adds the ability
-- to control when this kind of compression can be applied to swapchain
-- images.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-05-02 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_compression_control_swapchain Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_compression_control_swapchain  ( PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT(..)
                                                                     , EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION
                                                                     , pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION
                                                                     , EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME
                                                                     , pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT))
-- | VkPhysicalDeviceImageCompressionControlSwapchainFeaturesEXT - Structure
-- describing whether per-swapchain image compression controls can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control_swapchain VK_EXT_image_compression_control_swapchain>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT = PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT
  { -- | #features-imageCompressionControlSwapchain#
    -- @imageCompressionControlSwapchain@ indicates that the implementation
    -- supports controlling image controls per swapchain and querying image
    -- compression properties per surface.
    imageCompressionControlSwapchain :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT

instance ToCStruct PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageCompressionControlSwapchain))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT where
  peekCStruct p = do
    imageCompressionControlSwapchain <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT
             (bool32ToBool imageCompressionControlSwapchain)

instance Storable PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT where
  zero = PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT
           zero


type EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION"
pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION = 1


type EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME = "VK_EXT_image_compression_control_swapchain"

-- No documentation found for TopLevel "VK_EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME"
pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME = "VK_EXT_image_compression_control_swapchain"

