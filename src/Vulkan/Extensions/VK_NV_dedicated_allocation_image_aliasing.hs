{-# language CPP #-}
-- | = Name
--
-- VK_NV_dedicated_allocation_image_aliasing - device extension
--
-- == VK_NV_dedicated_allocation_image_aliasing
--
-- [__Name String__]
--     @VK_NV_dedicated_allocation_image_aliasing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     241
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dedicated_allocation VK_KHR_dedicated_allocation>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Nuno Subtil
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_dedicated_allocation_image_aliasing] @nsubtil%0A*Here describe the issue or question you have about the VK_NV_dedicated_allocation_image_aliasing extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-04
--
-- [__Contributors__]
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Axel Gneiting, id Software
--
-- == Description
--
-- This extension allows applications to alias images on dedicated
-- allocations, subject to specific restrictions: the extent and the number
-- of layers in the image being aliased must be smaller than or equal to
-- those of the original image for which the allocation was created, and
-- every other image parameter must match.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME'
--
-- -   'NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-01-04 (Nuno Subtil)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_dedicated_allocation_image_aliasing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing  ( PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
                                                                    , NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
                                                                    , pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
                                                                    , NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
                                                                    , pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV))
-- | VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV - Structure
-- describing dedicated allocation image aliasing features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_dedicated_allocation_image_aliasing VK_NV_dedicated_allocation_image_aliasing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  { -- | #features-dedicatedAllocationImageAliasing#
    -- @dedicatedAllocationImageAliasing@ indicates that the implementation
    -- supports aliasing of compatible image objects on a dedicated allocation.
    dedicatedAllocationImageAliasing :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV

instance ToCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dedicatedAllocationImageAliasing))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  peekCStruct p = do
    dedicatedAllocationImageAliasing <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
             (bool32ToBool dedicatedAllocationImageAliasing)

instance Storable PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  zero = PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
           zero


type NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION"
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION = 1


type NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = "VK_NV_dedicated_allocation_image_aliasing"

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME"
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME = "VK_NV_dedicated_allocation_image_aliasing"

