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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_dedicated_allocation@
--
-- [__Contact__]
--
--     -   Nuno Subtil
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_dedicated_allocation_image_aliasing:%20&body=@nsubtil%20 >
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
-- = See Also
--
-- 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_dedicated_allocation_image_aliasing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing  ( PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
                                                                    , NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
                                                                    , pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION
                                                                    , NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
                                                                    , pattern NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME
                                                                    ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV))
-- | VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV - Structure
-- describing dedicated allocation image aliasing features that can be
-- supported by an implementation
--
-- = Members
--
-- The members of the
-- 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV' /can/ also be
-- included in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
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
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

