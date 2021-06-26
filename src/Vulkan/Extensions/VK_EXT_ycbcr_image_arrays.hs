{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ycbcr_image_arrays - device extension
--
-- == VK_EXT_ycbcr_image_arrays
--
-- [__Name String__]
--     @VK_EXT_ycbcr_image_arrays@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     253
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_ycbcr_image_arrays:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-15
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows images of a format that requires
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
-- to be created with multiple array layers, which is otherwise restricted.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME'
--
-- -   'EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-01-15 (Piers Daniell)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ycbcr_image_arrays Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ycbcr_image_arrays  ( PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
                                                    , EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
                                                    , pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
                                                    , EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
                                                    , pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT))
-- | VkPhysicalDeviceYcbcrImageArraysFeaturesEXT - Structure describing
-- extended Y′CBCR image creation features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceYcbcrImageArraysFeaturesEXT = PhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- | #features-ycbcrImageArrays# @ycbcrImageArrays@ indicates that the
    -- implementation supports creating images with a format that requires
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
    -- and has multiple array layers.
    ycbcrImageArrays :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceYcbcrImageArraysFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (ycbcrImageArrays))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  peekCStruct p = do
    ycbcrImageArrays <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceYcbcrImageArraysFeaturesEXT
             (bool32ToBool ycbcrImageArrays)

instance Storable PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  zero = PhysicalDeviceYcbcrImageArraysFeaturesEXT
           zero


type EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1


type EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"

