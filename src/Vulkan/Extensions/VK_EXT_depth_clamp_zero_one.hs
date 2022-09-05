{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clamp_zero_one - device extension
--
-- == VK_EXT_depth_clamp_zero_one
--
-- [__Name String__]
--     @VK_EXT_depth_clamp_zero_one@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     422
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clamp_zero_one] @gnl21%0A<<Here describe the issue or question you have about the VK_EXT_depth_clamp_zero_one extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-07-29
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension gives defined behavior to fragment depth values which end
-- up outside the conventional [0, 1] range. It can be used to ensure
-- portability in edge cases of features like depthBias. The particular
-- behavior is chosen to match OpenGL to aid porting or emulation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClampZeroOneFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-07-29 (Graeme Leese)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDepthClampZeroOneFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clamp_zero_one  ( PhysicalDeviceDepthClampZeroOneFeaturesEXT(..)
                                                      , EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
                                                      , pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT))
-- | VkPhysicalDeviceDepthClampZeroOneFeaturesEXT - Structure describing
-- feature to control zero to one depth clamping
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDepthClampZeroOneFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDepthClampZeroOneFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthClampZeroOneFeaturesEXT = PhysicalDeviceDepthClampZeroOneFeaturesEXT
  { -- | #features-depthClampZeroOne# @depthClampZeroOne@ indicates that the
    -- implementation supports clamping the depth to a range of @0@ to @1@.
    depthClampZeroOne :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClampZeroOneFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthClampZeroOneFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClampZeroOneFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClampZeroOneFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClampZeroOne))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClampZeroOneFeaturesEXT where
  peekCStruct p = do
    depthClampZeroOne <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClampZeroOneFeaturesEXT
             (bool32ToBool depthClampZeroOne)

instance Storable PhysicalDeviceDepthClampZeroOneFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClampZeroOneFeaturesEXT where
  zero = PhysicalDeviceDepthClampZeroOneFeaturesEXT
           zero


type EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION"
pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1


type EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_EXT_depth_clamp_zero_one"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME"
pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_EXT_depth_clamp_zero_one"

