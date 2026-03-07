{-# language CPP #-}
-- | = Name
--
-- VK_KHR_depth_clamp_zero_one - device extension
--
-- = VK_KHR_depth_clamp_zero_one
--
-- [__Name String__]
--     @VK_KHR_depth_clamp_zero_one@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     605
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_depth_clamp_zero_one] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_depth_clamp_zero_one extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-09-10
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension is based on the @VK_EXT_depth_clamp_zero_one@ extension.
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
--     -   'PhysicalDeviceDepthClampZeroOneFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME'
--
-- -   'KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-09-10 (Graeme Leese)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_depth_clamp_zero_one Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_depth_clamp_zero_one  ( PhysicalDeviceDepthClampZeroOneFeaturesKHR(..)
                                                      , KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , pattern KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
                                                      , pattern KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR))
-- | VkPhysicalDeviceDepthClampZeroOneFeaturesKHR - Structure describing
-- feature to control zero to one depth clamping
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDepthClampZeroOneFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDepthClampZeroOneFeaturesKHR', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_clamp_zero_one VK_KHR_depth_clamp_zero_one>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthClampZeroOneFeaturesKHR = PhysicalDeviceDepthClampZeroOneFeaturesKHR
  { -- | #features-depthClampZeroOne# @depthClampZeroOne@ indicates that the
    -- implementation supports clamping the depth to a range of @0@ to @1@.
    depthClampZeroOne :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClampZeroOneFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceDepthClampZeroOneFeaturesKHR

instance ToCStruct PhysicalDeviceDepthClampZeroOneFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClampZeroOneFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClampZeroOne))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClampZeroOneFeaturesKHR where
  peekCStruct p = do
    depthClampZeroOne <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClampZeroOneFeaturesKHR
             (bool32ToBool depthClampZeroOne)

instance Storable PhysicalDeviceDepthClampZeroOneFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClampZeroOneFeaturesKHR where
  zero = PhysicalDeviceDepthClampZeroOneFeaturesKHR
           zero


type KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION"
pattern KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1


type KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_KHR_depth_clamp_zero_one"

-- No documentation found for TopLevel "VK_KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME"
pattern KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_KHR_depth_clamp_zero_one"

