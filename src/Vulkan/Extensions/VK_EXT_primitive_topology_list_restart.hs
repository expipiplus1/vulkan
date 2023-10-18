{-# language CPP #-}
-- | = Name
--
-- VK_EXT_primitive_topology_list_restart - device extension
--
-- == VK_EXT_primitive_topology_list_restart
--
-- [__Name String__]
--     @VK_EXT_primitive_topology_list_restart@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     357
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_primitive_topology_list_restart] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_primitive_topology_list_restart extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-01-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Shahbaz Youssefi, Google
--
-- == Description
--
-- This extension allows list primitives to use the primitive restart index
-- value. This provides a more efficient implementation when layering
-- OpenGL functionality on Vulkan by avoiding emulation which incurs data
-- copies.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME'
--
-- -   'EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2020-09-14 (Courtney Goeltzenleuchter)
--
--     -   Internal revisions
--
-- -   Revision 1, 2021-01-11 (Shahbaz Youssefi)
--
--     -   Add the @primitiveTopologyPatchListRestart@ feature
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_primitive_topology_list_restart Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_primitive_topology_list_restart  ( PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT(..)
                                                                 , EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION
                                                                 , pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION
                                                                 , EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME
                                                                 , pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT))
-- | VkPhysicalDevicePrimitiveTopologyListRestartFeaturesEXT - Structure
-- describing whether list type primitives can support primitive restart
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_primitive_topology_list_restart VK_EXT_primitive_topology_list_restart>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT = PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT
  { -- | #features-primitiveTopologyListRestart# @primitiveTopologyListRestart@
    -- indicates that list type primitives,
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST',
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY'
    -- and
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY',
    -- /can/ use the primitive restart index value in index buffers.
    primitiveTopologyListRestart :: Bool
  , -- | #features-primitiveTopologyPatchListRestart#
    -- @primitiveTopologyPatchListRestart@ indicates that the
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
    -- topology /can/ use the primitive restart index value in index buffers.
    primitiveTopologyPatchListRestart :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT

instance ToCStruct PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (primitiveTopologyListRestart))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (primitiveTopologyPatchListRestart))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT where
  peekCStruct p = do
    primitiveTopologyListRestart <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    primitiveTopologyPatchListRestart <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT
             (bool32ToBool primitiveTopologyListRestart)
             (bool32ToBool primitiveTopologyPatchListRestart)

instance Storable PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT where
  zero = PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT
           zero
           zero


type EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION"
pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION = 1


type EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME = "VK_EXT_primitive_topology_list_restart"

-- No documentation found for TopLevel "VK_EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME"
pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME = "VK_EXT_primitive_topology_list_restart"

