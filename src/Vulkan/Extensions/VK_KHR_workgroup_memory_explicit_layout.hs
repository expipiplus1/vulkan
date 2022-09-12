{-# language CPP #-}
-- | = Name
--
-- VK_KHR_workgroup_memory_explicit_layout - device extension
--
-- == VK_KHR_workgroup_memory_explicit_layout
--
-- [__Name String__]
--     @VK_KHR_workgroup_memory_explicit_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     337
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
--     -   Caio Marcelo de Oliveira Filho
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_workgroup_memory_explicit_layout] @cmarcelo%0A*Here describe the issue or question you have about the VK_KHR_workgroup_memory_explicit_layout extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_workgroup_memory_explicit_layout.html SPV_KHR_workgroup_memory_explicit_layout>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shared_memory_block.txt GL_EXT_shared_memory_block>
--
-- [__Contributors__]
--
--     -   Caio Marcelo de Oliveira Filho, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Graeme Leese, Broadcom
--
--     -   Jason Ekstrand, Intel
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_workgroup_memory_explicit_layout.html SPV_KHR_workgroup_memory_explicit_layout>
-- SPIR-V extension, which allows shaders to explicitly define the layout
-- of @Workgroup@ storage class memory and create aliases between variables
-- from that storage class in a compute shader.
--
-- The aliasing feature allows different “views” on the same data, so the
-- shader can bulk copy data from another storage class using one type
-- (e.g. an array of large vectors), and then use the data with a more
-- specific type. It also enables reducing the amount of workgroup memory
-- consumed by allowing the shader to alias data whose lifetimes do not
-- overlap.
--
-- The explicit layout support and some form of aliasing is also required
-- for layering OpenCL on top of Vulkan.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME'
--
-- -   'KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayoutKHR WorkgroupMemoryExplicitLayoutKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayout8BitAccessKHR WorkgroupMemoryExplicitLayout8BitAccessKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayout16BitAccessKHR WorkgroupMemoryExplicitLayout16BitAccessKHR>
--
-- == Version History
--
-- -   Revision 1, 2020-06-01 (Caio Marcelo de Oliveira Filho)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_workgroup_memory_explicit_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout  ( PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR(..)
                                                                  , KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION
                                                                  , pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION
                                                                  , KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
                                                                  , pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR))
-- | VkPhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR - Structure
-- describing the workgroup storage explicit layout features that can be
-- supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_workgroup_memory_explicit_layout VK_KHR_workgroup_memory_explicit_layout>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
  { -- | #features-workgroupMemoryExplicitLayout# @workgroupMemoryExplicitLayout@
    -- indicates whether the implementation supports the SPIR-V
    -- @WorkgroupMemoryExplicitLayoutKHR@ capability.
    workgroupMemoryExplicitLayout :: Bool
  , -- | #features-workgroupMemoryExplicitLayoutScalarBlockLayout#
    -- @workgroupMemoryExplicitLayoutScalarBlockLayout@ indicates whether the
    -- implementation supports scalar alignment for laying out Workgroup
    -- Blocks.
    workgroupMemoryExplicitLayoutScalarBlockLayout :: Bool
  , -- | #features-workgroupMemoryExplicitLayout8BitAccess#
    -- @workgroupMemoryExplicitLayout8BitAccess@ indicates whether objects in
    -- the @Workgroup@ storage class with the @Block@ decoration /can/ have
    -- 8-bit integer members. If this feature is not enabled, 8-bit integer
    -- members /must/ not be used in such objects. This also indicates whether
    -- shader modules /can/ declare the
    -- @WorkgroupMemoryExplicitLayout8BitAccessKHR@ capability.
    workgroupMemoryExplicitLayout8BitAccess :: Bool
  , -- | #features-workgroupMemoryExplicitLayout16BitAccess#
    -- @workgroupMemoryExplicitLayout16BitAccess@ indicates whether objects in
    -- the @Workgroup@ storage class with the @Block@ decoration /can/ have
    -- 16-bit integer and 16-bit floating-point members. If this feature is not
    -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
    -- used in such objects. This also indicates whether shader modules /can/
    -- declare the @WorkgroupMemoryExplicitLayout16BitAccessKHR@ capability.
    workgroupMemoryExplicitLayout16BitAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR

instance ToCStruct PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (workgroupMemoryExplicitLayout))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (workgroupMemoryExplicitLayoutScalarBlockLayout))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (workgroupMemoryExplicitLayout8BitAccess))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (workgroupMemoryExplicitLayout16BitAccess))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR where
  peekCStruct p = do
    workgroupMemoryExplicitLayout <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    workgroupMemoryExplicitLayoutScalarBlockLayout <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    workgroupMemoryExplicitLayout8BitAccess <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    workgroupMemoryExplicitLayout16BitAccess <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
             (bool32ToBool workgroupMemoryExplicitLayout) (bool32ToBool workgroupMemoryExplicitLayoutScalarBlockLayout) (bool32ToBool workgroupMemoryExplicitLayout8BitAccess) (bool32ToBool workgroupMemoryExplicitLayout16BitAccess)

instance Storable PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR where
  zero = PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
           zero
           zero
           zero
           zero


type KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION"
pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION = 1


type KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME = "VK_KHR_workgroup_memory_explicit_layout"

-- No documentation found for TopLevel "VK_KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME"
pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME = "VK_KHR_workgroup_memory_explicit_layout"

