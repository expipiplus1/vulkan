{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_subgroup_partitioned - device extension
--
-- = VK_EXT_shader_subgroup_partitioned
--
-- [__Name String__]
--     @VK_EXT_shader_subgroup_partitioned@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     663
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
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_subgroup_partitioned.html SPV_EXT_shader_subgroup_partitioned>
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_subgroup_partitioned.html SPV_NV_shader_subgroup_partitioned>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_subgroup_partitioned] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_shader_subgroup_partitioned extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-11-12
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GL_NV_shader_subgroup_partitioned.txt GL_NV_shader_subgroup_partitioned>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension enables support for a new class of
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-group-operations group operations>
-- on
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-scope-subgroup subgroups>
-- via the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GL_NV_shader_subgroup_partitioned.txt GL_NV_shader_subgroup_partitioned>
-- GLSL extension and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_subgroup_partitioned.html SPV_EXT_shader_subgroup_partitioned>
-- SPIR-V extension. Support for these new operations is advertised via the
-- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_PARTITIONED_BIT_EXT'
-- bit. Note that there is not a new GLSL extension since
-- @SPV_EXT_shader_subgroup_partitioned@ is compatible with
-- @SPV_NV_shader_subgroup_partitioned@, so the
-- @GL_NV_shader_subgroup_partitioned@ GLSL extension can still be used.
--
-- This extension requires Vulkan 1.1, for general subgroup support.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME'
--
-- -   'EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_PARTITIONED_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits':
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_PARTITIONED_BIT_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-11-12 (Jeff Bolz)
--
--     -   Promoted from @VK_NV_shader_subgroup_partitioned@
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_subgroup_partitioned Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_subgroup_partitioned  ( PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT(..)
                                                             , EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                             , pattern EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                             , EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                                                             , pattern EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_PARTITIONED_FEATURES_EXT))
-- | VkPhysicalDeviceShaderSubgroupPartitionedFeaturesEXT - Structure
-- describing shader subgroup partitioned features that can be supported by
-- an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT', it /must/ add an
-- instance of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_subgroup_partitioned VK_EXT_shader_subgroup_partitioned>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT = PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT
  { -- | #features-shaderSubgroupPartitioned# @shaderSubgroupPartitioned@
    -- indicates that the implementation supports
    -- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_PARTITIONED_BIT_EXT'.
    shaderSubgroupPartitioned :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT

instance ToCStruct PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_PARTITIONED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderSubgroupPartitioned))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_PARTITIONED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT where
  peekCStruct p = do
    shaderSubgroupPartitioned <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT
             (bool32ToBool shaderSubgroupPartitioned)

instance Storable PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT where
  zero = PhysicalDeviceShaderSubgroupPartitionedFeaturesEXT
           zero


type EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION"
pattern EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1


type EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_EXT_shader_subgroup_partitioned"

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME"
pattern EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_EXT_shader_subgroup_partitioned"

