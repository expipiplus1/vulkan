{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_float8 - device extension
--
-- = VK_EXT_shader_float8
--
-- [__Name String__]
--     @VK_EXT_shader_float8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     568
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
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_cooperative_matrix
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_float8.html SPV_EXT_float8>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_float8] @kpet%0A*Here describe the issue or question you have about the VK_EXT_shader_float8 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_float8.adoc VK_EXT_shader_float8>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-04-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Stu Smith, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Craig Graham, Samsung
--
-- == Description
--
-- This extension enables support for 8-bit floating-point data types as
-- defined in SPV_EXT_float8.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderFloat8FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_FLOAT8_EXTENSION_NAME'
--
-- -   'EXT_SHADER_FLOAT8_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_cooperative_vector.ComponentTypeKHR':
--
--     -   'Vulkan.Extensions.VK_NV_cooperative_vector.COMPONENT_TYPE_FLOAT8_E4M3_EXT'
--
--     -   'Vulkan.Extensions.VK_NV_cooperative_vector.COMPONENT_TYPE_FLOAT8_E5M2_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-Float8EXT Float8EXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-Float8CooperativeMatrixEXT Float8CooperativeMatrixEXT>
--
-- == Issues
--
-- 1) Resolve interactions with the changes VK_KHR_shader_float16 makes to
-- rules for denorm flushing (always allowed by default for all FP
-- formats). How to describe the requirement to preserve subnormals?
--
-- + __RESOLVED__: Subnormals are always preserved when converting FP8
-- values to IEEE 754 binary 16. In all other cases, subnormals may be
-- flushed to zero.
--
-- +
--
-- == Version History
--
-- -   Revision 1, 2025-04-16 (Kévin Petit)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_float8 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_float8  ( PhysicalDeviceShaderFloat8FeaturesEXT(..)
                                               , EXT_SHADER_FLOAT8_SPEC_VERSION
                                               , pattern EXT_SHADER_FLOAT8_SPEC_VERSION
                                               , EXT_SHADER_FLOAT8_EXTENSION_NAME
                                               , pattern EXT_SHADER_FLOAT8_EXTENSION_NAME
                                               , ComponentTypeKHR(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT))
import Vulkan.Extensions.VK_NV_cooperative_vector (ComponentTypeKHR(..))
-- | VkPhysicalDeviceShaderFloat8FeaturesEXT - Structure describing float8
-- features that can be supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderFloat8FeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderFloat8FeaturesEXT', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_float8 VK_EXT_shader_float8>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderFloat8FeaturesEXT = PhysicalDeviceShaderFloat8FeaturesEXT
  { -- | #features-shaderFloat8# @shaderFloat8@ indicates whether the
    -- implementation supports shaders with the @Float8EXT@ capability.
    shaderFloat8 :: Bool
  , -- | #features-shaderFloat8CooperativeMatrix# @shaderFloat8CooperativeMatrix@
    -- indicates whether the implementation supports shaders with the
    -- @Float8CooperativeMatrixEXT@ capability.
    shaderFloat8CooperativeMatrix :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderFloat8FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderFloat8FeaturesEXT

instance ToCStruct PhysicalDeviceShaderFloat8FeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderFloat8FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderFloat8))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderFloat8CooperativeMatrix))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT8_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderFloat8FeaturesEXT where
  peekCStruct p = do
    shaderFloat8 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderFloat8CooperativeMatrix <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderFloat8FeaturesEXT
             (bool32ToBool shaderFloat8)
             (bool32ToBool shaderFloat8CooperativeMatrix)

instance Storable PhysicalDeviceShaderFloat8FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderFloat8FeaturesEXT where
  zero = PhysicalDeviceShaderFloat8FeaturesEXT
           zero
           zero


type EXT_SHADER_FLOAT8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_FLOAT8_SPEC_VERSION"
pattern EXT_SHADER_FLOAT8_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_FLOAT8_SPEC_VERSION = 1


type EXT_SHADER_FLOAT8_EXTENSION_NAME = "VK_EXT_shader_float8"

-- No documentation found for TopLevel "VK_EXT_SHADER_FLOAT8_EXTENSION_NAME"
pattern EXT_SHADER_FLOAT8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_FLOAT8_EXTENSION_NAME = "VK_EXT_shader_float8"

