{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_long_vector - device extension
--
-- = VK_EXT_shader_long_vector
--
-- [__Name String__]
--     @VK_EXT_shader_long_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     636
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_workgroup_memory_explicit_layout
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_long_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_shader_long_vector extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_long_vector.adoc VK_EXT_shader_long_vector>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_long_vector.txt GL_EXT_long_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
-- == Description
--
-- This extension adds support for using vector types with more than four
-- components in SPIR-V.
--
-- Long vector types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_long_vector.txt GL_EXT_long_vector>
-- GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderLongVectorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderLongVectorPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_LONG_VECTOR_EXTENSION_NAME'
--
-- -   'EXT_SHADER_LONG_VECTOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-LongVectorEXT LongVectorEXT>
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Jeff Bolz)
--
--     -   Initial revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_long_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_long_vector  ( PhysicalDeviceShaderLongVectorFeaturesEXT(..)
                                                    , PhysicalDeviceShaderLongVectorPropertiesEXT(..)
                                                    , EXT_SHADER_LONG_VECTOR_SPEC_VERSION
                                                    , pattern EXT_SHADER_LONG_VECTOR_SPEC_VERSION
                                                    , EXT_SHADER_LONG_VECTOR_EXTENSION_NAME
                                                    , pattern EXT_SHADER_LONG_VECTOR_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT))
-- | VkPhysicalDeviceShaderLongVectorFeaturesEXT - Structure describing long
-- vector features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderLongVectorFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceShaderLongVectorFeaturesEXT', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_long_vector VK_EXT_shader_long_vector>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderLongVectorFeaturesEXT = PhysicalDeviceShaderLongVectorFeaturesEXT
  { -- | #features-longVector# @longVector@ indicates that the implementation
    -- supports the @LongVectorEXT@ SPIR-V capability.
    longVector :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderLongVectorFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderLongVectorFeaturesEXT

instance ToCStruct PhysicalDeviceShaderLongVectorFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderLongVectorFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (longVector))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderLongVectorFeaturesEXT where
  peekCStruct p = do
    longVector <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderLongVectorFeaturesEXT
             (bool32ToBool longVector)

instance Storable PhysicalDeviceShaderLongVectorFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderLongVectorFeaturesEXT where
  zero = PhysicalDeviceShaderLongVectorFeaturesEXT
           zero


-- | VkPhysicalDeviceShaderLongVectorPropertiesEXT - Structure describing
-- long vector properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderLongVectorPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_long_vector VK_EXT_shader_long_vector>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderLongVectorPropertiesEXT = PhysicalDeviceShaderLongVectorPropertiesEXT
  { -- | #limits-maxVectorComponents# @maxVectorComponents@ indicates the maximum
    -- number of components that /can/ be in a vector type.
    maxVectorComponents :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderLongVectorPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceShaderLongVectorPropertiesEXT

instance ToCStruct PhysicalDeviceShaderLongVectorPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderLongVectorPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxVectorComponents)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderLongVectorPropertiesEXT where
  peekCStruct p = do
    maxVectorComponents <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceShaderLongVectorPropertiesEXT
             maxVectorComponents

instance Storable PhysicalDeviceShaderLongVectorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderLongVectorPropertiesEXT where
  zero = PhysicalDeviceShaderLongVectorPropertiesEXT
           zero


type EXT_SHADER_LONG_VECTOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_LONG_VECTOR_SPEC_VERSION"
pattern EXT_SHADER_LONG_VECTOR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_LONG_VECTOR_SPEC_VERSION = 1


type EXT_SHADER_LONG_VECTOR_EXTENSION_NAME = "VK_EXT_shader_long_vector"

-- No documentation found for TopLevel "VK_EXT_SHADER_LONG_VECTOR_EXTENSION_NAME"
pattern EXT_SHADER_LONG_VECTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_LONG_VECTOR_EXTENSION_NAME = "VK_EXT_shader_long_vector"

