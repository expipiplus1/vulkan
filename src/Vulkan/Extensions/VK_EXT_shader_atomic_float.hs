{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_atomic_float - device extension
--
-- == VK_EXT_shader_atomic_float
--
-- [__Name String__]
--     @VK_EXT_shader_atomic_float@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     261
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_atomic_float] @vkushwaha-nv%0A<<Here describe the issue or question you have about the VK_EXT_shader_atomic_float extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float_add.html SPV_EXT_shader_atomic_float_add>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_atomic_float.txt GL_EXT_shader_atomic_float>
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows a shader to contain floating-point atomic
-- operations on buffer, workgroup, and image memory. It also advertises
-- the SPIR-V @AtomicFloat32AddEXT@ and @AtomicFloat64AddEXT@ capabilities
-- that allows atomic addition on floating-points numbers. The supported
-- operations include @OpAtomicFAddEXT@, @OpAtomicExchange@, @OpAtomicLoad@
-- and @OpAtomicStore@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloatFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME'
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat32AddEXT AtomicFloat32AddEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat64AddEXT AtomicFloat64AddEXT>
--
-- == Version History
--
-- -   Revision 1, 2020-07-15 (Vikram Kushwaha)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceShaderAtomicFloatFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_atomic_float Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_atomic_float  ( PhysicalDeviceShaderAtomicFloatFeaturesEXT(..)
                                                     , EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION
                                                     , pattern EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION
                                                     , EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                                                     , pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT))
-- | VkPhysicalDeviceShaderAtomicFloatFeaturesEXT - Structure describing
-- features supported by VK_EXT_shader_atomic_float
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderAtomicFloatFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderAtomicFloatFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_atomic_float VK_EXT_shader_atomic_float>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAtomicFloatFeaturesEXT = PhysicalDeviceShaderAtomicFloatFeaturesEXT
  { -- | #features-shaderBufferFloat32Atomics# @shaderBufferFloat32Atomics@
    -- indicates whether shaders /can/ perform 32-bit floating-point load,
    -- store and exchange atomic operations on storage buffers.
    shaderBufferFloat32Atomics :: Bool
  , -- | #features-shaderBufferFloat32AtomicAdd# @shaderBufferFloat32AtomicAdd@
    -- indicates whether shaders /can/ perform 32-bit floating-point add atomic
    -- operations on storage buffers.
    shaderBufferFloat32AtomicAdd :: Bool
  , -- | #features-shaderBufferFloat64Atomics# @shaderBufferFloat64Atomics@
    -- indicates whether shaders /can/ perform 64-bit floating-point load,
    -- store and exchange atomic operations on storage buffers.
    shaderBufferFloat64Atomics :: Bool
  , -- | #features-shaderBufferFloat64AtomicAdd# @shaderBufferFloat64AtomicAdd@
    -- indicates whether shaders /can/ perform 64-bit floating-point add atomic
    -- operations on storage buffers.
    shaderBufferFloat64AtomicAdd :: Bool
  , -- | #features-shaderSharedFloat32Atomics# @shaderSharedFloat32Atomics@
    -- indicates whether shaders /can/ perform 32-bit floating-point load,
    -- store and exchange atomic operations on shared memory.
    shaderSharedFloat32Atomics :: Bool
  , -- | #features-shaderSharedFloat32AtomicAdd# @shaderSharedFloat32AtomicAdd@
    -- indicates whether shaders /can/ perform 32-bit floating-point add atomic
    -- operations on shared memory.
    shaderSharedFloat32AtomicAdd :: Bool
  , -- | #features-shaderSharedFloat64Atomics# @shaderSharedFloat64Atomics@
    -- indicates whether shaders /can/ perform 64-bit floating-point load,
    -- store and exchange atomic operations on shared memory.
    shaderSharedFloat64Atomics :: Bool
  , -- | #features-shaderSharedFloat64AtomicAdd# @shaderSharedFloat64AtomicAdd@
    -- indicates whether shaders /can/ perform 64-bit floating-point add atomic
    -- operations on shared memory.
    shaderSharedFloat64AtomicAdd :: Bool
  , -- | #features-shaderImageFloat32Atomics# @shaderImageFloat32Atomics@
    -- indicates whether shaders /can/ perform 32-bit floating-point load,
    -- store and exchange atomic image operations.
    shaderImageFloat32Atomics :: Bool
  , -- | #features-shaderImageFloat32AtomicAdd# @shaderImageFloat32AtomicAdd@
    -- indicates whether shaders /can/ perform 32-bit floating-point add atomic
    -- image operations.
    shaderImageFloat32AtomicAdd :: Bool
  , -- | #features-sparseImageFloat32Atomics# @sparseImageFloat32Atomics@
    -- indicates whether 32-bit floating-point load, store and exchange atomic
    -- operations /can/ be used on sparse images.
    sparseImageFloat32Atomics :: Bool
  , -- | #features-sparseImageFloat32AtomicAdd# @sparseImageFloat32AtomicAdd@
    -- indicates whether 32-bit floating-point add atomic operations /can/ be
    -- used on sparse images.
    sparseImageFloat32AtomicAdd :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderAtomicFloatFeaturesEXT

instance ToCStruct PhysicalDeviceShaderAtomicFloatFeaturesEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAtomicFloatFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat32Atomics))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat32AtomicAdd))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat64Atomics))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat64AtomicAdd))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat32Atomics))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat32AtomicAdd))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat64Atomics))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat64AtomicAdd))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderImageFloat32Atomics))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (shaderImageFloat32AtomicAdd))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (sparseImageFloat32Atomics))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (sparseImageFloat32AtomicAdd))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderAtomicFloatFeaturesEXT where
  peekCStruct p = do
    shaderBufferFloat32Atomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderBufferFloat32AtomicAdd <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderBufferFloat64Atomics <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderBufferFloat64AtomicAdd <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderSharedFloat32Atomics <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderSharedFloat32AtomicAdd <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderSharedFloat64Atomics <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderSharedFloat64AtomicAdd <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderImageFloat32Atomics <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    shaderImageFloat32AtomicAdd <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    sparseImageFloat32Atomics <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    sparseImageFloat32AtomicAdd <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderAtomicFloatFeaturesEXT
             (bool32ToBool shaderBufferFloat32Atomics) (bool32ToBool shaderBufferFloat32AtomicAdd) (bool32ToBool shaderBufferFloat64Atomics) (bool32ToBool shaderBufferFloat64AtomicAdd) (bool32ToBool shaderSharedFloat32Atomics) (bool32ToBool shaderSharedFloat32AtomicAdd) (bool32ToBool shaderSharedFloat64Atomics) (bool32ToBool shaderSharedFloat64AtomicAdd) (bool32ToBool shaderImageFloat32Atomics) (bool32ToBool shaderImageFloat32AtomicAdd) (bool32ToBool sparseImageFloat32Atomics) (bool32ToBool sparseImageFloat32AtomicAdd)

instance Storable PhysicalDeviceShaderAtomicFloatFeaturesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAtomicFloatFeaturesEXT where
  zero = PhysicalDeviceShaderAtomicFloatFeaturesEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


type EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION"
pattern EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION = 1


type EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME = "VK_EXT_shader_atomic_float"

-- No documentation found for TopLevel "VK_EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME"
pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME = "VK_EXT_shader_atomic_float"

