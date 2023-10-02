{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_atomic_float2 - device extension
--
-- == VK_EXT_shader_atomic_float2
--
-- [__Name String__]
--     @VK_EXT_shader_atomic_float2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     274
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_atomic_float VK_EXT_shader_atomic_float>
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_atomic_float2] @gfxstrand%0A*Here describe the issue or question you have about the VK_EXT_shader_atomic_float2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires the VK_EXT_shader_atomic_float
--         extension.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float_min_max.html SPV_EXT_shader_atomic_float_min_max>
--         and
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float16_add.html SPV_EXT_shader_atomic_float16_add>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_atomic_float2.txt GLSL_EXT_shader_atomic_float2>
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Intel
--
-- == Description
--
-- This extension allows a shader to perform 16-bit floating-point atomic
-- operations on buffer and workgroup memory as well as floating-point
-- atomic minimum and maximum operations on buffer, workgroup, and image
-- memory. It advertises the SPIR-V @AtomicFloat16AddEXT@ capability which
-- allows atomic add operations on 16-bit floating-point numbers and the
-- SPIR-V @AtomicFloat16MinMaxEXT@, @AtomicFloat32MinMaxEXT@ and
-- @AtomicFloat64MinMaxEXT@ capabilities which allow atomic minimum and
-- maximum operations on floating-point numbers. The supported operations
-- include @OpAtomicFAddEXT@, @OpAtomicFMinEXT@ and @OpAtomicFMaxEXT@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloat2FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME'
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should this extension add support for 16-bit image atomics?
--
-- __RESOLVED__: No. While Vulkan supports creating storage images with
-- 'Vulkan.Core10.Enums.Format.FORMAT_R16_SFLOAT' and doing load and store
-- on them, the data in the shader has a 32-bit representation. Vulkan
-- currently has no facility for even basic reading or writing such images
-- using 16-bit float values in the shader. Adding such functionality would
-- be required before 16-bit image atomics would make sense and is outside
-- the scope of this extension.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16AddEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16MinMaxEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat32MinMaxEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat64MinMaxEXT AtomicFloat64MinMaxEXT>
--
-- == Version History
--
-- -   Revision 1, 2020-08-14 (Faith Ekstrand)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceShaderAtomicFloat2FeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_atomic_float2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_atomic_float2  ( PhysicalDeviceShaderAtomicFloat2FeaturesEXT(..)
                                                      , EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION
                                                      , pattern EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION
                                                      , EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
                                                      , pattern EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT))
-- | VkPhysicalDeviceShaderAtomicFloat2FeaturesEXT - Structure describing
-- features supported by VK_EXT_shader_atomic_float2
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderAtomicFloat2FeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderAtomicFloat2FeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_atomic_float2 VK_EXT_shader_atomic_float2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAtomicFloat2FeaturesEXT = PhysicalDeviceShaderAtomicFloat2FeaturesEXT
  { -- | #features-shaderBufferFloat16Atomics# @shaderBufferFloat16Atomics@
    -- indicates whether shaders /can/ perform 16-bit floating-point load,
    -- store, and exchange atomic operations on storage buffers.
    shaderBufferFloat16Atomics :: Bool
  , -- | #features-shaderBufferFloat16AtomicAdd# @shaderBufferFloat16AtomicAdd@
    -- indicates whether shaders /can/ perform 16-bit floating-point add atomic
    -- operations on storage buffers.
    shaderBufferFloat16AtomicAdd :: Bool
  , -- | #features-shaderBufferFloat16AtomicMinMax#
    -- @shaderBufferFloat16AtomicMinMax@ indicates whether shaders /can/
    -- perform 16-bit floating-point min and max atomic operations on storage
    -- buffers.
    shaderBufferFloat16AtomicMinMax :: Bool
  , -- | #features-shaderBufferFloat32AtomicMinMax#
    -- @shaderBufferFloat32AtomicMinMax@ indicates whether shaders /can/
    -- perform 32-bit floating-point min and max atomic operations on storage
    -- buffers.
    shaderBufferFloat32AtomicMinMax :: Bool
  , -- | #features-shaderBufferFloat64AtomicMinMax#
    -- @shaderBufferFloat64AtomicMinMax@ indicates whether shaders /can/
    -- perform 64-bit floating-point min and max atomic operations on storage
    -- buffers.
    shaderBufferFloat64AtomicMinMax :: Bool
  , -- | #features-shaderSharedFloat16Atomics# @shaderSharedFloat16Atomics@
    -- indicates whether shaders /can/ perform 16-bit floating-point load,
    -- store and exchange atomic operations on shared and payload memory.
    shaderSharedFloat16Atomics :: Bool
  , -- | #features-shaderSharedFloat16AtomicAdd# @shaderSharedFloat16AtomicAdd@
    -- indicates whether shaders /can/ perform 16-bit floating-point add atomic
    -- operations on shared and payload memory.
    shaderSharedFloat16AtomicAdd :: Bool
  , -- | #features-shaderSharedFloat16AtomicMinMax#
    -- @shaderSharedFloat16AtomicMinMax@ indicates whether shaders /can/
    -- perform 16-bit floating-point min and max atomic operations on shared
    -- and payload memory.
    shaderSharedFloat16AtomicMinMax :: Bool
  , -- | #features-shaderSharedFloat32AtomicMinMax#
    -- @shaderSharedFloat32AtomicMinMax@ indicates whether shaders /can/
    -- perform 32-bit floating-point min and max atomic operations on shared
    -- and payload memory.
    shaderSharedFloat32AtomicMinMax :: Bool
  , -- | #features-shaderSharedFloat64AtomicMinMax#
    -- @shaderSharedFloat64AtomicMinMax@ indicates whether shaders /can/
    -- perform 64-bit floating-point min and max atomic operations on shared
    -- and payload memory.
    shaderSharedFloat64AtomicMinMax :: Bool
  , -- | #features-shaderImageFloat32AtomicMinMax#
    -- @shaderImageFloat32AtomicMinMax@ indicates whether shaders /can/ perform
    -- 32-bit floating-point min and max atomic image operations.
    shaderImageFloat32AtomicMinMax :: Bool
  , -- | #features-sparseImageFloat32AtomicMinMax#
    -- @sparseImageFloat32AtomicMinMax@ indicates whether 32-bit floating-point
    -- min and max atomic operations /can/ be used on sparse images.
    sparseImageFloat32AtomicMinMax :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAtomicFloat2FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderAtomicFloat2FeaturesEXT

instance ToCStruct PhysicalDeviceShaderAtomicFloat2FeaturesEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAtomicFloat2FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat16Atomics))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat16AtomicAdd))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat16AtomicMinMax))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat32AtomicMinMax))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shaderBufferFloat64AtomicMinMax))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat16Atomics))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat16AtomicAdd))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat16AtomicMinMax))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat32AtomicMinMax))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (shaderSharedFloat64AtomicMinMax))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (shaderImageFloat32AtomicMinMax))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (sparseImageFloat32AtomicMinMax))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT)
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

instance FromCStruct PhysicalDeviceShaderAtomicFloat2FeaturesEXT where
  peekCStruct p = do
    shaderBufferFloat16Atomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderBufferFloat16AtomicAdd <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    shaderBufferFloat16AtomicMinMax <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    shaderBufferFloat32AtomicMinMax <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    shaderBufferFloat64AtomicMinMax <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    shaderSharedFloat16Atomics <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    shaderSharedFloat16AtomicAdd <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    shaderSharedFloat16AtomicMinMax <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    shaderSharedFloat32AtomicMinMax <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    shaderSharedFloat64AtomicMinMax <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    shaderImageFloat32AtomicMinMax <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    sparseImageFloat32AtomicMinMax <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderAtomicFloat2FeaturesEXT
             (bool32ToBool shaderBufferFloat16Atomics)
             (bool32ToBool shaderBufferFloat16AtomicAdd)
             (bool32ToBool shaderBufferFloat16AtomicMinMax)
             (bool32ToBool shaderBufferFloat32AtomicMinMax)
             (bool32ToBool shaderBufferFloat64AtomicMinMax)
             (bool32ToBool shaderSharedFloat16Atomics)
             (bool32ToBool shaderSharedFloat16AtomicAdd)
             (bool32ToBool shaderSharedFloat16AtomicMinMax)
             (bool32ToBool shaderSharedFloat32AtomicMinMax)
             (bool32ToBool shaderSharedFloat64AtomicMinMax)
             (bool32ToBool shaderImageFloat32AtomicMinMax)
             (bool32ToBool sparseImageFloat32AtomicMinMax)

instance Storable PhysicalDeviceShaderAtomicFloat2FeaturesEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAtomicFloat2FeaturesEXT where
  zero = PhysicalDeviceShaderAtomicFloat2FeaturesEXT
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


type EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION"
pattern EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION = 1


type EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME = "VK_EXT_shader_atomic_float2"

-- No documentation found for TopLevel "VK_EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME"
pattern EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME = "VK_EXT_shader_atomic_float2"

