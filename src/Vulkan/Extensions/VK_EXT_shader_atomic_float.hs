{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_shader_atomic_float"
module Vulkan.Extensions.VK_EXT_shader_atomic_float  ( PhysicalDeviceShaderAtomicFloatFeaturesEXT(..)
                                                     , EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION
                                                     , pattern EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION
                                                     , EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                                                     , pattern EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT"
data PhysicalDeviceShaderAtomicFloatFeaturesEXT = PhysicalDeviceShaderAtomicFloatFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderBufferFloat32Atomics"
    shaderBufferFloat32Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderBufferFloat32AtomicAdd"
    shaderBufferFloat32AtomicAdd :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderBufferFloat64Atomics"
    shaderBufferFloat64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderBufferFloat64AtomicAdd"
    shaderBufferFloat64AtomicAdd :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderSharedFloat32Atomics"
    shaderSharedFloat32Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderSharedFloat32AtomicAdd"
    shaderSharedFloat32AtomicAdd :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderSharedFloat64Atomics"
    shaderSharedFloat64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderSharedFloat64AtomicAdd"
    shaderSharedFloat64AtomicAdd :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderImageFloat32Atomics"
    shaderImageFloat32Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "shaderImageFloat32AtomicAdd"
    shaderImageFloat32AtomicAdd :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "sparseImageFloat32Atomics"
    sparseImageFloat32Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicFloatFeaturesEXT" "sparseImageFloat32AtomicAdd"
    sparseImageFloat32AtomicAdd :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderAtomicFloatFeaturesEXT

instance ToCStruct PhysicalDeviceShaderAtomicFloatFeaturesEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
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

