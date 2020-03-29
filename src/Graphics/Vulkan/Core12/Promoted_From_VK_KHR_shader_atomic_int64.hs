{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64  ( PhysicalDeviceShaderAtomicInt64Features(..)
                                                                        , StructureType(..)
                                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceShaderAtomicInt64Features - Structure describing
-- features supported by VK_KHR_shader_atomic_int64
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderAtomicInt64Features = PhysicalDeviceShaderAtomicInt64Features
  { -- | @shaderBufferInt64Atomics@ indicates whether shaders /can/ support
    -- 64-bit unsigned and signed integer atomic operations on buffers.
    shaderBufferInt64Atomics :: Bool
  , -- | @shaderSharedInt64Atomics@ indicates whether shaders /can/ support
    -- 64-bit unsigned and signed integer atomic operations on shared memory.
    shaderSharedInt64Atomics :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceShaderAtomicInt64Features

instance ToCStruct PhysicalDeviceShaderAtomicInt64Features where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderAtomicInt64Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderBufferInt64Atomics))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderSharedInt64Atomics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderAtomicInt64Features where
  peekCStruct p = do
    shaderBufferInt64Atomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderSharedInt64Atomics <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderAtomicInt64Features
             (bool32ToBool shaderBufferInt64Atomics) (bool32ToBool shaderSharedInt64Atomics)

instance Storable PhysicalDeviceShaderAtomicInt64Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderAtomicInt64Features where
  zero = PhysicalDeviceShaderAtomicInt64Features
           zero
           zero

