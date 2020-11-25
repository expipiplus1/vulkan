{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_atomic_int64"
module Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64  ( PhysicalDeviceShaderAtomicInt64Features(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderAtomicInt64Features"
data PhysicalDeviceShaderAtomicInt64Features = PhysicalDeviceShaderAtomicInt64Features
  { -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicInt64Features" "shaderBufferInt64Atomics"
    shaderBufferInt64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderAtomicInt64Features" "shaderSharedInt64Atomics"
    shaderSharedInt64Atomics :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderAtomicInt64Features)
#endif
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

