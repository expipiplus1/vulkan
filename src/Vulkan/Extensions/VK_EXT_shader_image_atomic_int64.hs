{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_shader_image_atomic_int64"
module Vulkan.Extensions.VK_EXT_shader_image_atomic_int64  ( PhysicalDeviceShaderImageAtomicInt64FeaturesEXT(..)
                                                           , EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION
                                                           , pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION
                                                           , EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
                                                           , pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT"
data PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT" "shaderImageInt64Atomics"
    shaderImageInt64Atomics :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderImageAtomicInt64FeaturesEXT" "sparseImageInt64Atomics"
    sparseImageInt64Atomics :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderImageAtomicInt64FeaturesEXT

instance ToCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderImageAtomicInt64FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderImageInt64Atomics))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sparseImageInt64Atomics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  peekCStruct p = do
    shaderImageInt64Atomics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sparseImageInt64Atomics <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
             (bool32ToBool shaderImageInt64Atomics) (bool32ToBool sparseImageInt64Atomics)


instance Storable PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderImageAtomicInt64FeaturesEXT where
  zero = PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
           zero
           zero


type EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION"
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION = 1


type EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME = "VK_EXT_shader_image_atomic_int64"

-- No documentation found for TopLevel "VK_EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME"
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME = "VK_EXT_shader_image_atomic_int64"

