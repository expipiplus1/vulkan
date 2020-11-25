{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_float16_int8"
module Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8  ( PhysicalDeviceShaderFloat16Int8Features(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderFloat16Int8Features"
data PhysicalDeviceShaderFloat16Int8Features = PhysicalDeviceShaderFloat16Int8Features
  { -- No documentation found for Nested "VkPhysicalDeviceShaderFloat16Int8Features" "shaderFloat16"
    shaderFloat16 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceShaderFloat16Int8Features" "shaderInt8"
    shaderInt8 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderFloat16Int8Features)
#endif
deriving instance Show PhysicalDeviceShaderFloat16Int8Features

instance ToCStruct PhysicalDeviceShaderFloat16Int8Features where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderFloat16Int8Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderFloat16))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (shaderInt8))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderFloat16Int8Features where
  peekCStruct p = do
    shaderFloat16 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    shaderInt8 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderFloat16Int8Features
             (bool32ToBool shaderFloat16) (bool32ToBool shaderInt8)


instance Storable PhysicalDeviceShaderFloat16Int8Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderFloat16Int8Features where
  zero = PhysicalDeviceShaderFloat16Int8Features
           zero
           zero

