{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_shader_float16_int8"
module Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8  ( PhysicalDeviceShaderFloat16Int8Features(..)
                                                               , StructureType(..)
                                                               ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceShaderFloat16Int8Features - Structure describing
-- features supported by VK_KHR_shader_float16_int8
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderFloat16Int8Features' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderFloat16Int8Features' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float16_int8 VK_KHR_shader_float16_int8>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderFloat16Int8Features = PhysicalDeviceShaderFloat16Int8Features
  { -- | #extension-features-shaderFloat16# @shaderFloat16@ indicates whether
    -- 16-bit floats (halfs) are supported in shader code. This also indicates
    -- whether shader modules /can/ declare the @Float16@ capability. However,
    -- this only enables a subset of the storage classes that SPIR-V allows for
    -- the @Float16@ SPIR-V capability: Declaring and using 16-bit floats in
    -- the @Private@, @Workgroup@ (for non-Block variables), and @Function@
    -- storage classes is enabled, while declaring them in the interface
    -- storage classes (e.g., @UniformConstant@, @Uniform@, @StorageBuffer@,
    -- @Input@, @Output@, and @PushConstant@) is not enabled.
    shaderFloat16 :: Bool
  , -- | #extension-features-shaderInt8# @shaderInt8@ indicates whether 8-bit
    -- integers (signed and unsigned) are supported in shader code. This also
    -- indicates whether shader modules /can/ declare the @Int8@ capability.
    -- However, this only enables a subset of the storage classes that SPIR-V
    -- allows for the @Int8@ SPIR-V capability: Declaring and using 8-bit
    -- integers in the @Private@, @Workgroup@ (for non-Block variables), and
    -- @Function@ storage classes is enabled, while declaring them in the
    -- interface storage classes (e.g., @UniformConstant@, @Uniform@,
    -- @StorageBuffer@, @Input@, @Output@, and @PushConstant@) is not enabled.
    shaderInt8 :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderFloat16Int8Features)
#endif
deriving instance Show PhysicalDeviceShaderFloat16Int8Features

instance ToCStruct PhysicalDeviceShaderFloat16Int8Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

