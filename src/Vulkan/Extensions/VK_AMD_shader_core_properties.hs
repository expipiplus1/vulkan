{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_shader_core_properties"
module Vulkan.Extensions.VK_AMD_shader_core_properties  ( PhysicalDeviceShaderCorePropertiesAMD(..)
                                                        , AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
                                                        , pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderCorePropertiesAMD"
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "shaderEngineCount"
    shaderEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "shaderArraysPerEngineCount"
    shaderArraysPerEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "computeUnitsPerShaderArray"
    computeUnitsPerShaderArray :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "simdPerComputeUnit"
    simdPerComputeUnit :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
    wavefrontsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontSize"
    wavefrontSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "sgprsPerSimd"
    sgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "minSgprAllocation"
    minSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "maxSgprAllocation"
    maxSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "sgprAllocationGranularity"
    sgprAllocationGranularity :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vgprsPerSimd"
    vgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "minVgprAllocation"
    minVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "maxVgprAllocation"
    maxVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vgprAllocationGranularity"
    vgprAllocationGranularity :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderCorePropertiesAMD)
#endif
deriving instance Show PhysicalDeviceShaderCorePropertiesAMD

instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderCorePropertiesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderEngineCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (shaderArraysPerEngineCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (computeUnitsPerShaderArray)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (simdPerComputeUnit)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (wavefrontsPerSimd)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (wavefrontSize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (sgprsPerSimd)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (minSgprAllocation)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (maxSgprAllocation)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (sgprAllocationGranularity)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (vgprsPerSimd)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (minVgprAllocation)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxVgprAllocation)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (vgprAllocationGranularity)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceShaderCorePropertiesAMD where
  peekCStruct p = do
    shaderEngineCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    shaderArraysPerEngineCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    computeUnitsPerShaderArray <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    simdPerComputeUnit <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    wavefrontsPerSimd <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    wavefrontSize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    sgprsPerSimd <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    minSgprAllocation <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    maxSgprAllocation <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    sgprAllocationGranularity <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    vgprsPerSimd <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    minVgprAllocation <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxVgprAllocation <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    vgprAllocationGranularity <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    pure $ PhysicalDeviceShaderCorePropertiesAMD
             shaderEngineCount shaderArraysPerEngineCount computeUnitsPerShaderArray simdPerComputeUnit wavefrontsPerSimd wavefrontSize sgprsPerSimd minSgprAllocation maxSgprAllocation sgprAllocationGranularity vgprsPerSimd minVgprAllocation maxVgprAllocation vgprAllocationGranularity


instance Storable PhysicalDeviceShaderCorePropertiesAMD where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderCorePropertiesAMD where
  zero = PhysicalDeviceShaderCorePropertiesAMD
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
           zero
           zero


type AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 2


type AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_AMD_shader_core_properties"

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_AMD_shader_core_properties"

