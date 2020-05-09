{-# language CPP #-}
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
-- | VkPhysicalDeviceShaderCorePropertiesAMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderCorePropertiesAMD' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCorePropertiesAMD' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- | @shaderEngineCount@ is an unsigned integer value indicating the number
    -- of shader engines found inside the shader core of the physical device.
    shaderEngineCount :: Word32
  , -- | @shaderArraysPerEngineCount@ is an unsigned integer value indicating the
    -- number of shader arrays inside a shader engine. Each shader array has
    -- its own scan converter, set of compute units, and a render back end
    -- (color and depth buffers). Shader arrays within a shader engine share
    -- shader processor input (wave launcher) and shader export (export buffer)
    -- units. Currently, a shader engine can have one or two shader arrays.
    shaderArraysPerEngineCount :: Word32
  , -- | @computeUnitsPerShaderArray@ is an unsigned integer value indicating the
    -- physical number of compute units within a shader array. The active
    -- number of compute units in a shader array /may/ be lower. A compute unit
    -- houses a set of SIMDs along with a sequencer module and a local data
    -- store.
    computeUnitsPerShaderArray :: Word32
  , -- | @simdPerComputeUnit@ is an unsigned integer value indicating the number
    -- of SIMDs inside a compute unit. Each SIMD processes a single instruction
    -- at a time.
    simdPerComputeUnit :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
    wavefrontsPerSimd :: Word32
  , -- | @wavefrontSize@ is an unsigned integer value indicating the maximum size
    -- of a subgroup.
    wavefrontSize :: Word32
  , -- | @sgprsPerSimd@ is an unsigned integer value indicating the number of
    -- physical Scalar General Purpose Registers (SGPRs) per SIMD.
    sgprsPerSimd :: Word32
  , -- | @minSgprAllocation@ is an unsigned integer value indicating the minimum
    -- number of SGPRs allocated for a wave.
    minSgprAllocation :: Word32
  , -- | @maxSgprAllocation@ is an unsigned integer value indicating the maximum
    -- number of SGPRs allocated for a wave.
    maxSgprAllocation :: Word32
  , -- | @sgprAllocationGranularity@ is an unsigned integer value indicating the
    -- granularity of SGPR allocation for a wave.
    sgprAllocationGranularity :: Word32
  , -- | @vgprsPerSimd@ is an unsigned integer value indicating the number of
    -- physical Vector General Purpose Registers (VGPRs) per SIMD.
    vgprsPerSimd :: Word32
  , -- | @minVgprAllocation@ is an unsigned integer value indicating the minimum
    -- number of VGPRs allocated for a wave.
    minVgprAllocation :: Word32
  , -- | @maxVgprAllocation@ is an unsigned integer value indicating the maximum
    -- number of VGPRs allocated for a wave.
    maxVgprAllocation :: Word32
  , -- | @vgprAllocationGranularity@ is an unsigned integer value indicating the
    -- granularity of VGPR allocation for a wave.
    vgprAllocationGranularity :: Word32
  }
  deriving (Typeable, Eq)
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

