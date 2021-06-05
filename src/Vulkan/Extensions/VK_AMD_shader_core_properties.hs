{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_core_properties - device extension
--
-- == VK_AMD_shader_core_properties
--
-- [__Name String__]
--     @VK_AMD_shader_core_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     186
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Martin Dinkov
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_shader_core_properties:%20&body=@mdinkov%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Martin Dinkov, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
-- == Description
--
-- This extension exposes shader core properties for a target physical
-- device through the @VK_KHR_get_physical_device_properties2@ extension.
-- Please refer to the example below for proper usage.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCorePropertiesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME'
--
-- -   'AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD'
--
-- == Examples
--
-- This example retrieves the shader core properties for a physical device.
--
-- > extern VkInstance       instance;
-- >
-- > PFN_vkGetPhysicalDeviceProperties2 pfnVkGetPhysicalDeviceProperties2 =
-- >     reinterpret_cast<PFN_vkGetPhysicalDeviceProperties2>
-- >     (vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceProperties2") );
-- >
-- > VkPhysicalDeviceProperties2             general_props;
-- > VkPhysicalDeviceShaderCorePropertiesAMD shader_core_properties;
-- >
-- > shader_core_properties.pNext = nullptr;
-- > shader_core_properties.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD;
-- >
-- > general_props.pNext = &shader_core_properties;
-- > general_props.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2;
-- >
-- > // After this call, shader_core_properties has been populated
-- > pfnVkGetPhysicalDeviceProperties2(device, &general_props);
-- >
-- > printf("Number of shader engines: %d\n",
-- >     m_shader_core_properties.shader_engine_count =
-- >     shader_core_properties.shaderEngineCount;
-- > printf("Number of shader arrays: %d\n",
-- >     m_shader_core_properties.shader_arrays_per_engine_count =
-- >     shader_core_properties.shaderArraysPerEngineCount;
-- > printf("Number of CUs per shader array: %d\n",
-- >     m_shader_core_properties.compute_units_per_shader_array =
-- >     shader_core_properties.computeUnitsPerShaderArray;
-- > printf("Number of SIMDs per compute unit: %d\n",
-- >     m_shader_core_properties.simd_per_compute_unit =
-- >     shader_core_properties.simdPerComputeUnit;
-- > printf("Number of wavefront slots in each SIMD: %d\n",
-- >     m_shader_core_properties.wavefronts_per_simd =
-- >     shader_core_properties.wavefrontsPerSimd;
-- > printf("Number of threads per wavefront: %d\n",
-- >     m_shader_core_properties.wavefront_size =
-- >     shader_core_properties.wavefrontSize;
-- > printf("Number of physical SGPRs per SIMD: %d\n",
-- >     m_shader_core_properties.sgprs_per_simd =
-- >     shader_core_properties.sgprsPerSimd;
-- > printf("Minimum number of SGPRs that can be allocated by a wave: %d\n",
-- >     m_shader_core_properties.min_sgpr_allocation =
-- >     shader_core_properties.minSgprAllocation;
-- > printf("Number of available SGPRs: %d\n",
-- >     m_shader_core_properties.max_sgpr_allocation =
-- >     shader_core_properties.maxSgprAllocation;
-- > printf("SGPRs are allocated in groups of this size: %d\n",
-- >     m_shader_core_properties.sgpr_allocation_granularity =
-- >     shader_core_properties.sgprAllocationGranularity;
-- > printf("Number of physical VGPRs per SIMD: %d\n",
-- >     m_shader_core_properties.vgprs_per_simd =
-- >     shader_core_properties.vgprsPerSimd;
-- > printf("Minimum number of VGPRs that can be allocated by a wave: %d\n",
-- >     m_shader_core_properties.min_vgpr_allocation =
-- >     shader_core_properties.minVgprAllocation;
-- > printf("Number of available VGPRs: %d\n",
-- >     m_shader_core_properties.max_vgpr_allocation =
-- >     shader_core_properties.maxVgprAllocation;
-- > printf("VGPRs are allocated in groups of this size: %d\n",
-- >     m_shader_core_properties.vgpr_allocation_granularity =
-- >     shader_core_properties.vgprAllocationGranularity;
--
-- == Version History
--
-- -   Revision 2, 2019-06-25 (Matthaeus G. Chajdas)
--
--     -   Clarified the meaning of a few fields.
--
-- -   Revision 1, 2018-02-15 (Martin Dinkov)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'PhysicalDeviceShaderCorePropertiesAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_shader_core_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_core_properties  ( PhysicalDeviceShaderCorePropertiesAMD(..)
                                                        , AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
                                                        , AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
                                                        , pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
                                                        ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD))
-- | VkPhysicalDeviceShaderCorePropertiesAMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceShaderCorePropertiesAMD' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- | #limits-shaderEngineCount# @shaderEngineCount@ is an unsigned integer
    -- value indicating the number of shader engines found inside the shader
    -- core of the physical device.
    shaderEngineCount :: Word32
  , -- | #limits-shaderArraysPerEngineCount# @shaderArraysPerEngineCount@ is an
    -- unsigned integer value indicating the number of shader arrays inside a
    -- shader engine. Each shader array has its own scan converter, set of
    -- compute units, and a render back end (color and depth buffers). Shader
    -- arrays within a shader engine share shader processor input (wave
    -- launcher) and shader export (export buffer) units. Currently, a shader
    -- engine can have one or two shader arrays.
    shaderArraysPerEngineCount :: Word32
  , -- | #limits-computeUnitsPerShaderArray# @computeUnitsPerShaderArray@ is an
    -- unsigned integer value indicating the physical number of compute units
    -- within a shader array. The active number of compute units in a shader
    -- array /may/ be lower. A compute unit houses a set of SIMDs along with a
    -- sequencer module and a local data store.
    computeUnitsPerShaderArray :: Word32
  , -- | #limits-simdPerComputeUnit# @simdPerComputeUnit@ is an unsigned integer
    -- value indicating the number of SIMDs inside a compute unit. Each SIMD
    -- processes a single instruction at a time.
    simdPerComputeUnit :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
    wavefrontsPerSimd :: Word32
  , -- | #limits-wavefrontSize# @wavefrontSize@ is an unsigned integer value
    -- indicating the maximum size of a subgroup.
    wavefrontSize :: Word32
  , -- | #limits-sgprsPerSimd# @sgprsPerSimd@ is an unsigned integer value
    -- indicating the number of physical Scalar General Purpose Registers
    -- (SGPRs) per SIMD.
    sgprsPerSimd :: Word32
  , -- | #limits-minSgprAllocation# @minSgprAllocation@ is an unsigned integer
    -- value indicating the minimum number of SGPRs allocated for a wave.
    minSgprAllocation :: Word32
  , -- | #limits-maxSgprAllocation# @maxSgprAllocation@ is an unsigned integer
    -- value indicating the maximum number of SGPRs allocated for a wave.
    maxSgprAllocation :: Word32
  , -- | #limits-sgprAllocationGranularity# @sgprAllocationGranularity@ is an
    -- unsigned integer value indicating the granularity of SGPR allocation for
    -- a wave.
    sgprAllocationGranularity :: Word32
  , -- | #limits-vgprsPerSimd# @vgprsPerSimd@ is an unsigned integer value
    -- indicating the number of physical Vector General Purpose Registers
    -- (VGPRs) per SIMD.
    vgprsPerSimd :: Word32
  , -- | #limits-minVgprAllocation# @minVgprAllocation@ is an unsigned integer
    -- value indicating the minimum number of VGPRs allocated for a wave.
    minVgprAllocation :: Word32
  , -- | #limits-maxVgprAllocation# @maxVgprAllocation@ is an unsigned integer
    -- value indicating the maximum number of VGPRs allocated for a wave.
    maxVgprAllocation :: Word32
  , -- | #limits-vgprAllocationGranularity# @vgprAllocationGranularity@ is an
    -- unsigned integer value indicating the granularity of VGPR allocation for
    -- a wave.
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

