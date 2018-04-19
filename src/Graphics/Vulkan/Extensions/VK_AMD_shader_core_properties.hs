{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , VkPhysicalDeviceShaderCorePropertiesAMD(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD = VkStructureType 1000185000
-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_AMD_shader_core_properties"
-- | VkPhysicalDeviceShaderCorePropertiesAMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Members
-- #_members#
--
-- The members of the @VkPhysicalDeviceShaderCorePropertiesAMD@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
-- #_description#
--
-- -   @shaderEngineCount@ is an unsigned integer value indicating the
--     number of shader engines found inside the shader core of the
--     physical device.
--
-- -   @shaderArraysPerEngineCount@ is an unsigned integer value indicating
--     the number of shader arrays inside a shader engine. Each shader
--     array has its own scan converter, set of compute units, and a render
--     back end (color and depth buffers). Shader arrays within a shader
--     engine share shader processor input (wave launcher) and shader
--     export (export buffer) units. Currently, a shader engine can have
--     one or two shader arrays.
--
-- -   @computeUnitsPerShaderArray@ is an unsigned integer value indicating
--     the number of compute units within a shader array. A compute unit
--     houses a set of SIMDs along with a sequencer module and a local data
--     store.
--
-- -   @simdPerComputeUnit@ is an unsigned integer value indicating the
--     number of SIMDs inside a compute unit. Each SIMD processes a single
--     instruction at a time.
--
-- -   @wavefrontSize@ is an unsigned integer value indicating the number
--     of channels (or threads) in a wavefront.
--
-- -   @sgprsPerSimd@ is an unsigned integer value indicating the number of
--     physical Scalar General Purpose Registers (SGPRs) per SIMD.
--
-- -   @minSgprAllocation@ is an unsigned integer value indicating the
--     minimum number of SGPRs allocated for a wave.
--
-- -   @maxSgprAllocation@ is an unsigned integer value indicating the
--     maximum number of SGPRs allocated for a wave.
--
-- -   @sgprAllocationGranularity@ is an unsigned integer value indicating
--     the granularity of SGPR allocation for a wave.
--
-- -   @vgprsPerSimd@ is an unsigned integer value indicating the number of
--     physical Vector General Purpose Registers (VGPRs) per SIMD.
--
-- -   @minVgprAllocation@ is an unsigned integer value indicating the
--     minimum number of VGPRs allocated for a wave.
--
-- -   @maxVgprAllocation@ is an unsigned integer value indicating the
--     maximum number of VGPRs allocated for a wave.
--
-- -   @vgprAllocationGranularity@ is an unsigned integer value indicating
--     the granularity of VGPR allocation for a wave.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD@
--
-- If the @VkPhysicalDeviceShaderCorePropertiesAMD@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceShaderCorePropertiesAMD = VkPhysicalDeviceShaderCorePropertiesAMD
  { -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkShaderEngineCount"
  vkShaderEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkShaderArraysPerEngineCount"
  vkShaderArraysPerEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkComputeUnitsPerShaderArray"
  vkComputeUnitsPerShaderArray :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkSimdPerComputeUnit"
  vkSimdPerComputeUnit :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkWavefrontsPerSimd"
  vkWavefrontsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkWavefrontSize"
  vkWavefrontSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkSgprsPerSimd"
  vkSgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkMinSgprAllocation"
  vkMinSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkMaxSgprAllocation"
  vkMaxSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkSgprAllocationGranularity"
  vkSgprAllocationGranularity :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkVgprsPerSimd"
  vkVgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkMinVgprAllocation"
  vkMinVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkMaxVgprAllocation"
  vkMaxVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vkVgprAllocationGranularity"
  vkVgprAllocationGranularity :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShaderCorePropertiesAMD where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShaderCorePropertiesAMD <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 20)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 28)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 36)
                                                     <*> peek (ptr `plusPtr` 40)
                                                     <*> peek (ptr `plusPtr` 44)
                                                     <*> peek (ptr `plusPtr` 48)
                                                     <*> peek (ptr `plusPtr` 52)
                                                     <*> peek (ptr `plusPtr` 56)
                                                     <*> peek (ptr `plusPtr` 60)
                                                     <*> peek (ptr `plusPtr` 64)
                                                     <*> peek (ptr `plusPtr` 68)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 16) (vkShaderEngineCount (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 20) (vkShaderArraysPerEngineCount (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 24) (vkComputeUnitsPerShaderArray (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 28) (vkSimdPerComputeUnit (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 32) (vkWavefrontsPerSimd (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 36) (vkWavefrontSize (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 40) (vkSgprsPerSimd (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 44) (vkMinSgprAllocation (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 48) (vkMaxSgprAllocation (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 52) (vkSgprAllocationGranularity (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 56) (vkVgprsPerSimd (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 60) (vkMinVgprAllocation (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 64) (vkMaxVgprAllocation (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
                *> poke (ptr `plusPtr` 68) (vkVgprAllocationGranularity (poked :: VkPhysicalDeviceShaderCorePropertiesAMD))
