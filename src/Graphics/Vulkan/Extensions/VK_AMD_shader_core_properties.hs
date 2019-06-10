{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceShaderCorePropertiesAMD(..)
  , 
#endif
  pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShaderCorePropertiesAMD"
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "shaderEngineCount"
  shaderEngineCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "shaderArraysPerEngineCount"
  shaderArraysPerEngineCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "computeUnitsPerShaderArray"
  computeUnitsPerShaderArray :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "simdPerComputeUnit"
  simdPerComputeUnit :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
  wavefrontsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "wavefrontSize"
  wavefrontSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "sgprsPerSimd"
  sgprsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "minSgprAllocation"
  minSgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "maxSgprAllocation"
  maxSgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "sgprAllocationGranularity"
  sgprAllocationGranularity :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "vgprsPerSimd"
  vgprsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "minVgprAllocation"
  minVgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "maxVgprAllocation"
  maxVgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "vgprAllocationGranularity"
  vgprAllocationGranularity :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShaderCorePropertiesAMD where
  zero = PhysicalDeviceShaderCorePropertiesAMD Nothing
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

#endif

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
