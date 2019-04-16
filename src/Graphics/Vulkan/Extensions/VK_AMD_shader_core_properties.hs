{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( withCStructPhysicalDeviceShaderCorePropertiesAMD
  , fromCStructPhysicalDeviceShaderCorePropertiesAMD
  , PhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceShaderCorePropertiesAMD"
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "shaderEngineCount"
  vkShaderEngineCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "shaderArraysPerEngineCount"
  vkShaderArraysPerEngineCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "computeUnitsPerShaderArray"
  vkComputeUnitsPerShaderArray :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "simdPerComputeUnit"
  vkSimdPerComputeUnit :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
  vkWavefrontsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "wavefrontSize"
  vkWavefrontSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "sgprsPerSimd"
  vkSgprsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "minSgprAllocation"
  vkMinSgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "maxSgprAllocation"
  vkMaxSgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "sgprAllocationGranularity"
  vkSgprAllocationGranularity :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "vgprsPerSimd"
  vkVgprsPerSimd :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "minVgprAllocation"
  vkMinVgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "maxVgprAllocation"
  vkMaxVgprAllocation :: Word32
  , -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "vgprAllocationGranularity"
  vkVgprAllocationGranularity :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShaderCorePropertiesAMD :: PhysicalDeviceShaderCorePropertiesAMD -> (VkPhysicalDeviceShaderCorePropertiesAMD -> IO a) -> IO a
withCStructPhysicalDeviceShaderCorePropertiesAMD from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShaderCorePropertiesAMD)) (\pPNext -> cont (VkPhysicalDeviceShaderCorePropertiesAMD VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD pPNext (vkShaderEngineCount (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkShaderArraysPerEngineCount (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkComputeUnitsPerShaderArray (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkSimdPerComputeUnit (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkWavefrontsPerSimd (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkWavefrontSize (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkSgprsPerSimd (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkMinSgprAllocation (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkMaxSgprAllocation (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkSgprAllocationGranularity (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkVgprsPerSimd (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkMinVgprAllocation (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkMaxVgprAllocation (from :: PhysicalDeviceShaderCorePropertiesAMD)) (vkVgprAllocationGranularity (from :: PhysicalDeviceShaderCorePropertiesAMD))))
fromCStructPhysicalDeviceShaderCorePropertiesAMD :: VkPhysicalDeviceShaderCorePropertiesAMD -> IO PhysicalDeviceShaderCorePropertiesAMD
fromCStructPhysicalDeviceShaderCorePropertiesAMD c = PhysicalDeviceShaderCorePropertiesAMD <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderCorePropertiesAMD)))
                                                                                           <*> pure (vkShaderEngineCount (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkShaderArraysPerEngineCount (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkComputeUnitsPerShaderArray (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkSimdPerComputeUnit (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkWavefrontsPerSimd (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkWavefrontSize (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkSgprsPerSimd (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkMinSgprAllocation (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkMaxSgprAllocation (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkSgprAllocationGranularity (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkVgprsPerSimd (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkMinVgprAllocation (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkMaxVgprAllocation (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
                                                                                           <*> pure (vkVgprAllocationGranularity (c :: VkPhysicalDeviceShaderCorePropertiesAMD))
