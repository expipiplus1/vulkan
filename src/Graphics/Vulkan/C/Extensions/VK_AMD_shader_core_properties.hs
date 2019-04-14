{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceShaderCorePropertiesAMD"
data VkPhysicalDeviceShaderCorePropertiesAMD = VkPhysicalDeviceShaderCorePropertiesAMD
  { -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "shaderEngineCount"
  vkShaderEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "shaderArraysPerEngineCount"
  vkShaderArraysPerEngineCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "computeUnitsPerShaderArray"
  vkComputeUnitsPerShaderArray :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "simdPerComputeUnit"
  vkSimdPerComputeUnit :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontsPerSimd"
  vkWavefrontsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "wavefrontSize"
  vkWavefrontSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "sgprsPerSimd"
  vkSgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "minSgprAllocation"
  vkMinSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "maxSgprAllocation"
  vkMaxSgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "sgprAllocationGranularity"
  vkSgprAllocationGranularity :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vgprsPerSimd"
  vkVgprsPerSimd :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "minVgprAllocation"
  vkMinVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "maxVgprAllocation"
  vkMaxVgprAllocation :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceShaderCorePropertiesAMD" "vgprAllocationGranularity"
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
-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = "VK_AMD_shader_core_properties"
-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD = VkStructureType 1000185000
