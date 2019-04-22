{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( withCStructPhysicalDeviceShaderCorePropertiesAMD
  , fromCStructPhysicalDeviceShaderCorePropertiesAMD
  , PhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  ) where

import Data.String
  ( IsString
  )
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME
  , pattern VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )



-- | VkPhysicalDeviceShaderCorePropertiesAMD - Structure describing shader
-- core properties that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties.VkPhysicalDeviceShaderCorePropertiesAMD'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties.VkPhysicalDeviceShaderCorePropertiesAMD'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceShaderCorePropertiesAMD = PhysicalDeviceShaderCorePropertiesAMD
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShaderCorePropertiesAMD" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShaderCorePropertiesAMD' and
-- marshal a 'PhysicalDeviceShaderCorePropertiesAMD' into it. The 'VkPhysicalDeviceShaderCorePropertiesAMD' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShaderCorePropertiesAMD :: PhysicalDeviceShaderCorePropertiesAMD -> (VkPhysicalDeviceShaderCorePropertiesAMD -> IO a) -> IO a
withCStructPhysicalDeviceShaderCorePropertiesAMD marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (\pPNext -> cont (VkPhysicalDeviceShaderCorePropertiesAMD VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD pPNext (shaderEngineCount (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (shaderArraysPerEngineCount (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (computeUnitsPerShaderArray (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (simdPerComputeUnit (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (wavefrontsPerSimd (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (wavefrontSize (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (sgprsPerSimd (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (minSgprAllocation (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (maxSgprAllocation (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (sgprAllocationGranularity (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (vgprsPerSimd (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (minVgprAllocation (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (maxVgprAllocation (marshalled :: PhysicalDeviceShaderCorePropertiesAMD)) (vgprAllocationGranularity (marshalled :: PhysicalDeviceShaderCorePropertiesAMD))))

-- | A function to read a 'VkPhysicalDeviceShaderCorePropertiesAMD' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShaderCorePropertiesAMD'.
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


-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME"
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME = VK_AMD_SHADER_CORE_PROPERTIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION"
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION = VK_AMD_SHADER_CORE_PROPERTIES_SPEC_VERSION
