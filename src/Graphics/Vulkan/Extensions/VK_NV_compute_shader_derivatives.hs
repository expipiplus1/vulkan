{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , 
#endif
  pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
  , pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV"
data PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupQuads"
  computeDerivativeGroupQuads :: Bool
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupLinear"
  computeDerivativeGroupLinear :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  zero = PhysicalDeviceComputeShaderDerivativesFeaturesNV Nothing
                                                          False
                                                          False

#endif

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME"
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION"
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION :: Integral a => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
