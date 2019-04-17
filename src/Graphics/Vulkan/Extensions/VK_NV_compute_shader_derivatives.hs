{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives
  ( withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
  , fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
  , PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  ) where

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
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( VkPhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceComputeShaderDerivativesFeaturesNV"
data PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupQuads"
  vkComputeDerivativeGroupQuads :: Bool
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupLinear"
  vkComputeDerivativeGroupLinear :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> (VkPhysicalDeviceComputeShaderDerivativesFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceComputeShaderDerivativesFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV pPNext (boolToBool32 (vkComputeDerivativeGroupQuads (from :: PhysicalDeviceComputeShaderDerivativesFeaturesNV))) (boolToBool32 (vkComputeDerivativeGroupLinear (from :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)))))
fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV -> IO PhysicalDeviceComputeShaderDerivativesFeaturesNV
fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV c = PhysicalDeviceComputeShaderDerivativesFeaturesNV <$> -- Univalued Member elided
                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))
                                                                                                                 <*> pure (bool32ToBool (vkComputeDerivativeGroupQuads (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))
                                                                                                                 <*> pure (bool32ToBool (vkComputeDerivativeGroupLinear (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))
instance Zero PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  zero = PhysicalDeviceComputeShaderDerivativesFeaturesNV Nothing
                                                          False
                                                          False
