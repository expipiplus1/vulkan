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



-- | VkPhysicalDeviceComputeShaderDerivativesFeaturesNV - Structure
-- describing compute shader derivative features that can be supported by
-- an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives.VkPhysicalDeviceComputeShaderDerivativesFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#texture-derivatives-compute Compute Shader Derivatives>
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives.VkPhysicalDeviceComputeShaderDerivativesFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives.VkPhysicalDeviceComputeShaderDerivativesFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceComputeShaderDerivativesFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceComputeShaderDerivativesFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupQuads"
  computeDerivativeGroupQuads :: Bool
  , -- No documentation found for Nested "PhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupLinear"
  computeDerivativeGroupLinear :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' and
-- marshal a 'PhysicalDeviceComputeShaderDerivativesFeaturesNV' into it. The 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV :: PhysicalDeviceComputeShaderDerivativesFeaturesNV -> (VkPhysicalDeviceComputeShaderDerivativesFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceComputeShaderDerivativesFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV pPNext (boolToBool32 (computeDerivativeGroupQuads (marshalled :: PhysicalDeviceComputeShaderDerivativesFeaturesNV))) (boolToBool32 (computeDerivativeGroupLinear (marshalled :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceComputeShaderDerivativesFeaturesNV'.
fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV -> IO PhysicalDeviceComputeShaderDerivativesFeaturesNV
fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV c = PhysicalDeviceComputeShaderDerivativesFeaturesNV <$> -- Univalued Member elided
                                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))
                                                                                                                 <*> pure (bool32ToBool (vkComputeDerivativeGroupQuads (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))
                                                                                                                 <*> pure (bool32ToBool (vkComputeDerivativeGroupLinear (c :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV)))

instance Zero PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  zero = PhysicalDeviceComputeShaderDerivativesFeaturesNV Nothing
                                                          False
                                                          False

