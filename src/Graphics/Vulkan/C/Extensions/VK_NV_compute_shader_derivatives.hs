{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( VkPhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
  , pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  ) where

import Data.String
  ( IsString
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
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkPhysicalDeviceComputeShaderDerivativesFeaturesNV - Structure
-- describing compute shader derivative features that can be supported by
-- an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#texture-derivatives-compute Compute Shader Derivatives>
-- for more information.
--
-- If the 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceComputeShaderDerivativesFeaturesNV' /can/ also be used
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceComputeShaderDerivativesFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceComputeShaderDerivativesFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceComputeShaderDerivativesFeaturesNV = VkPhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @computeDerivativeGroupQuads@ indicates that the implementation supports
  -- the @ComputeDerivativeGroupQuadsNV@ SPIR-V capability.
  vkComputeDerivativeGroupQuads :: VkBool32
  , -- | @computeDerivativeGroupLinear@ indicates that the implementation
  -- supports the @ComputeDerivativeGroupLinearNV@ SPIR-V capability.
  vkComputeDerivativeGroupLinear :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceComputeShaderDerivativesFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceComputeShaderDerivativesFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                                <*> peek (ptr `plusPtr` 8)
                                                                <*> peek (ptr `plusPtr` 16)
                                                                <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkComputeDerivativeGroupQuads (poked :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))
                *> poke (ptr `plusPtr` 20) (vkComputeDerivativeGroupLinear (poked :: VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))

instance Zero VkPhysicalDeviceComputeShaderDerivativesFeaturesNV where
  zero = VkPhysicalDeviceComputeShaderDerivativesFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
                                                            zero
                                                            zero
                                                            zero

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME"
pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_NV_compute_shader_derivatives"

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION"
pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION :: Integral a => a
pattern VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV = VkStructureType 1000201000
