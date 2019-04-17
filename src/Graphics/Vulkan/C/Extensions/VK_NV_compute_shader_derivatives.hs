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


-- No documentation found for TopLevel "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV"
data VkPhysicalDeviceComputeShaderDerivativesFeaturesNV = VkPhysicalDeviceComputeShaderDerivativesFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupQuads"
  vkComputeDerivativeGroupQuads :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV" "computeDerivativeGroupLinear"
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
  zero = VkPhysicalDeviceComputeShaderDerivativesFeaturesNV zero
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
