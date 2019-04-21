{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParameterFeatures
  , pattern VkPhysicalDeviceShaderDrawParameterFeatures
  , VkPhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  ) where

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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkPhysicalDeviceShaderDrawParameterFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'VkPhysicalDeviceShaderDrawParameterFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
type VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParametersFeatures


-- | VkPhysicalDeviceShaderDrawParameterFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'VkPhysicalDeviceShaderDrawParameterFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
pattern VkPhysicalDeviceShaderDrawParameterFeatures :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("shaderDrawParameters" ::: VkBool32) -> VkPhysicalDeviceShaderDrawParameterFeatures
pattern VkPhysicalDeviceShaderDrawParameterFeatures vkSType vkPNext vkShaderDrawParameters = VkPhysicalDeviceShaderDrawParametersFeatures vkSType vkPNext vkShaderDrawParameters

-- | VkPhysicalDeviceShaderDrawParametersFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'VkPhysicalDeviceShaderDrawParametersFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- Unresolved directive in VkPhysicalDeviceShaderDrawParametersFeatures.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceShaderDrawParametersFeatures.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceShaderDrawParametersFeatures = VkPhysicalDeviceShaderDrawParametersFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceShaderDrawParametersFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderDrawParametersFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- | @shaderDrawParameters@ specifies whether shader draw parameters are
  -- supported.
  vkShaderDrawParameters :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShaderDrawParametersFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShaderDrawParametersFeatures <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShaderDrawParametersFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShaderDrawParametersFeatures))
                *> poke (ptr `plusPtr` 16) (vkShaderDrawParameters (poked :: VkPhysicalDeviceShaderDrawParametersFeatures))

instance Zero VkPhysicalDeviceShaderDrawParametersFeatures where
  zero = VkPhysicalDeviceShaderDrawParametersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
                                                      zero
                                                      zero

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES = VkStructureType 1000063000

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
