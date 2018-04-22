{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  , VkPhysicalDeviceShaderDrawParameterFeatures(..)
  ) where

import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = VkStructureType 1000063000
-- | VkPhysicalDeviceShaderDrawParameterFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the @VkPhysicalDeviceShaderDrawParameterFeatures@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParameterFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceShaderDrawParameterFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceShaderDrawParameterFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- | @shaderDrawParameters@ specifies whether shader draw parameters are
  -- supported.
  vkShaderDrawParameters :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceShaderDrawParameterFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceShaderDrawParameterFeatures <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceShaderDrawParameterFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceShaderDrawParameterFeatures))
                *> poke (ptr `plusPtr` 16) (vkShaderDrawParameters (poked :: VkPhysicalDeviceShaderDrawParameterFeatures))
