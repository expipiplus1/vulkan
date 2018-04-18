{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  , VkPhysicalDeviceShaderDrawParameterFeatures(..)
  ) where

import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = VkStructureType 1000063000
-- | TODO: Struct comments
data VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParameterFeatures
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkShaderDrawParameters :: VkBool32
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
