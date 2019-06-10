{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParameterFeatures
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceShaderDrawParametersFeatures(..)
#endif
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  ) where




import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


type PhysicalDeviceShaderDrawParameterFeatures = PhysicalDeviceShaderDrawParametersFeatures
-- TODO: Pattern constructor alias)


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShaderDrawParametersFeatures"
data PhysicalDeviceShaderDrawParametersFeatures = PhysicalDeviceShaderDrawParametersFeatures
  { -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "shaderDrawParameters"
  shaderDrawParameters :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShaderDrawParametersFeatures where
  zero = PhysicalDeviceShaderDrawParametersFeatures Nothing
                                                    False

#endif

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
