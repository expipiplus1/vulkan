{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParameterFeatures
  , withCStructPhysicalDeviceShaderDrawParametersFeatures
  , fromCStructPhysicalDeviceShaderDrawParametersFeatures
  , PhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  )


type PhysicalDeviceShaderDrawParameterFeatures = PhysicalDeviceShaderDrawParametersFeatures
-- TODO: Pattern constructor alias)
-- No documentation found for TopLevel "PhysicalDeviceShaderDrawParametersFeatures"
data PhysicalDeviceShaderDrawParametersFeatures = PhysicalDeviceShaderDrawParametersFeatures
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "shaderDrawParameters"
  vkShaderDrawParameters :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShaderDrawParametersFeatures :: PhysicalDeviceShaderDrawParametersFeatures -> (VkPhysicalDeviceShaderDrawParametersFeatures -> IO a) -> IO a
withCStructPhysicalDeviceShaderDrawParametersFeatures from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShaderDrawParametersFeatures)) (\pPNext -> cont (VkPhysicalDeviceShaderDrawParametersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES pPNext (boolToBool32 (vkShaderDrawParameters (from :: PhysicalDeviceShaderDrawParametersFeatures)))))
fromCStructPhysicalDeviceShaderDrawParametersFeatures :: VkPhysicalDeviceShaderDrawParametersFeatures -> IO PhysicalDeviceShaderDrawParametersFeatures
fromCStructPhysicalDeviceShaderDrawParametersFeatures c = PhysicalDeviceShaderDrawParametersFeatures <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderDrawParametersFeatures)))
                                                                                                     <*> pure (bool32ToBool (vkShaderDrawParameters (c :: VkPhysicalDeviceShaderDrawParametersFeatures)))
