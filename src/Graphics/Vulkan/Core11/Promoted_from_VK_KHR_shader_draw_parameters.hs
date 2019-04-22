{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParameterFeatures
  , withCStructPhysicalDeviceShaderDrawParametersFeatures
  , fromCStructPhysicalDeviceShaderDrawParametersFeatures
  , PhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


type PhysicalDeviceShaderDrawParameterFeatures = PhysicalDeviceShaderDrawParametersFeatures
-- TODO: Pattern constructor alias)


-- | VkPhysicalDeviceShaderDrawParametersFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters.VkPhysicalDeviceShaderDrawParametersFeatures'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceShaderDrawParametersFeatures = PhysicalDeviceShaderDrawParametersFeatures
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderDrawParametersFeatures" "shaderDrawParameters"
  shaderDrawParameters :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShaderDrawParametersFeatures' and
-- marshal a 'PhysicalDeviceShaderDrawParametersFeatures' into it. The 'VkPhysicalDeviceShaderDrawParametersFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShaderDrawParametersFeatures :: PhysicalDeviceShaderDrawParametersFeatures -> (VkPhysicalDeviceShaderDrawParametersFeatures -> IO a) -> IO a
withCStructPhysicalDeviceShaderDrawParametersFeatures marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShaderDrawParametersFeatures)) (\pPNext -> cont (VkPhysicalDeviceShaderDrawParametersFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES pPNext (boolToBool32 (shaderDrawParameters (marshalled :: PhysicalDeviceShaderDrawParametersFeatures)))))

-- | A function to read a 'VkPhysicalDeviceShaderDrawParametersFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShaderDrawParametersFeatures'.
fromCStructPhysicalDeviceShaderDrawParametersFeatures :: VkPhysicalDeviceShaderDrawParametersFeatures -> IO PhysicalDeviceShaderDrawParametersFeatures
fromCStructPhysicalDeviceShaderDrawParametersFeatures c = PhysicalDeviceShaderDrawParametersFeatures <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderDrawParametersFeatures)))
                                                                                                     <*> pure (bool32ToBool (vkShaderDrawParameters (c :: VkPhysicalDeviceShaderDrawParametersFeatures)))

instance Zero PhysicalDeviceShaderDrawParametersFeatures where
  zero = PhysicalDeviceShaderDrawParametersFeatures Nothing
                                                    False


-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
