{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( withCStructPhysicalDeviceFloatControlsPropertiesKHR
  , fromCStructPhysicalDeviceFloatControlsPropertiesKHR
  , PhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  )



-- | VkPhysicalDeviceFloatControlsPropertiesKHR - Structure describing
-- properties supported by VK_KHR_shader_float_controls
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls.VkPhysicalDeviceFloatControlsPropertiesKHR'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceFloatControlsPropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceFloatControlsPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceFloatControlsPropertiesKHR = PhysicalDeviceFloatControlsPropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateDenormSettings"
  separateDenormSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateRoundingModeSettings"
  separateRoundingModeSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat16"
  shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat32"
  shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat64"
  shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat16"
  shaderDenormPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat32"
  shaderDenormPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat64"
  shaderDenormPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat16"
  shaderDenormFlushToZeroFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat32"
  shaderDenormFlushToZeroFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat64"
  shaderDenormFlushToZeroFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat16"
  shaderRoundingModeRTEFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat32"
  shaderRoundingModeRTEFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat64"
  shaderRoundingModeRTEFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat16"
  shaderRoundingModeRTZFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat32"
  shaderRoundingModeRTZFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat64"
  shaderRoundingModeRTZFloat64 :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFloatControlsPropertiesKHR' and
-- marshal a 'PhysicalDeviceFloatControlsPropertiesKHR' into it. The 'VkPhysicalDeviceFloatControlsPropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFloatControlsPropertiesKHR :: PhysicalDeviceFloatControlsPropertiesKHR -> (VkPhysicalDeviceFloatControlsPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceFloatControlsPropertiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceFloatControlsPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR pPNext (boolToBool32 (separateDenormSettings (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (separateRoundingModeSettings (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat16 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat32 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderSignedZeroInfNanPreserveFloat64 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormPreserveFloat16 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormPreserveFloat32 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormPreserveFloat64 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormFlushToZeroFloat16 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormFlushToZeroFloat32 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderDenormFlushToZeroFloat64 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTEFloat16 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTEFloat32 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTEFloat64 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTZFloat16 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTZFloat32 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (shaderRoundingModeRTZFloat64 (marshalled :: PhysicalDeviceFloatControlsPropertiesKHR)))))

-- | A function to read a 'VkPhysicalDeviceFloatControlsPropertiesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFloatControlsPropertiesKHR'.
fromCStructPhysicalDeviceFloatControlsPropertiesKHR :: VkPhysicalDeviceFloatControlsPropertiesKHR -> IO PhysicalDeviceFloatControlsPropertiesKHR
fromCStructPhysicalDeviceFloatControlsPropertiesKHR c = PhysicalDeviceFloatControlsPropertiesKHR <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkSeparateDenormSettings (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkSeparateRoundingModeSettings (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))

instance Zero PhysicalDeviceFloatControlsPropertiesKHR where
  zero = PhysicalDeviceFloatControlsPropertiesKHR Nothing
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False

