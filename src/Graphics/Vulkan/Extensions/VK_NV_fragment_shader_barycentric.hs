{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  , fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  , PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceFragmentShaderBarycentricFeaturesNV"
data PhysicalDeviceFragmentShaderBarycentricFeaturesNV = PhysicalDeviceFragmentShaderBarycentricFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "fragmentShaderBarycentric"
  vkFragmentShaderBarycentric :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV -> (VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV pPNext (boolToBool32 (vkFragmentShaderBarycentric (from :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)))))
fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV -> IO PhysicalDeviceFragmentShaderBarycentricFeaturesNV
fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV c = PhysicalDeviceFragmentShaderBarycentricFeaturesNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV)))
                                                                                                                   <*> pure (bool32ToBool (vkFragmentShaderBarycentric (c :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV)))
instance Zero PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  zero = PhysicalDeviceFragmentShaderBarycentricFeaturesNV Nothing
                                                           False
