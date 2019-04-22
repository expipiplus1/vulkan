{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  , fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  , PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )
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
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  )



-- | VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV - Structure
-- describing barycentric support in fragment shaders that can be supported
-- by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric.VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-barycentric Barycentric Interpolation>
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric.VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric.VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceFragmentShaderBarycentricFeaturesNV = PhysicalDeviceFragmentShaderBarycentricFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "fragmentShaderBarycentric"
  fragmentShaderBarycentric :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV' and
-- marshal a 'PhysicalDeviceFragmentShaderBarycentricFeaturesNV' into it. The 'VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV -> (VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV pPNext (boolToBool32 (fragmentShaderBarycentric (marshalled :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFragmentShaderBarycentricFeaturesNV'.
fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV -> IO PhysicalDeviceFragmentShaderBarycentricFeaturesNV
fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV c = PhysicalDeviceFragmentShaderBarycentricFeaturesNV <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV)))
                                                                                                                   <*> pure (bool32ToBool (vkFragmentShaderBarycentric (c :: VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV)))

instance Zero PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  zero = PhysicalDeviceFragmentShaderBarycentricFeaturesNV Nothing
                                                           False


-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION :: Integral a => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
