{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  SurfaceProtectedCapabilitiesKHR(..)
  , 
#endif
  pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  , pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceProtectedCapabilitiesKHR"
data SurfaceProtectedCapabilitiesKHR = SurfaceProtectedCapabilitiesKHR
  { -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "supportsProtected"
  supportsProtected :: Bool
  }
  deriving (Show, Eq)

instance Zero SurfaceProtectedCapabilitiesKHR where
  zero = SurfaceProtectedCapabilitiesKHR Nothing
                                         False

#endif

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME"
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION"
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
