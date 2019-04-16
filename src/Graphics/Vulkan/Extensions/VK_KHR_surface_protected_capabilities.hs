{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities
  ( withCStructSurfaceProtectedCapabilitiesKHR
  , fromCStructSurfaceProtectedCapabilitiesKHR
  , SurfaceProtectedCapabilitiesKHR(..)
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( VkSurfaceProtectedCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
  )


-- No documentation found for TopLevel "SurfaceProtectedCapabilitiesKHR"
data SurfaceProtectedCapabilitiesKHR = SurfaceProtectedCapabilitiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "supportsProtected"
  vkSupportsProtected :: Bool
  }
  deriving (Show, Eq)
withCStructSurfaceProtectedCapabilitiesKHR :: SurfaceProtectedCapabilitiesKHR -> (VkSurfaceProtectedCapabilitiesKHR -> IO a) -> IO a
withCStructSurfaceProtectedCapabilitiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SurfaceProtectedCapabilitiesKHR)) (\pPNext -> cont (VkSurfaceProtectedCapabilitiesKHR VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR pPNext (boolToBool32 (vkSupportsProtected (from :: SurfaceProtectedCapabilitiesKHR)))))
fromCStructSurfaceProtectedCapabilitiesKHR :: VkSurfaceProtectedCapabilitiesKHR -> IO SurfaceProtectedCapabilitiesKHR
fromCStructSurfaceProtectedCapabilitiesKHR c = SurfaceProtectedCapabilitiesKHR <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceProtectedCapabilitiesKHR)))
                                                                               <*> pure (bool32ToBool (vkSupportsProtected (c :: VkSurfaceProtectedCapabilitiesKHR)))
