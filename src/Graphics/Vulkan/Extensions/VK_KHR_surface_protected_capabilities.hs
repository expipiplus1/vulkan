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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
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



-- | VkSurfaceProtectedCapabilitiesKHR - Structure describing capability of a
-- surface to be protected
--
-- = Description
--
-- Unresolved directive in VkSurfaceProtectedCapabilitiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkSurfaceProtectedCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SurfaceProtectedCapabilitiesKHR = SurfaceProtectedCapabilitiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceProtectedCapabilitiesKHR" "supportsProtected"
  supportsProtected :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSurfaceProtectedCapabilitiesKHR' and
-- marshal a 'SurfaceProtectedCapabilitiesKHR' into it. The 'VkSurfaceProtectedCapabilitiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSurfaceProtectedCapabilitiesKHR :: SurfaceProtectedCapabilitiesKHR -> (VkSurfaceProtectedCapabilitiesKHR -> IO a) -> IO a
withCStructSurfaceProtectedCapabilitiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SurfaceProtectedCapabilitiesKHR)) (\pPNext -> cont (VkSurfaceProtectedCapabilitiesKHR VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR pPNext (boolToBool32 (supportsProtected (marshalled :: SurfaceProtectedCapabilitiesKHR)))))

-- | A function to read a 'VkSurfaceProtectedCapabilitiesKHR' and all additional
-- structures in the pointer chain into a 'SurfaceProtectedCapabilitiesKHR'.
fromCStructSurfaceProtectedCapabilitiesKHR :: VkSurfaceProtectedCapabilitiesKHR -> IO SurfaceProtectedCapabilitiesKHR
fromCStructSurfaceProtectedCapabilitiesKHR c = SurfaceProtectedCapabilitiesKHR <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceProtectedCapabilitiesKHR)))
                                                                               <*> pure (bool32ToBool (vkSupportsProtected (c :: VkSurfaceProtectedCapabilitiesKHR)))

instance Zero SurfaceProtectedCapabilitiesKHR where
  zero = SurfaceProtectedCapabilitiesKHR Nothing
                                         False

