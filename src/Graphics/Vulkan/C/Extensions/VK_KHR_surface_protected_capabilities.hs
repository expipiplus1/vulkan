{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( VkSurfaceProtectedCapabilitiesKHR(..)
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- | VkSurfaceProtectedCapabilitiesKHR - Structure describing capability of a
-- surface to be protected
--
-- = Description
--
-- Unresolved directive in VkSurfaceProtectedCapabilitiesKHR.txt -
-- include::..\/validity\/structs\/VkSurfaceProtectedCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSurfaceProtectedCapabilitiesKHR = VkSurfaceProtectedCapabilitiesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @supportsProtected@ specifies whether a protected swapchain created from
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'::@surface@
  -- for a particular windowing system /can/ be displayed on screen or not.
  -- If @supportsProtected@ is @VK_TRUE@, then creation of swapchains with
  -- the @VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR@ flag set /must/ be supported
  -- for @surface@.
  vkSupportsProtected :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSurfaceProtectedCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceProtectedCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceProtectedCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceProtectedCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSupportsProtected (poked :: VkSurfaceProtectedCapabilitiesKHR))

instance Zero VkSurfaceProtectedCapabilitiesKHR where
  zero = VkSurfaceProtectedCapabilitiesKHR zero
                                           zero
                                           zero
-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = "VK_KHR_surface_protected_capabilities"
-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR"
pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR = VkStructureType 1000239000
