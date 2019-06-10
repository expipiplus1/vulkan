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


-- No documentation found for TopLevel "VkSurfaceProtectedCapabilitiesKHR"
data VkSurfaceProtectedCapabilitiesKHR = VkSurfaceProtectedCapabilitiesKHR
  { -- No documentation found for Nested "VkSurfaceProtectedCapabilitiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSurfaceProtectedCapabilitiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceProtectedCapabilitiesKHR" "supportsProtected"
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
  zero = VkSurfaceProtectedCapabilitiesKHR VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
                                           zero
                                           zero

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = "VK_KHR_surface_protected_capabilities"

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR"
pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR = VkStructureType 1000239000
