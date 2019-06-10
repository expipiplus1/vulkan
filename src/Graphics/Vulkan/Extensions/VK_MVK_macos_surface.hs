{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateFlagsMVK
#if defined(VK_USE_PLATFORM_GGP)
  , MacOSSurfaceCreateInfoMVK(..)
#endif
  , createMacOSSurfaceMVK
  , pattern MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern MVK_MACOS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK(..)
  , vkCreateMacOSSurfaceMVK
  , pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  )


-- No documentation found for TopLevel "MacOSSurfaceCreateFlagsMVK"
type MacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK


-- No complete pragma for MacOSSurfaceCreateFlagsMVK as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMacOSSurfaceCreateInfoMVK"
data MacOSSurfaceCreateInfoMVK = MacOSSurfaceCreateInfoMVK
  { -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "flags"
  flags :: MacOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pView"
  view :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero MacOSSurfaceCreateInfoMVK where
  zero = MacOSSurfaceCreateInfoMVK Nothing
                                   zero
                                   nullPtr

#endif


-- No documentation found for TopLevel "vkCreateMacOSSurfaceMVK"
createMacOSSurfaceMVK :: Instance ->  MacOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMacOSSurfaceMVK = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_EXTENSION_NAME"
pattern MVK_MACOS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern MVK_MACOS_SURFACE_EXTENSION_NAME = VK_MVK_MACOS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_SPEC_VERSION"
pattern MVK_MACOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern MVK_MACOS_SURFACE_SPEC_VERSION = VK_MVK_MACOS_SURFACE_SPEC_VERSION
