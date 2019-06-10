{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateFlagsMVK
#if defined(VK_USE_PLATFORM_GGP)
  , IOSSurfaceCreateInfoMVK(..)
#endif
  , createIOSSurfaceMVK
  , pattern MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern MVK_IOS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , vkCreateIOSSurfaceMVK
  , pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
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
  ( pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  )


-- No documentation found for TopLevel "IOSSurfaceCreateFlagsMVK"
type IOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK


-- No complete pragma for IOSSurfaceCreateFlagsMVK as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkIOSSurfaceCreateInfoMVK"
data IOSSurfaceCreateInfoMVK = IOSSurfaceCreateInfoMVK
  { -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "flags"
  flags :: IOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pView"
  view :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero IOSSurfaceCreateInfoMVK where
  zero = IOSSurfaceCreateInfoMVK Nothing
                                 zero
                                 nullPtr

#endif


-- No documentation found for TopLevel "vkCreateIOSSurfaceMVK"
createIOSSurfaceMVK :: Instance ->  IOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createIOSSurfaceMVK = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_EXTENSION_NAME"
pattern MVK_IOS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern MVK_IOS_SURFACE_EXTENSION_NAME = VK_MVK_IOS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_SPEC_VERSION"
pattern MVK_IOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern MVK_IOS_SURFACE_SPEC_VERSION = VK_MVK_IOS_SURFACE_SPEC_VERSION
