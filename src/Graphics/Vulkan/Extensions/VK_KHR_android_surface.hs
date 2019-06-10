{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , AndroidSurfaceCreateInfoKHR(..)
#endif
  , createAndroidSurfaceKHR
  , pattern KHR_ANDROID_SURFACE_EXTENSION_NAME
  , pattern KHR_ANDROID_SURFACE_SPEC_VERSION
  , ANativeWindow
  , pattern STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateFlagsKHR(..)
  , vkCreateAndroidSurfaceKHR
  , pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( ANativeWindow
  )
#endif
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
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( ANativeWindow
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "AndroidSurfaceCreateFlagsKHR"
type AndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR


-- No complete pragma for AndroidSurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAndroidSurfaceCreateInfoKHR"
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "flags"
  flags :: AndroidSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "window"
  window :: Ptr ANativeWindow
  }
  deriving (Show, Eq)

instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR Nothing
                                     zero
                                     nullPtr

#endif


-- No documentation found for TopLevel "vkCreateAndroidSurfaceKHR"
createAndroidSurfaceKHR :: Instance ->  AndroidSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createAndroidSurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_EXTENSION_NAME"
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_ANDROID_SURFACE_EXTENSION_NAME = VK_KHR_ANDROID_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_ANDROID_SURFACE_SPEC_VERSION"
pattern KHR_ANDROID_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_ANDROID_SURFACE_SPEC_VERSION = VK_KHR_ANDROID_SURFACE_SPEC_VERSION
