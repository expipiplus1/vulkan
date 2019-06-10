{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateFlagsNN
#if defined(VK_USE_PLATFORM_GGP)
  , ViSurfaceCreateInfoNN(..)
#endif
  , createViSurfaceNN
  , pattern NN_VI_SURFACE_EXTENSION_NAME
  , pattern NN_VI_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
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
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , vkCreateViSurfaceNN
  , pattern VK_NN_VI_SURFACE_EXTENSION_NAME
  , pattern VK_NN_VI_SURFACE_SPEC_VERSION
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
  ( pattern STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  )


-- No documentation found for TopLevel "ViSurfaceCreateFlagsNN"
type ViSurfaceCreateFlagsNN = VkViSurfaceCreateFlagsNN


-- No complete pragma for ViSurfaceCreateFlagsNN as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkViSurfaceCreateInfoNN"
data ViSurfaceCreateInfoNN = ViSurfaceCreateInfoNN
  { -- No documentation found for Nested "ViSurfaceCreateInfoNN" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "flags"
  flags :: ViSurfaceCreateFlagsNN
  , -- No documentation found for Nested "ViSurfaceCreateInfoNN" "window"
  window :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero ViSurfaceCreateInfoNN where
  zero = ViSurfaceCreateInfoNN Nothing
                               zero
                               nullPtr

#endif


-- No documentation found for TopLevel "vkCreateViSurfaceNN"
createViSurfaceNN :: Instance ->  ViSurfaceCreateInfoNN ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createViSurfaceNN = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_EXTENSION_NAME"
pattern NN_VI_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NN_VI_SURFACE_EXTENSION_NAME = VK_NN_VI_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NN_VI_SURFACE_SPEC_VERSION"
pattern NN_VI_SURFACE_SPEC_VERSION :: Integral a => a
pattern NN_VI_SURFACE_SPEC_VERSION = VK_NN_VI_SURFACE_SPEC_VERSION
