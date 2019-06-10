{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( MetalSurfaceCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , MetalSurfaceCreateInfoEXT(..)
#endif
  , createMetalSurfaceEXT
  , pattern EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern EXT_METAL_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateFlagsEXT(..)
  , vkCreateMetalSurfaceEXT
  , pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_METAL_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( CAMetalLayer
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
  )


-- No documentation found for TopLevel "MetalSurfaceCreateFlagsEXT"
type MetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT


-- No complete pragma for MetalSurfaceCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMetalSurfaceCreateInfoEXT"
data MetalSurfaceCreateInfoEXT = MetalSurfaceCreateInfoEXT
  { -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "flags"
  flags :: MetalSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pLayer"
  layer :: Ptr CAMetalLayer
  }
  deriving (Show, Eq)

instance Zero MetalSurfaceCreateInfoEXT where
  zero = MetalSurfaceCreateInfoEXT Nothing
                                   zero
                                   zero

#endif


-- No documentation found for TopLevel "vkCreateMetalSurfaceEXT"
createMetalSurfaceEXT :: Instance ->  MetalSurfaceCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMetalSurfaceEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_EXTENSION_NAME"
pattern EXT_METAL_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_METAL_SURFACE_EXTENSION_NAME = VK_EXT_METAL_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_SPEC_VERSION"
pattern EXT_METAL_SURFACE_SPEC_VERSION :: Integral a => a
pattern EXT_METAL_SURFACE_SPEC_VERSION = VK_EXT_METAL_SURFACE_SPEC_VERSION
