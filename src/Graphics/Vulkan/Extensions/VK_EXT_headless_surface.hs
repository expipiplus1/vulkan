{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_headless_surface
  ( HeadlessSurfaceCreateFlagsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , HeadlessSurfaceCreateInfoEXT(..)
#endif
  , createHeadlessSurfaceEXT
  , pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME
  , pattern EXT_HEADLESS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
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
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateFlagsEXT(..)
  , vkCreateHeadlessSurfaceEXT
  , pattern VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_HEADLESS_SURFACE_SPEC_VERSION
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
  ( pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
  )


-- No documentation found for TopLevel "HeadlessSurfaceCreateFlagsEXT"
type HeadlessSurfaceCreateFlagsEXT = VkHeadlessSurfaceCreateFlagsEXT


-- No complete pragma for HeadlessSurfaceCreateFlagsEXT as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkHeadlessSurfaceCreateInfoEXT"
data HeadlessSurfaceCreateInfoEXT = HeadlessSurfaceCreateInfoEXT
  { -- No documentation found for Nested "HeadlessSurfaceCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "HeadlessSurfaceCreateInfoEXT" "flags"
  flags :: HeadlessSurfaceCreateFlagsEXT
  }
  deriving (Show, Eq)

instance Zero HeadlessSurfaceCreateInfoEXT where
  zero = HeadlessSurfaceCreateInfoEXT Nothing
                                      zero

#endif


-- No documentation found for TopLevel "vkCreateHeadlessSurfaceEXT"
createHeadlessSurfaceEXT :: Instance ->  HeadlessSurfaceCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createHeadlessSurfaceEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME"
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME = VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_SPEC_VERSION"
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION :: Integral a => a
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION = VK_EXT_HEADLESS_SURFACE_SPEC_VERSION
