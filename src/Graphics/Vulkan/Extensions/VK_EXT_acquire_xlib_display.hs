{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( acquireXlibDisplayEXT
  , getRandROutputDisplayEXT
  , pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Ptr
  ( Ptr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , vkAcquireXlibDisplayEXT
  , vkGetRandROutputDisplayEXT
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )



-- No documentation found for TopLevel "vkAcquireXlibDisplayEXT"
acquireXlibDisplayEXT :: PhysicalDevice ->  Ptr Display ->  DisplayKHR ->  IO ()
acquireXlibDisplayEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetRandROutputDisplayEXT"
getRandROutputDisplayEXT :: PhysicalDevice ->  Ptr Display ->  RROutput ->  IO (DisplayKHR)
getRandROutputDisplayEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: Integral a => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
