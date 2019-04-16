{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( acquireXlibDisplayEXT
  , getRandROutputDisplayEXT
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( acquireXlibDisplayEXT
  , getRandROutputDisplayEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  )



-- | Wrapper for vkAcquireXlibDisplayEXT
acquireXlibDisplayEXT :: PhysicalDevice ->  DisplayKHR ->  IO (Display)
acquireXlibDisplayEXT = \(PhysicalDevice physicalDevice commandTable) -> \display -> alloca (\pDpy -> Graphics.Vulkan.C.Dynamic.acquireXlibDisplayEXT commandTable physicalDevice pDpy display >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pDpy)))

-- | Wrapper for vkGetRandROutputDisplayEXT
getRandROutputDisplayEXT :: PhysicalDevice ->  RROutput ->  IO (Display, DisplayKHR)
getRandROutputDisplayEXT = \(PhysicalDevice physicalDevice commandTable) -> \rrOutput -> alloca (\pDisplay -> alloca (\pDpy -> Graphics.Vulkan.C.Dynamic.getRandROutputDisplayEXT commandTable physicalDevice pDpy rrOutput pDisplay >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> peek pDpy<*>peek pDisplay))))
