{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , FN_vkAcquireXlibDisplayEXT
  , PFN_vkAcquireXlibDisplayEXT
  , FN_vkGetRandROutputDisplayEXT
  , PFN_vkGetRandROutputDisplayEXT
  ) where

import Data.Word
  ( Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display
  )


-- No documentation found for TopLevel "RROutput"
type RROutput = Word64
  

type FN_vkAcquireXlibDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkAcquireXlibDisplayEXT = FunPtr FN_vkAcquireXlibDisplayEXT

type FN_vkGetRandROutputDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetRandROutputDisplayEXT = FunPtr FN_vkGetRandROutputDisplayEXT
