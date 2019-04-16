{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
  ( releaseDisplayEXT
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( releaseDisplayEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
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
import Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  )



-- | Wrapper for vkReleaseDisplayEXT
releaseDisplayEXT :: PhysicalDevice ->  DisplayKHR ->  IO ()
releaseDisplayEXT = \(PhysicalDevice physicalDevice commandTable) -> \display -> Graphics.Vulkan.C.Dynamic.releaseDisplayEXT commandTable physicalDevice display >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
