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


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( vkReleaseDisplayEXT
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



-- | vkReleaseDisplayEXT - Release access to an acquired VkDisplayKHR
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device the display is on.
--
-- -   @display@ The display to release control of.
--
-- = Description
--
-- Unresolved directive in vkReleaseDisplayEXT.txt -
-- include::{generated}\/validity\/protos\/vkReleaseDisplayEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
releaseDisplayEXT :: PhysicalDevice ->  DisplayKHR ->  IO ()
releaseDisplayEXT = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> vkReleaseDisplayEXT commandTable physicalDevice' display' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))
