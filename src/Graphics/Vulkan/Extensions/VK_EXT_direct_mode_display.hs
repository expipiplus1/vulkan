{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
  ( pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , vkReleaseDisplayEXT
  ) where

import Data.String
  ( IsString
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )


pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"
-- | 
foreign import ccall "vkReleaseDisplayEXT" vkReleaseDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
