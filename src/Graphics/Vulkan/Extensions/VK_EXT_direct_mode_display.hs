{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
  ( releaseDisplayEXT
  , pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( vkReleaseDisplayEXT
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )



-- No documentation found for TopLevel "vkReleaseDisplayEXT"
releaseDisplayEXT :: PhysicalDevice ->  DisplayKHR ->  IO ()
releaseDisplayEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: Integral a => a
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
