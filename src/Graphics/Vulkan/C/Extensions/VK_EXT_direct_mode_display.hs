{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( FN_vkReleaseDisplayEXT
  , PFN_vkReleaseDisplayEXT
  , vkReleaseDisplayEXT
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseDisplayEXT" vkReleaseDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
#else
vkReleaseDisplayEXT :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
vkReleaseDisplayEXT deviceCmds = mkVkReleaseDisplayEXT (pVkReleaseDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
#endif

type FN_vkReleaseDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkReleaseDisplayEXT = FunPtr FN_vkReleaseDisplayEXT

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1
