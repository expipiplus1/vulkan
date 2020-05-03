{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_direct_mode_display  ( releaseDisplayEXT
                                                     , EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , DisplayKHR(..)
                                                     ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkReleaseDisplayEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> IO Result

-- | vkReleaseDisplayEXT - Release access to an acquired VkDisplayKHR
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device the display is on.
--
-- -   @display@ The display to release control of.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
releaseDisplayEXT :: forall io . MonadIO io => PhysicalDevice -> DisplayKHR -> io ()
releaseDisplayEXT physicalDevice display = liftIO $ do
  let vkReleaseDisplayEXT' = mkVkReleaseDisplayEXT (pVkReleaseDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  _ <- vkReleaseDisplayEXT' (physicalDeviceHandle (physicalDevice)) (display)
  pure $ ()


type EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1


type EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"

-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"

