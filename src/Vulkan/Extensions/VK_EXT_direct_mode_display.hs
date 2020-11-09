{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_direct_mode_display  ( releaseDisplayEXT
                                                     , EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
                                                     , EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , pattern EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
                                                     , DisplayKHR(..)
                                                     ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
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
releaseDisplayEXT :: forall io
                   . (MonadIO io)
                  => -- | @physicalDevice@ The physical device the display is on.
                     --
                     -- #VUID-vkReleaseDisplayEXT-physicalDevice-parameter# @physicalDevice@
                     -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                     PhysicalDevice
                  -> -- | @display@ The display to release control of.
                     --
                     -- #VUID-vkReleaseDisplayEXT-display-parameter# @display@ /must/ be a valid
                     -- 'Vulkan.Extensions.Handles.DisplayKHR' handle
                     --
                     -- #VUID-vkReleaseDisplayEXT-display-parent# @display@ /must/ have been
                     -- created, allocated, or retrieved from @physicalDevice@
                     DisplayKHR
                  -> io ()
releaseDisplayEXT physicalDevice display = liftIO $ do
  let vkReleaseDisplayEXTPtr = pVkReleaseDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkReleaseDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseDisplayEXT is null" Nothing Nothing
  let vkReleaseDisplayEXT' = mkVkReleaseDisplayEXT vkReleaseDisplayEXTPtr
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

