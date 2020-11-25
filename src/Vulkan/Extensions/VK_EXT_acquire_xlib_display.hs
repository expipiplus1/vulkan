{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_acquire_xlib_display"
module Vulkan.Extensions.VK_EXT_acquire_xlib_display  ( acquireXlibDisplayEXT
                                                      , getRandROutputDisplayEXT
                                                      , EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                      , pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                      , EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                      , pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                      , RROutput
                                                      , DisplayKHR(..)
                                                      , Display
                                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_xlib_surface (Display)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkAcquireXlibDisplayEXT))
import Vulkan.Dynamic (InstanceCmds(pVkGetRandROutputDisplayEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_xlib_surface (Display)
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireXlibDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result

-- No documentation found for TopLevel "vkAcquireXlibDisplayEXT"
acquireXlibDisplayEXT :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkAcquireXlibDisplayEXT" "physicalDevice"
                         PhysicalDevice
                      -> -- No documentation found for Nested "vkAcquireXlibDisplayEXT" "dpy"
                         ("dpy" ::: Ptr Display)
                      -> -- No documentation found for Nested "vkAcquireXlibDisplayEXT" "display"
                         DisplayKHR
                      -> io ()
acquireXlibDisplayEXT physicalDevice dpy display = liftIO $ do
  let vkAcquireXlibDisplayEXTPtr = pVkAcquireXlibDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkAcquireXlibDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireXlibDisplayEXT is null" Nothing Nothing
  let vkAcquireXlibDisplayEXT' = mkVkAcquireXlibDisplayEXT vkAcquireXlibDisplayEXTPtr
  r <- vkAcquireXlibDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (display)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRandROutputDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Display -> RROutput -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Display -> RROutput -> Ptr DisplayKHR -> IO Result

-- No documentation found for TopLevel "vkGetRandROutputDisplayEXT"
getRandROutputDisplayEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkGetRandROutputDisplayEXT" "physicalDevice"
                            PhysicalDevice
                         -> -- No documentation found for Nested "vkGetRandROutputDisplayEXT" "dpy"
                            ("dpy" ::: Ptr Display)
                         -> -- No documentation found for Nested "vkGetRandROutputDisplayEXT" "rrOutput"
                            RROutput
                         -> io (DisplayKHR)
getRandROutputDisplayEXT physicalDevice dpy rrOutput = liftIO . evalContT $ do
  let vkGetRandROutputDisplayEXTPtr = pVkGetRandROutputDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetRandROutputDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRandROutputDisplayEXT is null" Nothing Nothing
  let vkGetRandROutputDisplayEXT' = mkVkGetRandROutputDisplayEXT vkGetRandROutputDisplayEXTPtr
  pPDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  r <- lift $ vkGetRandROutputDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (rrOutput) (pPDisplay)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDisplay <- lift $ peek @DisplayKHR pPDisplay
  pure $ (pDisplay)


type EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1


type EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"


type RROutput = Word64

