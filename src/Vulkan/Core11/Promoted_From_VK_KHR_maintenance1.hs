{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance1"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance1  ( trimCommandPool
                                                        , CommandPoolTrimFlags(..)
                                                        , Result(..)
                                                        , ImageCreateFlagBits(..)
                                                        , ImageCreateFlags
                                                        , FormatFeatureFlagBits(..)
                                                        , FormatFeatureFlags
                                                        ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkTrimCommandPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.Result (Result(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTrimCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ()) -> Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ()

-- No documentation found for TopLevel "vkTrimCommandPool"
trimCommandPool :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkTrimCommandPool" "device"
                   Device
                -> -- No documentation found for Nested "vkTrimCommandPool" "commandPool"
                   CommandPool
                -> -- No documentation found for Nested "vkTrimCommandPool" "flags"
                   CommandPoolTrimFlags
                -> io ()
trimCommandPool device commandPool flags = liftIO $ do
  let vkTrimCommandPoolPtr = pVkTrimCommandPool (deviceCmds (device :: Device))
  unless (vkTrimCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkTrimCommandPool is null" Nothing Nothing
  let vkTrimCommandPool' = mkVkTrimCommandPool vkTrimCommandPoolPtr
  vkTrimCommandPool' (deviceHandle (device)) (commandPool) (flags)
  pure $ ()

