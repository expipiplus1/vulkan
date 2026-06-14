{-# LANGUAGE OverloadedStrings #-}

{-| Shared boot prelude for the windowed examples. Drives the standard
@withInstance \/ withSurface \/ withDevice \/ withAllocator \/ allocSwapchain@
sequence so each main can open with a single call instead of a 20-line
copy-paste.

The window backend (SDL2 \/ GLFW) is plugged in via
'Vulkan.Utils.WindowAdapter.WindowAdapter' — 'glfwAdapter' or 'sdl2Adapter'
from the respective init package — keeping the helper itself oblivious to
which library is in use.
-}
module WindowedBoot
  ( WindowedConfig (..)
  , withWindowedVk
  , debugMessengerCreateInfo
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as Text
import Say (sayErr)
import qualified Vulkan.Core10 as Vk
import Vulkan.Core13 (pattern API_VERSION_1_3)
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Utils.Frame (frameDeviceRequirements, frameInstanceRequirements)
import Vulkan.Utils.Queues (withDevice)
import Vulkan.Utils.Swapchain (Swapchain, SwapchainConfig, allocSwapchain)
import Vulkan.Utils.VulkanContext (VulkanContext, mkVulkanContext)
import Vulkan.Utils.WindowAdapter (WindowAdapter (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import VulkanMemoryAllocator.Utils (allocatorCreateInfo)

-- | Per-example knobs the helper can't infer.
data WindowedConfig = WindowedConfig
  { wcAppName :: Text
  -- ^ Shows up in the application info; also used as the window title.
  , wcInstanceReqs :: [InstanceRequirement]
  -- ^ Extra instance requirements (frame's defaults are added automatically).
  , wcDeviceReqs :: [DeviceRequirement]
  -- ^ Extra device requirements (frame's defaults are added automatically).
  , wcVmaFlags :: VMA.AllocatorCreateFlags
  -- ^ VMA flags (e.g. @ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT@ for rays).
  , wcSwapchainConfig :: SwapchainConfig
  {- ^ Knobs for the initial swapchain. Pass 'defaultSwapchainConfig' for the
  common case; compute-shader callers (e.g. @resize@) tweak the storage bits.
  -}
  }

{- | Open a Vulkan instance + device + initial swapchain bound to the given
window. Logs the chosen device's name to stderr.
-}
withWindowedVk
  :: (MonadResource m, MonadFail m)
  => WindowedConfig
  -> WindowAdapter m
  -> m (VulkanContext, VMA.Allocator, Swapchain)
withWindowedVk WindowedConfig{..} WindowAdapter{..} = do
  inst <-
    waWithInstance
      ( Just
          zero
            { Vk.applicationName = Just (Text.encodeUtf8 wcAppName)
            , Vk.apiVersion = API_VERSION_1_3
            }
      )
      ( RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound
          : frameInstanceRequirements
          ++ wcInstanceReqs
      )
      [RequireInstanceLayer "VK_LAYER_KHRONOS_validation" minBound]
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  surf <- waWithSurface inst
  (phys, dev, qs) <-
    withDevice inst (Just surf) (frameDeviceRequirements ++ wcDeviceReqs)
  (_, vma) <-
    VMA.withAllocator
      (allocatorCreateInfo wcVmaFlags API_VERSION_1_3 inst phys dev)
      allocate
  props <- Vk.getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (Vk.deviceName props)
  vc <- liftIO $ mkVulkanContext inst phys dev qs

  initialSize <- waDrawableSize
  initialSC <-
    allocSwapchain phys dev wcSwapchainConfig Vk.NULL_HANDLE initialSize surf
  pure (vc, vma, initialSC)

{- | Standard validation/perf debug messenger create info, shared with
'HeadlessBoot.withHeadlessVk'.
-}
debugMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugMessengerCreateInfo =
  zero
    { messageSeverity =
        DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
    , messageType =
        DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    , pfnUserCallback = debugCallbackPtr
    }
