{-# LANGUAGE OverloadedStrings #-}

{-| Shared boot prelude for the windowed examples. Drives the standard
@withInstance \/ withSurface \/ withDevice \/ createVMA \/ allocSwapchain@
sequence so each main can open with a single call instead of a 20-line
copy-paste.

The window backend (SDL2 \/ GLFW) is plugged in via 'WindowAdapter' —
'Window.SDL2.sdl2Adapter' or 'Window.GLFW.glfwAdapter' — keeping the helper
itself oblivious to which library is in use.
-}
module WindowedBoot
  ( WindowedConfig (..)
  , WindowAdapter (..)
  , withWindowedVk
  , debugMessengerCreateInfo
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as Text
import Frame (frameDeviceRequirements, frameInstanceRequirements)
import InitDevice (withDevice)
import Say (sayErr)
import Swapchain (Swapchain, allocSwapchain)
import UnliftIO (MonadUnliftIO)
import VkResources (VkResources, mkVkResources)
import qualified Vma
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12 (pattern API_VERSION_1_2)
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_KHR_surface (SurfaceKHR)
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

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
  }

-- | Bridge between the helper and a particular window-library backend.
data WindowAdapter m = WindowAdapter
  { waWithInstance
      :: Maybe Vk.ApplicationInfo
      -> [InstanceRequirement]
      -> [InstanceRequirement]
      -> m Vk.Instance
  , waWithSurface :: Vk.Instance -> m (ReleaseKey, SurfaceKHR)
  , waDrawableSize :: m Vk.Extent2D
  }

{- | Open a Vulkan instance + device + initial swapchain bound to the given
window. Logs the chosen device's name to stderr.
-}
withWindowedVk
  :: (MonadResource m, MonadFail m, MonadUnliftIO m)
  => WindowedConfig
  -> WindowAdapter m
  -> m (VkResources, Swapchain)
withWindowedVk WindowedConfig{..} WindowAdapter{..} = do
  inst <-
    waWithInstance
      ( Just
          zero
            { Vk.applicationName = Just (Text.encodeUtf8 wcAppName)
            , Vk.apiVersion = API_VERSION_1_2
            }
      )
      ( RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound
          : frameInstanceRequirements
          ++ wcInstanceReqs
      )
      [RequireInstanceLayer "VK_LAYER_KHRONOS_validation" minBound]
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  (_, surf) <- waWithSurface inst
  (phys, dev, qs) <-
    withDevice inst (Just surf) (frameDeviceRequirements ++ wcDeviceReqs)
  vma <- Vma.createVMA wcVmaFlags API_VERSION_1_2 inst phys dev
  props <- Vk.getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (Vk.deviceName props)
  vr <- liftIO $ mkVkResources inst phys dev vma qs

  initialSize <- waDrawableSize
  initialSC <- allocSwapchain vr Vk.NULL_HANDLE initialSize surf
  pure (vr, initialSC)

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
