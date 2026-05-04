{-# LANGUAGE QuasiQuotes #-}
module Init
  ( Init.createInstance
  , Init.createDevice
  , createVMA
  ) where

import           Control.Applicative            ( empty )
import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           Say
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_timeline_semaphore

import qualified SDL.Video                     as SDL
import           Utils                          ( noSuchThing )
import           VkResources                    ( Queues(..) )
import qualified Vma
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import qualified Vulkan.Core10                 as MemoryHeap (MemoryHeap(..))
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Requirement
import qualified Vulkan.Utils.Init.SDL2        as VkInit
import           Vulkan.Utils.Initialization
import           Vulkan.Utils.QueueAssignment
import qualified Vulkan.Utils.Requirements.TH  as U
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator )
import           Window.SDL2

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

----------------------------------------------------------------
-- Instance Creation
----------------------------------------------------------------

createInstance :: MonadResource m => SDL.Window -> m Instance
createInstance win = VkInit.withInstance
  win
  (Just zero { applicationName = Nothing, apiVersion = myApiVersion })
  [ RequireInstanceExtension
      Nothing
      KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
      minBound
  ]
  []

----------------------------------------------------------------
-- Device creation
----------------------------------------------------------------

createDevice
  :: forall m
   . (MonadResource m)
  => Instance
  -> SDL.Window
  -> m
       ( PhysicalDevice
       , Device
       , Queues (QueueFamilyIndex, Queue)
       , SurfaceKHR
       )
createDevice inst win = do
  (_  , surf) <- createSurface inst win
  (pdi, phys) <-
    maybe (noSuchThing "Unable to find appropriate PhysicalDevice") pure
      =<< pickPhysicalDevice inst (physicalDeviceInfo surf) pdiScore
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  let deviceCreateInfo =
        zero { queueCreateInfos = SomeStruct <$> pdiQueueCreateInfos pdi }
      reqs = [U.reqs|
          1.0
          VK_KHR_swapchain
          VK_KHR_timeline_semaphore
          PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
        |]
  dev    <- createDeviceFromRequirements reqs [] phys deviceCreateInfo
  queues <- liftIO $ pdiGetQueues pdi dev
  pure (phys, dev, queues, surf)

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory      :: Word64
  , pdiQueueCreateInfos :: Vector (DeviceQueueCreateInfo '[])
  , pdiGetQueues        :: Device -> IO (Queues (QueueFamilyIndex, Queue))
  }

pdiScore :: PhysicalDeviceInfo -> Word64
pdiScore = pdiTotalMemory

physicalDeviceInfo
  :: MonadIO m => SurfaceKHR -> PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surf phys = runMaybeT $ do
  deviceName            <- physicalDeviceName phys

  hasTimelineSemaphores <- deviceHasTimelineSemaphores phys
  unless hasTimelineSemaphores $ do
    sayErr
      $  "Not using physical device "
      <> deviceName
      <> " because it doesn't support timeline semaphores"
    empty

  hasSwapchainSupport <- deviceHasSwapchain phys
  unless hasSwapchainSupport $ do
    sayErr
      $  "Not using physical device "
      <> deviceName
      <> " because it doesn't support swapchains"
    empty

  (pdiQueueCreateInfos, pdiGetQueues) <- MaybeT
    $ assignQueues phys (queueRequirements phys surf)

  -- Score by total device memory.
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum (MemoryHeap.size <$> heaps)

  pure PhysicalDeviceInfo { .. }

-- | A graphics queue that can also present to the given surface.
queueRequirements
  :: MonadIO m => PhysicalDevice -> SurfaceKHR -> Queues (QueueSpec m)
queueRequirements phys surf = Queues (QueueSpec 1 isGraphicsPresentQueue)
 where
  isGraphicsPresentQueue queueFamilyIndex queueFamilyProperties =
    (&& isGraphicsQueueFamily queueFamilyProperties)
      <$> isPresentQueueFamily phys surf queueFamilyIndex

deviceHasSwapchain :: MonadIO m => PhysicalDevice -> m Bool
deviceHasSwapchain dev = do
  (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
  pure $ V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions

deviceHasTimelineSemaphores :: MonadIO m => PhysicalDevice -> m Bool
deviceHasTimelineSemaphores phys = do
  let
    hasExt = do
      (_, extensions) <- enumerateDeviceExtensionProperties phys Nothing
      pure $ V.any
        ((KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME ==) . extensionName)
        extensions

    hasFeat = do
      feats <- getPhysicalDeviceFeatures2KHR phys
      let _ ::& (PhysicalDeviceTimelineSemaphoreFeatures hasTimelineSemaphores :& ())
            = feats
      pure hasTimelineSemaphores

  (&&) <$> hasExt <*> hasFeat

----------------------------------------------------------------
-- VulkanMemoryAllocator
----------------------------------------------------------------

createVMA
  :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA = Vma.createVMA zero myApiVersion
