module Init
  ( Init.createInstance
  , Init.createDevice
  , Queues(..)
  , createVMA
  ) where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Word
import           Say
import           UnliftIO.Exception
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                                ( PhysicalDeviceTimelineSemaphoreFeatures(..)
                                                )
import           Vulkan.Extensions.VK_KHR_timeline_semaphore

import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.Initialization
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator
                                                , AllocatorCreateInfo(..)
                                                , withAllocator
                                                )

import           Control.Applicative
import qualified Data.ByteString               as BS
import           Data.Vector                    ( Vector )
import           GHC.IO.Exception               ( IOErrorType(NoSuchThing)
                                                , IOException(IOError)
                                                )
import qualified SDL.Video                     as SDL
import qualified SDL.Video.Vulkan              as SDL
import           Vulkan.Utils.QueueAssignment
import           Window

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

----------------------------------------------------------------
-- Instance Creation
----------------------------------------------------------------

-- | Create an instance with a debug messenger
createInstance :: MonadResource m => SDL.Window -> m Instance
createInstance win = do
  windowExtensions <-
    liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions win
  let createInfo = zero
        { applicationInfo = Just zero { applicationName = Nothing
                                      , apiVersion      = myApiVersion
                                      }
        }
      extensions =
        [KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME]
          <> windowExtensions
  createDebugInstanceWithExtensions [] [] extensions [] createInfo

----------------------------------------------------------------
-- Device creation
----------------------------------------------------------------

createDevice
  :: forall m
   . (MonadResource m)
  => Instance
  -> SDL.Window
  -> m (PhysicalDevice, Device, Queues (QueueFamilyIndex, Queue))
createDevice inst win = do
  (_  , surf) <- createSurface inst win
  (pdi, phys) <-
    maybe (noSuchThing "Unable to find appropriate PhysicalDevice") pure
      =<< pickPhysicalDevice inst (physicalDeviceInfo surf) pdiScore
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys
  let deviceCreateInfo =
        zero { queueCreateInfos = SomeStruct <$> pdiQueueCreateInfos pdi }
          ::& PhysicalDeviceTimelineSemaphoreFeatures True
          :&  ()
      extensions =
        [KHR_TIMELINE_SEMAPHORE_EXTENSION_NAME, KHR_SWAPCHAIN_EXTENSION_NAME]
  dev    <- createDeviceWithExtensions phys [] extensions deviceCreateInfo
  queues <- liftIO $ pdiGetQueues pdi dev
  pure (phys, dev, queues)

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

newtype Queues a = Queues { graphicsQueue :: a }
  deriving (Functor, Foldable, Traversable)

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

  hasSwapChainSupport <- deviceHasSwapChain phys
  unless hasSwapChainSupport $ do
    sayErr
      $  "Not using physical device "
      <> deviceName
      <> " because it doesn't support swapchains"
    empty

  (pdiQueueCreateInfos, pdiGetQueues) <- MaybeT
    $ assignQueues phys (queueRequirements phys surf)

  --
  -- We'll use the amount of memory to pick the "best" device
  --
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)

  pure PhysicalDeviceInfo { .. }

-- | Requirements for a 'Queue' which has graphics suppor and can present to
-- the specified surface.
queueRequirements
  :: MonadIO m => PhysicalDevice -> SurfaceKHR -> Queues (QueueSpec m)
queueRequirements phys surf = Queues (QueueSpec 1 isGraphicsPresentQueue)
 where
  isGraphicsPresentQueue queueFamilyIndex queueFamilyProperties =
    pure (isGraphicsQueueFamily queueFamilyProperties)
      <&&> isPresentQueueFamily phys surf queueFamilyIndex

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

deviceHasSwapChain :: MonadIO m => PhysicalDevice -> m Bool
deviceHasSwapChain dev = do
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
      let
        _ ::& (PhysicalDeviceTimelineSemaphoreFeatures hasTimelineSemaphores :& ())
          = feats
      pure hasTimelineSemaphores

  hasExt <&&> hasFeat

----------------------------------------------------------------
-- VulkanMemoryAllocator
----------------------------------------------------------------

createVMA
  :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA inst phys dev =
  snd
    <$> withAllocator
          zero { flags            = zero
               , physicalDevice   = physicalDeviceHandle phys
               , device           = deviceHandle dev
               , instance'        = instanceHandle inst
               , vulkanApiVersion = myApiVersion
               }
          allocate

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

noSuchThing :: MonadIO m => String -> m a
noSuchThing message =
  liftIO . throwIO $ IOError Nothing NoSuchThing "" message Nothing Nothing

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
