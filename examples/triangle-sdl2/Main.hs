{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Functor.Identity          ( Identity(..) )
import           Data.String                    ( IsString )
import           Data.Word                      ( Word32 )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Data.Vector                   as V
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           Say
import qualified Triangle
import qualified Vma
import           VkResources                    ( Queues(..)
                                                , mkVkResources
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import qualified Vulkan.Core10.DeviceInitialization as DI
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Requirement             ( DeviceRequirement(..) )
import qualified Vulkan.Utils.Init.SDL2        as Init
import           Vulkan.Utils.Initialization    ( createDeviceFromRequirements
                                                , pickPhysicalDevice
                                                )
import           Vulkan.Utils.QueueAssignment   ( QueueFamilyIndex(..)
                                                , QueueSpec(..)
                                                , assignQueues
                                                , isGraphicsQueueFamily
                                                , isPresentQueueFamily
                                                )
import           Vulkan.Zero
import           Frame                          ( frameDeviceRequirements
                                                , frameInstanceRequirements
                                                )
import           Swapchain                      ( allocSwapchain )
import qualified Window.SDL2                   as Window

main :: IO ()
main = runResourceT $ do
  Window.withSDL
  window <- Window.createWindow appName windowWidth windowHeight
  inst   <- Init.withInstance
    window
    (Just zero { applicationName = Just appName, apiVersion = API_VERSION_1_0 })
    frameInstanceRequirements
    []
  surface                       <- Init.withSurface inst window
  (phys, dev, qfi, gQueue)      <- createGraphicalDevice inst surface
  vma                           <- Vma.createVMA zero API_VERSION_1_0 inst phys dev
  props                         <- getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (deviceName props)

  let qs = Queues (QueueFamilyIndex qfi, gQueue)
  vr <- liftIO $ mkVkResources inst phys dev vma qs

  initialSize <- liftIO $ drawableSize window
  initialSC   <- allocSwapchain vr NULL_HANDLE initialSize surface

  SDL.showWindow window
  Triangle.runTriangle vr initialSC (drawableSize window) (Window.shouldQuit Window.NoLimit)

appName :: IsString a => a
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

drawableSize :: SDL.Window -> IO Extent2D
drawableSize win = do
  SDL.V2 w h <- SDL.vkGetDrawableSize win
  pure $ Extent2D (fromIntegral w) (fromIntegral h)

----------------------------------------------------------------
-- Pick a device with a unified graphics+present queue.
----------------------------------------------------------------

createGraphicalDevice
  :: (MonadResource m, MonadFail m)
  => Instance
  -> SurfaceKHR
  -> m (PhysicalDevice, Device, Word32, Queue)
createGraphicalDevice inst surface = do
  mPd <- pickPhysicalDevice inst suitable id
  (_, phys) <- maybe (sayErr "No suitable devices found" >> error "no GPU") pure mPd

  -- One queue family with both graphics and present capability.
  let queueSpec = QueueSpec 1 $ \i q ->
        if isGraphicsQueueFamily q
          then isPresentQueueFamily phys surface i
          else pure False
  Just (qInfos, getQs) <- assignQueues phys (Identity queueSpec)

  let deviceReqs =
        [ RequireDeviceExtension Nothing e minBound
        | e <- Init.getRequiredDeviceExtensions
        ] ++ frameDeviceRequirements
  dev <- createDeviceFromRequirements deviceReqs [] phys
    zero { queueCreateInfos = SomeStruct <$> qInfos }
  Identity (QueueFamilyIndex familyIdx, queue) <- liftIO (getQs dev)
  pure (phys, dev, familyIdx, queue)
 where
  suitable phys = runMaybeT $ do
    (_, exts) <- enumerateDeviceExtensionProperties phys Nothing
    True <- pure $ V.any ((Init.getRequiredDeviceExtensions !! 0 ==) . extensionName) exts
    qProps <- getPhysicalDeviceQueueFamilyProperties phys
    True <- pure $ V.any isGraphicsQueueFamily qProps
    let presentSupport i =
          isPresentQueueFamily phys surface (QueueFamilyIndex (fromIntegral i))
    hasPresent <- V.or <$> V.imapM (\i _ -> presentSupport i) qProps
    True <- pure hasPresent
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure (sum $ DI.size <$> heaps)
