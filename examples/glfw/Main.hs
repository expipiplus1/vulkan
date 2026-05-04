{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import qualified Data.Text as Text
import Data.String (IsString)
import Data.Text.Encoding
import Data.Traversable
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as V
import Data.Word
import qualified Graphics.UI.GLFW as GLFW
import Say
import System.Exit
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SF
import Vulkan.Extensions.VK_KHR_swapchain
import qualified Vulkan.Extensions.VK_KHR_swapchain as SW
import Vulkan.Requirement (DeviceRequirement (..))
import qualified Vulkan.Utils.Init.GLFW as Init
import Vulkan.Utils.Initialization (createDeviceFromRequirements, pickPhysicalDevice)
import Vulkan.Utils.QueueAssignment
  ( QueueFamilyIndex (..)
  , QueueSpec (..)
  , assignQueues
  , isGraphicsQueueFamily
  , isPresentQueueFamily
  )
import Vulkan.Zero
import qualified Triangle
import qualified Window.GLFW as Window

main :: IO ()
main = runResourceT $ do
  Window.withGLFW
  VulkanWindow{..} <- withVulkanWindow windowWidth windowHeight
  liftIO $ Window.showWindow vwGlfwWindow
  Triangle.runTriangle
    vwDevice
    vwSwapchain
    vwFormat
    vwExtent
    vwImageViews
    vwGraphicsQueueFamilyIndex
    vwGraphicsQueue
    vwPresentQueue
    (Window.shouldQuit vwGlfwWindow)

data VulkanWindow = VulkanWindow
  { vwGlfwWindow :: GLFW.Window
  , vwDevice :: Device
  , vwSurface :: SurfaceKHR
  , vwSwapchain :: SwapchainKHR
  , vwExtent :: Extent2D
  , vwFormat :: Format
  , vwImageViews :: V.Vector ImageView
  , vwGraphicsQueue :: Queue
  , vwGraphicsQueueFamilyIndex :: Word32
  , vwPresentQueue :: Queue
  }

withVulkanWindow :: Int -> Int -> ResourceT IO VulkanWindow
withVulkanWindow width height = do
  window <- Window.createWindow (Text.pack appName) width height
  inst <- Init.withInstance
    window
    (Just zero{applicationName = Just appName, apiVersion = API_VERSION_1_0})
    []
    []
  surface <- Init.withSurface inst window
  (dev, graphicsQueue, graphicsQueueFamilyIndex, presentQueue, swapchainFormat, swapchainExtent, swapchain) <-
    createGraphicalDevice inst surface width height
  (_, images) <- getSwapchainImagesKHR dev swapchain
  let imageViewCreateInfo i =
        zero
          { image = i
          , viewType = IMAGE_VIEW_TYPE_2D
          , format = swapchainFormat
          , components =
              zero
                { r = COMPONENT_SWIZZLE_IDENTITY
                , g = COMPONENT_SWIZZLE_IDENTITY
                , b = COMPONENT_SWIZZLE_IDENTITY
                , a = COMPONENT_SWIZZLE_IDENTITY
                }
          , subresourceRange =
              zero
                { aspectMask = IMAGE_ASPECT_COLOR_BIT
                , baseMipLevel = 0
                , levelCount = 1
                , baseArrayLayer = 0
                , layerCount = 1
                }
          }
  imageViews <- for images $ \i ->
    snd <$> withImageView dev (imageViewCreateInfo i) Nothing allocate
  pure $ VulkanWindow
    window dev surface swapchain swapchainExtent swapchainFormat imageViews
    graphicsQueue graphicsQueueFamilyIndex presentQueue

appName :: (IsString a) => a
appName = "Haskell Vulkan triangle example (GLFW)"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

createGraphicalDevice
  :: Instance
  -> SurfaceKHR
  -> Int
  -> Int
  -> ResourceT IO (Device, Queue, Word32, Queue, Format, Extent2D, SwapchainKHR)
createGraphicalDevice inst surface width height = do
  let desiredFormat =
        SurfaceFormatKHR FORMAT_B8G8R8_UNORM COLOR_SPACE_SRGB_NONLINEAR_KHR
  (physicalDevice, graphicsQueueFamilyIndex, presentQueueFamilyIndex, surfaceFormat, presentMode, surfaceCaps, graphicsQueue, presentQueue, dev) <-
    pickGraphicalPhysicalDevice inst surface desiredFormat
  props <- getPhysicalDeviceProperties physicalDevice
  sayErr $ "Using device: " <> decodeUtf8 (deviceName props)
  let
    swapchainCreateInfo :: SwapchainCreateInfoKHR '[]
    swapchainCreateInfo =
      let (sharingMode, queueFamilyIndices) =
            if graphicsQueue == presentQueue
              then (SHARING_MODE_EXCLUSIVE, [])
              else
                ( SHARING_MODE_CONCURRENT
                , [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
                )
       in zero
            { surface = surface
            , minImageCount = SF.minImageCount surfaceCaps + 1
            , imageFormat = SF.format surfaceFormat
            , imageColorSpace = SF.colorSpace surfaceFormat
            , imageExtent = case currentExtent (surfaceCaps :: SurfaceCapabilitiesKHR) of
                Extent2D w h
                  | w == maxBound, h == maxBound ->
                      Extent2D (fromIntegral width) (fromIntegral height)
                e -> e
            , imageArrayLayers = 1
            , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            , imageSharingMode = sharingMode
            , queueFamilyIndices = queueFamilyIndices
            , preTransform = currentTransform (surfaceCaps :: SurfaceCapabilitiesKHR)
            , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
            , presentMode = presentMode
            , clipped = True
            }
  swapchain <- snd <$> withSwapchainKHR dev swapchainCreateInfo Nothing allocate
  pure
    ( dev
    , graphicsQueue
    , graphicsQueueFamilyIndex
    , presentQueue
    , SF.format surfaceFormat
    , SW.imageExtent swapchainCreateInfo
    , swapchain
    )

pickGraphicalPhysicalDevice
  :: Instance
  -> SurfaceKHR
  -> SurfaceFormatKHR
  -> ResourceT
      IO
      ( PhysicalDevice
      , Word32
      , Word32
      , SurfaceFormatKHR
      , PresentModeKHR
      , SurfaceCapabilitiesKHR
      , Queue
      , Queue
      , Device
      )
pickGraphicalPhysicalDevice inst surface desiredFormat = do
  mPd <- pickPhysicalDevice inst suitable id
  (_, phys) <- case mPd of
    Just x -> pure x
    Nothing -> sayErr "No suitable devices found" >> liftIO exitFailure
  bestFormat <- getFormat phys
  presentMode <- getPresentMode phys
  surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR phys surface
  let queueSpec = QueueSpec 1 $ \i q ->
        if isGraphicsQueueFamily q
          then isPresentQueueFamily phys surface i
          else pure False
  Just (qInfos, getQs) <- assignQueues phys (Identity queueSpec)
  let deviceReqs =
        [ RequireDeviceExtension Nothing e minBound
        | e <- Init.getRequiredDeviceExtensions
        ]
  dev <- createDeviceFromRequirements deviceReqs [] phys
    zero{queueCreateInfos = SomeStruct <$> qInfos}
  Identity (QueueFamilyIndex familyIdx, queue) <- liftIO (getQs dev)
  pure
    ( phys
    , familyIdx
    , familyIdx
    , bestFormat
    , presentMode
    , surfaceCaps
    , queue
    , queue
    , dev
    )
 where
  suitable :: PhysicalDevice -> ResourceT IO (Maybe Word64)
  suitable phys = runMaybeT $ do
    (_, exts) <- enumerateDeviceExtensionProperties phys Nothing
    guard (V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) exts)
    qProps <- getPhysicalDeviceQueueFamilyProperties phys
    guard (V.any isGraphicsQueueFamily qProps)
    let presentSupport i =
          isPresentQueueFamily phys surface (QueueFamilyIndex (fromIntegral i))
    hasPresent <- V.or <$> V.imapM (\i _ -> presentSupport i) qProps
    guard hasPresent
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure (sum $ DI.size <$> heaps)

  headMay = \case
    [] -> Nothing
    xs -> Just (V.unsafeHead xs)

  getFormat :: (MonadIO m) => PhysicalDevice -> m SurfaceFormatKHR
  getFormat dev = do
    (_, formats) <- getPhysicalDeviceSurfaceFormatsKHR dev surface
    pure $ case formats of
      [] -> desiredFormat
      [SurfaceFormatKHR FORMAT_UNDEFINED _] -> desiredFormat
      _
        | V.any
            ( \f ->
                SF.format f == SF.format desiredFormat
                  && SF.colorSpace f == SF.colorSpace desiredFormat
            )
            formats ->
            desiredFormat
      _ -> V.head formats

  getPresentMode :: (MonadIO m) => PhysicalDevice -> m PresentModeKHR
  getPresentMode dev = do
    (_, presentModes) <- getPhysicalDeviceSurfacePresentModesKHR dev surface
    let desiredPresentModes =
          [ PRESENT_MODE_MAILBOX_KHR
          , PRESENT_MODE_FIFO_KHR
          , PRESENT_MODE_IMMEDIATE_KHR
          ]
        match = V.filter (`V.elem` presentModes) desiredPresentModes
    pure $ case headMay match of
      Just m  -> m
      Nothing -> case presentModes V.!? 0 of
        Just m  -> m
        Nothing -> PRESENT_MODE_FIFO_KHR
