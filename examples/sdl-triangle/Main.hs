{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Ord
import Data.String (IsString)
import Data.Text.Encoding
import Data.Traversable
import qualified Data.Vector as V
import Data.Word
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Say
import System.Exit
import Vulkan.CStruct.Extends
import Vulkan.Core10
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_KHR_portability_enumeration
import Vulkan.Extensions.VK_KHR_surface
import qualified Vulkan.Extensions.VK_KHR_surface as SF
import Vulkan.Extensions.VK_KHR_swapchain
import qualified Vulkan.Extensions.VK_KHR_swapchain as SW
import Vulkan.Requirement (InstanceRequirement (..))
import Vulkan.Utils.Debug
import Vulkan.Utils.Initialization (createDebugInstanceFromRequirements)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang
  ( frag
  , vert
  )
import Vulkan.Zero
import qualified Window

main :: IO ()
main = runResourceT $ do
  Window.withSDL

  VulkanWindow{..} <- withVulkanWindow windowWidth windowHeight
  renderPass <- Main.createRenderPass vwDevice vwFormat
  graphicsPipeline <-
    createGraphicsPipeline
      vwDevice
      renderPass
      vwExtent
      vwFormat
  framebuffers <- createFramebuffers vwDevice vwImageViews renderPass vwExtent
  commandBuffers <-
    createCommandBuffers
      vwDevice
      renderPass
      graphicsPipeline
      vwGraphicsQueueFamilyIndex
      framebuffers
      vwExtent
  (imageAvailableSemaphore, renderFinishedSemaphore) <-
    createSemaphores
      vwDevice
  SDL.showWindow vwSdlWindow
  liftIO $
    mainLoop $
      drawFrame
        vwDevice
        vwSwapchain
        vwGraphicsQueue
        vwPresentQueue
        imageAvailableSemaphore
        renderFinishedSemaphore
        commandBuffers
  deviceWaitIdle vwDevice

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- Window.shouldQuit Window.NoLimit
  if quit
    then pure False
    else do
      draw
      pure True

drawFrame
  :: Device
  -> SwapchainKHR
  -> Queue
  -> Queue
  -> Semaphore
  -> Semaphore
  -> V.Vector CommandBuffer
  -> IO ()
drawFrame dev swapchain graphicsQueue presentQueue imageAvailableSemaphore renderFinishedSemaphore commandBuffers = do
  (_, imageIndex) <-
    acquireNextImageKHR
      dev
      swapchain
      maxBound
      imageAvailableSemaphore
      zero
  let
    submitInfo =
      zero
        { waitSemaphores = [imageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers =
            [ commandBufferHandle $
                commandBuffers
                  V.! fromIntegral imageIndex
            ]
        , signalSemaphores = [renderFinishedSemaphore]
        }
    presentInfo =
      zero
        { waitSemaphores = [renderFinishedSemaphore]
        , swapchains = [swapchain]
        , imageIndices = [imageIndex]
        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  _ <- queuePresentKHR presentQueue presentInfo
  pure ()

createSemaphores :: Device -> ResourceT IO (Semaphore, Semaphore)
createSemaphores dev = do
  imageAvailableSemaphore <- withSemaphore dev zero Nothing allocate'
  renderFinishedSemaphore <- withSemaphore dev zero Nothing allocate'
  pure (imageAvailableSemaphore, renderFinishedSemaphore)

createCommandBuffers
  :: Device
  -> RenderPass
  -> Pipeline
  -> Word32
  -> V.Vector Framebuffer
  -> Extent2D
  -> ResourceT IO (V.Vector CommandBuffer)
createCommandBuffers dev renderPass graphicsPipeline graphicsQueueFamilyIndex framebuffers swapchainExtent = do
  let commandPoolCreateInfo = zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex}
  commandPool <- withCommandPool dev commandPoolCreateInfo Nothing allocate'
  let
    commandBufferAllocateInfo :: CommandBufferAllocateInfo
    commandBufferAllocateInfo =
      zero
        { commandPool = commandPool
        , level = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral $ V.length framebuffers
        }
    cbFlags = zero{CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT}
  buffers <- withCommandBuffers dev commandBufferAllocateInfo allocate'
  _ <- liftIO . for (V.zip framebuffers buffers) $ \(framebuffer, buffer) ->
    useCommandBuffer buffer cbFlags $ do
      let renderPassBeginInfo =
            zero
              { renderPass = renderPass
              , framebuffer = framebuffer
              , renderArea =
                  Rect2D
                    { offset = zero
                    , extent = swapchainExtent
                    }
              , clearValues = [Color (Float32 0.1 0.1 0.1 0)]
              }
      cmdUseRenderPass buffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
        cmdBindPipeline
          buffer
          PIPELINE_BIND_POINT_GRAPHICS
          graphicsPipeline
        cmdDraw buffer 3 1 0 0
  pure buffers

createShaders
  :: Device -> ResourceT IO (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
  let
    fragCode =
      [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;

        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]
    vertCode =
      [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) out vec3 fragColor;

        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );

        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );

        void main() {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor = colors[gl_VertexIndex];
        }
      |]
  fragModule <- withShaderModule dev zero{code = fragCode} Nothing allocate'
  vertModule <- withShaderModule dev zero{code = vertCode} Nothing allocate'
  let
    vertShaderStageCreateInfo =
      zero
        { stage = SHADER_STAGE_VERTEX_BIT
        , module' = vertModule
        , name = "main"
        }
    fragShaderStageCreateInfo =
      zero
        { stage = SHADER_STAGE_FRAGMENT_BIT
        , module' = fragModule
        , name = "main"
        }
  pure
    [SomeStruct vertShaderStageCreateInfo, SomeStruct fragShaderStageCreateInfo]

createRenderPass :: Device -> Format -> ResourceT IO RenderPass
createRenderPass dev swapchainImageFormat = do
  let
    attachmentDescription :: AttachmentDescription
    attachmentDescription =
      zero
        { format = swapchainImageFormat
        , samples = SAMPLE_COUNT_1_BIT
        , loadOp = ATTACHMENT_LOAD_OP_CLEAR
        , storeOp = ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout = IMAGE_LAYOUT_UNDEFINED
        , finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
    subpass :: SubpassDescription
    subpass =
      zero
        { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
        , colorAttachments =
            [ zero
                { attachment = 0
                , layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        }
    subpassDependency :: SubpassDependency
    subpassDependency =
      zero
        { srcSubpass = SUBPASS_EXTERNAL
        , dstSubpass = 0
        , srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , srcAccessMask = zero
        , dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , dstAccessMask =
            ACCESS_COLOR_ATTACHMENT_READ_BIT
              .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        }
  withRenderPass
    dev
    zero
      { attachments = [attachmentDescription]
      , subpasses = [subpass]
      , dependencies = [subpassDependency]
      }
    Nothing
    allocate'

createGraphicsPipeline
  :: Device -> RenderPass -> Extent2D -> Format -> ResourceT IO Pipeline
createGraphicsPipeline dev renderPass swapchainExtent _swapchainImageFormat = do
  shaderStages <- createShaders dev
  pipelineLayout <- withPipelineLayout dev zero Nothing allocate'
  let
    Extent2D{width = swapchainWidth, height = swapchainHeight} = swapchainExtent
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { stages = shaderStages
        , vertexInputState = Just zero
        , inputAssemblyState =
            Just
              zero
                { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                , primitiveRestartEnable = False
                }
        , viewportState =
            Just . SomeStruct $
              zero
                { viewports =
                    [ Viewport
                        { x = 0
                        , y = 0
                        , width = realToFrac swapchainWidth
                        , height = realToFrac swapchainHeight
                        , minDepth = 0
                        , maxDepth = 1
                        }
                    ]
                , scissors =
                    [Rect2D{offset = Offset2D 0 0, extent = swapchainExtent}]
                }
        , rasterizationState =
            Just . SomeStruct $
              zero
                { depthClampEnable = False
                , rasterizerDiscardEnable = False
                , lineWidth = 1
                , polygonMode = POLYGON_MODE_FILL
                , cullMode = CULL_MODE_NONE
                , frontFace = FRONT_FACE_CLOCKWISE
                , depthBiasEnable = False
                }
        , multisampleState =
            Just . SomeStruct $
              zero
                { sampleShadingEnable = False
                , rasterizationSamples = SAMPLE_COUNT_1_BIT
                , minSampleShading = 1
                , sampleMask = [maxBound]
                }
        , depthStencilState = Nothing
        , colorBlendState =
            Just . SomeStruct $
              zero
                { logicOpEnable = False
                , attachments =
                    [ zero
                        { colorWriteMask =
                            COLOR_COMPONENT_R_BIT
                              .|. COLOR_COMPONENT_G_BIT
                              .|. COLOR_COMPONENT_B_BIT
                              .|. COLOR_COMPONENT_A_BIT
                        , blendEnable = False
                        }
                    ]
                }
        , dynamicState = Nothing
        , layout = pipelineLayout
        , renderPass = renderPass
        , subpass = 0
        , basePipelineHandle = zero
        }
  V.head . snd <$> withGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate'

createFramebuffers
  :: Device
  -> V.Vector ImageView
  -> RenderPass
  -> Extent2D
  -> ResourceT IO (V.Vector Framebuffer)
createFramebuffers dev imageViews renderPass Extent2D{width, height} =
  for imageViews $ \imageView -> do
    let
      framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo =
        zero
          { renderPass = renderPass
          , attachments = [imageView]
          , width
          , height
          , layers = 1
          }
    withFramebuffer dev framebufferCreateInfo Nothing allocate'

data VulkanWindow = VulkanWindow
  { vwSdlWindow :: SDL.Window
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
  window <- Window.createWindow appName width height
  windowExtensions <- liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions window

  let
    instanceCreateInfo =
      zero
        { applicationInfo =
            Just
              zero
                { applicationName = Just appName
                , apiVersion = API_VERSION_1_0
                }
        , enabledExtensionNames = V.fromList windowExtensions
        , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
        }
    portabilityEnum =
      RequireInstanceExtension
        Nothing
        KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
        minBound
  inst <- createDebugInstanceFromRequirements [] [portabilityEnum] instanceCreateInfo
  void $
    withDebugUtilsMessengerEXT
      inst
      debugUtilsMessengerCreateInfo
      Nothing
      allocate'
  submitDebugUtilsMessageEXT
    inst
    DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    zero{message = "Debug Message Test"}
  (_, surface) <- Window.createSurface inst window
  (dev, graphicsQueue, graphicsQueueFamilyIndex, presentQueue, swapchainFormat, swapchainExtent, swapchain) <-
    createGraphicalDevice inst surface
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
  imageViews <- for images $
    \i -> withImageView dev (imageViewCreateInfo i) Nothing allocate'
  pure $
    VulkanWindow
      window
      dev
      surface
      swapchain
      swapchainExtent
      swapchainFormat
      imageViews
      graphicsQueue
      graphicsQueueFamilyIndex
      presentQueue

appName :: (IsString a) => a
appName = "Haskell Vulkan triangle example"

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

createGraphicalDevice
  :: Instance
  -> SurfaceKHR
  -> ResourceT
      IO
      (Device, Queue, Word32, Queue, Format, Extent2D, SwapchainKHR)
createGraphicalDevice inst surface = do
  let requiredDeviceExtensions = [KHR_SWAPCHAIN_EXTENSION_NAME]
  (physicalDevice, graphicsQueueFamilyIndex, presentQueueFamilyIndex, surfaceFormat, presentMode, surfaceCaps) <-
    pickGraphicalPhysicalDevice
      inst
      surface
      requiredDeviceExtensions
      (SurfaceFormatKHR FORMAT_B8G8R8_UNORM COLOR_SPACE_SRGB_NONLINEAR_KHR)
  props <- getPhysicalDeviceProperties physicalDevice
  sayErr $ "Using device: " <> decodeUtf8 (deviceName props)
  let
    deviceCreateInfo :: DeviceCreateInfo '[]
    deviceCreateInfo =
      zero
        { queueCreateInfos =
            V.fromList
              [ SomeStruct $ zero{queueFamilyIndex = i, queuePriorities = [1]}
              | i <- nub [graphicsQueueFamilyIndex, presentQueueFamilyIndex]
              ]
        , enabledExtensionNames = requiredDeviceExtensions
        }
  dev <- withDevice physicalDevice deviceCreateInfo Nothing allocate'
  graphicsQueue <- getDeviceQueue dev graphicsQueueFamilyIndex 0
  presentQueue <- getDeviceQueue dev presentQueueFamilyIndex 0
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
          , imageExtent = case currentExtent
              (surfaceCaps :: SurfaceCapabilitiesKHR) of
              Extent2D w h
                | w == maxBound
                , h == maxBound ->
                    Extent2D
                      (fromIntegral windowWidth)
                      (fromIntegral windowHeight)
              e -> e
          , imageArrayLayers = 1
          , imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          , imageSharingMode = sharingMode
          , queueFamilyIndices = queueFamilyIndices
          , preTransform =
              currentTransform
                (surfaceCaps :: SurfaceCapabilitiesKHR)
          , compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR
          , presentMode = presentMode
          , clipped = True
          }
  swapchain <- withSwapchainKHR dev swapchainCreateInfo Nothing allocate'
  pure
    ( dev
    , graphicsQueue
    , graphicsQueueFamilyIndex
    , presentQueue
    , SF.format surfaceFormat
    , SW.imageExtent swapchainCreateInfo
    , swapchain
    )

-- | Find the device which has the most memory and a graphics queue family index
pickGraphicalPhysicalDevice
  :: (MonadIO m)
  => Instance
  -> SurfaceKHR
  -> V.Vector BS.ByteString
  -> SurfaceFormatKHR
  -> m
      ( PhysicalDevice
      , Word32
      , Word32
      , SurfaceFormatKHR
      , PresentModeKHR
      , SurfaceCapabilitiesKHR
      )
pickGraphicalPhysicalDevice inst surface _requiredExtensions desiredFormat = do
  (_, devs) <- enumeratePhysicalDevices inst
  -- All devices with support for all the graphical features we want
  graphicsDevs <- fmap (V.mapMaybe id) . for devs $ \dev -> runMaybeT $ do
    graphicsQueue <- MaybeT $ headMay <$> getGraphicsQueueIndices dev
    presentQueue <- MaybeT $ headMay <$> getPresentQueueIndices dev
    guard =<< deviceHasSwapchain dev
    bestFormat <- getFormat dev
    presentMode <- getPresentMode dev
    surfaceCaps <- getPhysicalDeviceSurfaceCapabilitiesKHR dev surface
    score <- deviceScore dev
    pure
      ( score
      , (dev, graphicsQueue, presentQueue, bestFormat, presentMode, surfaceCaps)
      )
  if V.null graphicsDevs
    then do
      sayErr "No suitable devices found"
      liftIO exitFailure
    else pure . snd . V.maximumBy (comparing fst) $ graphicsDevs
  where
    headMay = \case
      [] -> Nothing
      xs -> Just (V.unsafeHead xs)

    deviceScore :: (MonadIO m) => PhysicalDevice -> m Word64
    deviceScore dev = do
      heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties dev
      let totalSize = sum $ DI.size <$> heaps
      pure totalSize

    deviceHasSwapchain :: (MonadIO m) => PhysicalDevice -> m Bool
    deviceHasSwapchain dev = do
      (_, extensions) <- enumerateDeviceExtensionProperties dev Nothing
      pure $ V.any ((KHR_SWAPCHAIN_EXTENSION_NAME ==) . extensionName) extensions

    getGraphicsQueueIndices :: (MonadIO m) => PhysicalDevice -> m (V.Vector Word32)
    getGraphicsQueueIndices dev = do
      queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties dev
      let
        isGraphicsQueue q =
          (QUEUE_GRAPHICS_BIT .&&. queueFlags q) && (queueCount q > 0)
        graphicsQueueIndices =
          fromIntegral . fst
            <$> V.filter
              (isGraphicsQueue . snd)
              (V.indexed queueFamilyProperties)
      pure graphicsQueueIndices

    getPresentQueueIndices :: (MonadIO m) => PhysicalDevice -> m (V.Vector Word32)
    getPresentQueueIndices dev = do
      queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties dev
      let isPresentQueue i = getPhysicalDeviceSurfaceSupportKHR dev i surface
      V.filterM
        isPresentQueue
        (V.generate (V.length queueFamilyProperties) fromIntegral)

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

    getPresentMode :: (MonadIO m) => PhysicalDevice -> MaybeT m PresentModeKHR
    getPresentMode dev = do
      (_, presentModes) <- getPhysicalDeviceSurfacePresentModesKHR dev surface
      let desiredPresentModes =
            [ PRESENT_MODE_MAILBOX_KHR
            , PRESENT_MODE_FIFO_KHR
            , PRESENT_MODE_IMMEDIATE_KHR
            ]
      MaybeT
        . pure
        . headMay
        . V.filter (`V.elem` presentModes)
        $ desiredPresentModes

----------------------------------------------------------------
-- Debugging
----------------------------------------------------------------

debugUtilsMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo =
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

----------------------------------------------------------------
-- Resource handling with 'resourcet'
----------------------------------------------------------------

allocate' :: IO a -> (a -> IO ()) -> ResourceT IO a
allocate' c d = snd <$> allocate c d

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

(.&&.) :: (Bits a) => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
