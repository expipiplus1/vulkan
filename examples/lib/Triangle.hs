{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Backend-independent triangle renderer used by both the SDL2 and GLFW
-- triangle examples. Sets up a render pass, graphics pipeline, framebuffers,
-- command buffers and semaphores, then runs a render loop until the supplied
-- @shouldQuit@ poller returns 'True'.
module Triangle
  ( runTriangle
  ) where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Resource   ( ResourceT, allocate )
import           Data.Bits                      ( (.|.) )
import           Data.Traversable               ( for )
import qualified Data.Vector                   as V
import           Data.Word                      ( Word32 )

import           Vulkan.CStruct.Extends         ( SomeStruct(..) )
import           Vulkan.Core10           hiding ( createRenderPass )
import qualified Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import qualified Vulkan.Core10                 as CommandPoolCreateInfo  (CommandPoolCreateInfo(..))
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.ShaderQQ.GLSL.Glslang
                                                ( frag, vert )
import           Vulkan.Zero                    ( zero )

import           Window                         ( VulkanWindow(..) )

-- | Render a static triangle into the swapchain inside the given
-- 'VulkanWindow' until @shouldQuit@ reports 'True'.
runTriangle
  :: VulkanWindow w
  -> IO Bool             -- ^ Per-frame poller; 'True' = exit
  -> ResourceT IO ()
runTriangle VulkanWindow{..} shouldQuit = do
  renderPass       <- createRenderPass vwDevice vwFormat
  graphicsPipeline <- createGraphicsPipeline vwDevice renderPass vwExtent
  framebuffers     <- createFramebuffers vwDevice vwImageViews renderPass vwExtent
  commandBuffers   <- createCommandBuffers vwDevice renderPass graphicsPipeline vwGraphicsQueueFamilyIndex framebuffers vwExtent
  (imageAvailableSemaphore, renderFinishedSemaphore) <- createSemaphores vwDevice
  liftIO $ mainLoop $
    drawFrame vwDevice vwSwapchain vwGraphicsQueue vwPresentQueue imageAvailableSemaphore renderFinishedSemaphore commandBuffers
  deviceWaitIdle vwDevice
 where
  mainLoop draw = do
    quit <- shouldQuit
    if quit then pure () else draw >> mainLoop draw

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
  (_, imageIndex) <- acquireNextImageKHR dev swapchain maxBound imageAvailableSemaphore zero
  let submitInfo = zero
        { waitSemaphores   = [imageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers   = [commandBufferHandle (commandBuffers V.! fromIntegral imageIndex)]
        , signalSemaphores = [renderFinishedSemaphore]
        }
      presentInfo = zero
        { waitSemaphores = [renderFinishedSemaphore]
        , swapchains     = [swapchain]
        , imageIndices   = [imageIndex]
        }
  queueSubmit graphicsQueue [SomeStruct submitInfo] zero
  _ <- queuePresentKHR presentQueue presentInfo
  pure ()

allocate' :: IO a -> (a -> IO ()) -> ResourceT IO a
allocate' c d = snd <$> allocate c d

createSemaphores :: Device -> ResourceT IO (Semaphore, Semaphore)
createSemaphores dev = do
  imageAvailableSemaphore <- withSemaphore dev zero Nothing allocate'
  renderFinishedSemaphore <- withSemaphore dev zero Nothing allocate'
  pure (imageAvailableSemaphore, renderFinishedSemaphore)

createCommandBuffers
  :: Device -> RenderPass -> Pipeline -> Word32 -> V.Vector Framebuffer -> Extent2D
  -> ResourceT IO (V.Vector CommandBuffer)
createCommandBuffers dev renderPass graphicsPipeline graphicsQueueFamilyIndex framebuffers swapchainExtent = do
  let commandPoolCreateInfo = zero { CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex }
  commandPool <- withCommandPool dev commandPoolCreateInfo Nothing allocate'
  let commandBufferAllocateInfo = zero
        { commandPool        = commandPool
        , level              = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = fromIntegral $ V.length framebuffers
        }
      cbFlags = zero { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT }
  buffers <- withCommandBuffers dev commandBufferAllocateInfo allocate'
  liftIO . V.forM_ (V.zip framebuffers buffers) $ \(framebuffer, buffer) ->
    useCommandBuffer buffer cbFlags $ do
      let renderPassBeginInfo = zero
            { renderPass  = renderPass
            , framebuffer = framebuffer
            , renderArea  = Rect2D { offset = zero, extent = swapchainExtent }
            , clearValues = [Color (Float32 0.1 0.1 0.1 0)]
            }
      cmdUseRenderPass buffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
        cmdBindPipeline buffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        cmdDraw buffer 3 1 0 0
  pure buffers

createShaders
  :: Device -> ResourceT IO (V.Vector (SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
  let
    fragCode = [frag|
      #version 450
      #extension GL_ARB_separate_shader_objects : enable

      layout(location = 0) in vec3 fragColor;
      layout(location = 0) out vec4 outColor;

      void main() {
          outColor = vec4(fragColor, 1.0);
      }
    |]
    vertCode = [vert|
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
        fragColor   = colors[gl_VertexIndex];
      }
    |]
  fragModule <- withShaderModule dev zero { code = fragCode } Nothing allocate'
  vertModule <- withShaderModule dev zero { code = vertCode } Nothing allocate'
  let vertShaderStageCreateInfo = zero
        { stage   = SHADER_STAGE_VERTEX_BIT
        , module' = vertModule
        , name    = "main"
        }
      fragShaderStageCreateInfo = zero
        { stage   = SHADER_STAGE_FRAGMENT_BIT
        , module' = fragModule
        , name    = "main"
        }
  pure [SomeStruct vertShaderStageCreateInfo, SomeStruct fragShaderStageCreateInfo]

createRenderPass :: Device -> Format -> ResourceT IO RenderPass
createRenderPass dev swapchainImageFormat = do
  let attachmentDescription :: AttachmentDescription
      attachmentDescription = zero
        { format         = swapchainImageFormat
        , samples        = SAMPLE_COUNT_1_BIT
        , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
        , storeOp        = ATTACHMENT_STORE_OP_STORE
        , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
        , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
        , initialLayout  = IMAGE_LAYOUT_UNDEFINED
        , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
      subpass :: SubpassDescription
      subpass = zero
        { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
        , colorAttachments  =
            [ zero { attachment = 0, layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL } ]
        }
      subpassDependency :: SubpassDependency
      subpassDependency = zero
        { srcSubpass    = SUBPASS_EXTERNAL
        , dstSubpass    = 0
        , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , srcAccessMask = zero
        , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        }
  withRenderPass dev zero
    { attachments  = [attachmentDescription]
    , subpasses    = [subpass]
    , dependencies = [subpassDependency]
    } Nothing allocate'

createGraphicsPipeline
  :: Device -> RenderPass -> Extent2D -> ResourceT IO Pipeline
createGraphicsPipeline dev renderPass swapchainExtent = do
  shaderStages   <- createShaders dev
  pipelineLayout <- withPipelineLayout dev zero Nothing allocate'
  let Extent2D { width = swapchainWidth, height = swapchainHeight } = swapchainExtent
      pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
      pipelineCreateInfo = zero
        { stages             = shaderStages
        , vertexInputState   = Just zero
        , inputAssemblyState = Just zero
            { topology               = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            , primitiveRestartEnable = False
            }
        , viewportState      = Just . SomeStruct $ zero
            { viewports = [ Viewport
                              { x        = 0
                              , y        = 0
                              , width    = realToFrac swapchainWidth
                              , height   = realToFrac swapchainHeight
                              , minDepth = 0
                              , maxDepth = 1
                              } ]
            , scissors  = [ Rect2D { offset = Offset2D 0 0, extent = swapchainExtent } ]
            }
        , rasterizationState = Just . SomeStruct $ zero
            { depthClampEnable        = False
            , rasterizerDiscardEnable = False
            , lineWidth               = 1
            , polygonMode             = POLYGON_MODE_FILL
            , cullMode                = CULL_MODE_NONE
            , frontFace               = FRONT_FACE_CLOCKWISE
            , depthBiasEnable         = False
            }
        , multisampleState   = Just . SomeStruct $ zero
            { sampleShadingEnable  = False
            , rasterizationSamples = SAMPLE_COUNT_1_BIT
            , minSampleShading     = 1
            , sampleMask           = [maxBound]
            }
        , depthStencilState  = Nothing
        , colorBlendState    = Just . SomeStruct $ zero
            { logicOpEnable = False
            , attachments   =
                [ zero
                    { colorWriteMask =
                          COLOR_COMPONENT_R_BIT
                      .|. COLOR_COMPONENT_G_BIT
                      .|. COLOR_COMPONENT_B_BIT
                      .|. COLOR_COMPONENT_A_BIT
                    , blendEnable    = False
                    } ]
            }
        , dynamicState       = Nothing
        , layout             = pipelineLayout
        , renderPass         = renderPass
        , subpass            = 0
        , basePipelineHandle = zero
        }
  V.head . snd <$> withGraphicsPipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate'

createFramebuffers
  :: Device -> V.Vector ImageView -> RenderPass -> Extent2D
  -> ResourceT IO (V.Vector Framebuffer)
createFramebuffers dev imageViews renderPass Extent2D { width, height } =
  for imageViews $ \imageView -> do
    let framebufferCreateInfo :: FramebufferCreateInfo '[]
        framebufferCreateInfo = zero
          { renderPass  = renderPass
          , attachments = [imageView]
          , width       = width
          , height      = height
          , layers      = 1
          }
    withFramebuffer dev framebufferCreateInfo Nothing allocate'
