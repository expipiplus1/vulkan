{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Backend-independent triangle renderer using the recycling 'Frame' loop
-- from "Frame". Each backend (SDL2, GLFW) builds 'VkResources' + an initial
-- 'Swapchain', supplies callbacks for "current drawable size" and "should
-- quit", and hands off to 'runTriangle'.
module Triangle
  ( runTriangle
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits                      ( (.|.) )
import           Data.Foldable                  ( traverse_ )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )

import           Frame                          ( Frame(..)
                                                , advanceFrame
                                                , initialFrame
                                                , queueSubmitFrame
                                                , runFrame
                                                )
import qualified Framebuffer
import           Data.IORef
import           RefCounted                     ( releaseRefCounted )
import           Swapchain                      ( Swapchain(..)
                                                , recreateSwapchain
                                                , threwSwapchainError
                                                )
import           Utils                          ( loopJust )
import           VkResources                    ( Queues(..)
                                                , RecycledResources(..)
                                                , VkResources(..)
                                                )

import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withImage
                                                , createRenderPass
                                                )
import qualified Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Exception               ( VulkanException(..) )
import           Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR
                                                ( SurfaceFormatKHR(..) )
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           Vulkan.Utils.ShaderQQ.GLSL.Glslang
import           Vulkan.Zero

-- | Drive a recycling-Frame render loop drawing the colored triangle.
runTriangle
  :: VkResources
  -> Swapchain                       -- ^ Initial swapchain
  -> IO Extent2D                     -- ^ Get current drawable size (for resize)
  -> IO Bool                         -- ^ Per-frame poller; 'True' means quit
  -> ResourceT IO ()
runTriangle vr initialSC getDrawableSize shouldQuit = do
  let dev = vrDevice vr
  (_, renderPass) <- createRenderPass dev (SurfaceFormatKHR.format (sFormat initialSC))
  (_, pipeline)   <- createGraphicsPipeline dev renderPass
  initialFBs      <- Framebuffer.createFramebuffers dev renderPass (sImageViews initialSC) (sExtent initialSC)

  scRef  <- liftIO $ newIORef initialSC
  fbsRef <- liftIO $ newIORef initialFBs

  initial <- initialFrame vr initialSC

  let perFrame f = do
        currentSC          <- liftIO $ readIORef scRef
        (currentFBs, _rel) <- liftIO $ readIORef fbsRef
        let f' = f { fSwapchain = currentSC }
        needsNew <- threwSwapchainError $ liftIO $ runFrame vr f' $
          drawTriangle vr renderPass pipeline currentFBs f'
        sc' <- if needsNew
          then do
            newSize           <- liftIO getDrawableSize
            sc'               <- recreateSwapchain vr newSize currentSC
            newFBs            <- Framebuffer.createFramebuffers dev renderPass (sImageViews sc') (sExtent sc')
            (_oldFbs, oldRel) <- liftIO $ readIORef fbsRef
            releaseRefCounted oldRel
            liftIO $ writeIORef scRef  sc'
            liftIO $ writeIORef fbsRef newFBs
            pure sc'
          else pure currentSC
        advanceFrame vr sc' f'

      loop f = liftIO shouldQuit >>= \case
        True  -> do
          deviceWaitIdle dev
          pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle
  :: VkResources
  -> RenderPass
  -> Pipeline
  -> Vector Framebuffer
  -> Frame
  -> ResourceT IO ()
drawTriangle vr renderPass pipeline framebuffers f = do
  let RecycledResources {..} = fRecycled f
      sc                     = fSwapchain f
      dev                    = vrDevice vr
      Queues (_, gQ)         = vrQueues vr
      oneSecond              = 1e9

  (acquireResult, imageIndex) <-
    acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable NULL_HANDLE
      >>= \case
            r@(SUCCESS,        _) -> pure r
            r@(SUBOPTIMAL_KHR, _) -> pure r
            (TIMEOUT, _) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
            _            -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR

  (_, ~[commandBuffer]) <- withCommandBuffers
    dev
    zero { commandPool        = rrCommandPool
         , level              = COMMAND_BUFFER_LEVEL_PRIMARY
         , commandBufferCount = 1
         }
    allocate

  let renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = framebuffers V.! fromIntegral imageIndex
        , renderArea  = Rect2D { offset = zero, extent = sExtent sc }
        , clearValues = [Color (Float32 0.1 0.1 0.1 0)]
        }

  useCommandBuffer
      commandBuffer
      zero { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ do
        cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
          let Extent2D w h = sExtent sc
          cmdSetViewport commandBuffer 0
            [ Viewport { x        = 0
                       , y        = 0
                       , width    = realToFrac w
                       , height   = realToFrac h
                       , minDepth = 0
                       , maxDepth = 1
                       }
            ]
          cmdSetScissor commandBuffer 0
            [Rect2D { offset = Offset2D 0 0, extent = sExtent sc }]
          cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
          cmdDraw commandBuffer 3 1 0 0

  let submitInfo =
        zero { Vk.waitSemaphores  = [rrImageAvailable]
             , waitDstStageMask   = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
             , commandBuffers     = [commandBufferHandle commandBuffer]
             , signalSemaphores   = [rrRenderFinished, fHostTimeline f]
             }
          ::& zero { waitSemaphoreValues   = [1]
                   , signalSemaphoreValues = [1, fIndex f]
                   }
          :&  ()
  liftIO $ queueSubmitFrame gQ
                            f
                            [SomeStruct submitInfo]
                            (fHostTimeline f)
                            (fIndex f)

  presentResult <- queuePresentKHR
    gQ
    zero { Swap.waitSemaphores = [rrRenderFinished]
         , swapchains          = [sSwapchain sc]
         , imageIndices        = [imageIndex]
         }

  case (acquireResult, presentResult) of
    (SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    (_, SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    _                   -> pure ()

----------------------------------------------------------------
-- Render pass + pipeline (long-lived)
----------------------------------------------------------------

createRenderPass
  :: MonadResource m => Device -> Format -> m (ReleaseKey, RenderPass)
createRenderPass dev imageFormat = withRenderPass
  dev
  zero { attachments  = [attachmentDescription]
       , subpasses    = [subpass]
       , dependencies = [subpassDependency]
       }
  Nothing
  allocate
 where
  attachmentDescription :: AttachmentDescription
  attachmentDescription = zero
    { format         = imageFormat
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
      [zero { attachment = 0, layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL }]
    }
  subpassDependency :: SubpassDependency
  subpassDependency = zero
    { srcSubpass    = SUBPASS_EXTERNAL
    , dstSubpass    = 0
    , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , srcAccessMask = zero
    , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                        .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    }

createGraphicsPipeline
  :: (MonadResource m, MonadFail m)
  => Device
  -> RenderPass
  -> m (ReleaseKey, Pipeline)
createGraphicsPipeline dev renderPass = do
  (shaderKeys, shaderStages) <- V.unzip <$> createShaders dev
  (layoutKey , pipelineLayout) <- withPipelineLayout dev zero Nothing allocate
  let pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
      pipelineCreateInfo = zero
        { stages             = shaderStages
        , vertexInputState   = Just zero
        , inputAssemblyState = Just zero
                                 { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                                 , primitiveRestartEnable = False
                                 }
        , viewportState      = Just
          $ SomeStruct zero { viewportCount = 1, scissorCount = 1 }
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
                                       }
                                   ]
                                 }
        , dynamicState       = Just zero
                                 { dynamicStates = [ DYNAMIC_STATE_VIEWPORT
                                                   , DYNAMIC_STATE_SCISSOR
                                                   ]
                                 }
        , layout             = pipelineLayout
        , renderPass         = renderPass
        , subpass            = 0
        , basePipelineHandle = zero
        }
  (key, (_, [graphicsPipeline])) <- withGraphicsPipelines
    dev
    zero
    [SomeStruct pipelineCreateInfo]
    Nothing
    allocate
  release layoutKey
  traverse_ release shaderKeys
  pure (key, graphicsPipeline)

createShaders
  :: MonadResource m
  => Device
  -> m (V.Vector (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
  let fragCode = [frag|
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
  (fragKey, fragModule) <- withShaderModule dev zero { code = fragCode } Nothing allocate
  (vertKey, vertModule) <- withShaderModule dev zero { code = vertCode } Nothing allocate
  let vertShaderStageCreateInfo = zero { stage   = SHADER_STAGE_VERTEX_BIT
                                       , module' = vertModule
                                       , name    = "main"
                                       }
      fragShaderStageCreateInfo = zero { stage   = SHADER_STAGE_FRAGMENT_BIT
                                       , module' = fragModule
                                       , name    = "main"
                                       }
  pure
    [ (vertKey, SomeStruct vertShaderStageCreateInfo)
    , (fragKey, SomeStruct fragShaderStageCreateInfo)
    ]

