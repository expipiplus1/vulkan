{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-| Backend-independent triangle renderer using the recycling 'Frame' loop
from "Frame". Each backend (SDL2, GLFW) builds 'VkResources' + an initial
'Swapchain', supplies callbacks for "current drawable size" and "should
quit", and hands off to 'runTriangle'.
-}
module Triangle
  ( runTriangle
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResourceT, allocate, release)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import Frame (Frame (..), advanceFrame, initialFrame, queueSubmitFrame, runFrame)
import qualified Framebuffer
import RefCounted (releaseRefCounted)
import Swapchain (Swapchain (..), recreateSwapchain, threwSwapchainError)
import Utils (loopJust)
import VkResources (Queues (..), RecycledResources (..), VkResources (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core12 as Vk12
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import qualified Vulkan.Extensions.VK_KHR_swapchain as KHR
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Zero (zero)

-- | Drive a recycling-Frame render loop drawing the colored triangle.
runTriangle
  :: VkResources
  -> Swapchain
  -- ^ Initial swapchain
  -> IO Vk.Extent2D
  -- ^ Get current drawable size (for resize)
  -> IO Bool
  -- ^ Per-frame poller; 'True' means quit
  -> ResourceT IO ()
runTriangle vr initialSC getDrawableSize shouldQuit = do
  let dev = vrDevice vr
  (_, renderPass) <- createRenderPass dev (KHR.format (sFormat initialSC))
  (_, pipeline) <- createGraphicsPipeline dev renderPass
  initialFBs <- Framebuffer.createFramebuffers dev renderPass (sImageViews initialSC) (sExtent initialSC)

  scRef <- liftIO $ newIORef initialSC
  fbsRef <- liftIO $ newIORef initialFBs

  initial <- initialFrame vr initialSC

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      (currentFBs, _rel) <- liftIO $ readIORef fbsRef
      let f' = f{fSwapchain = currentSC}
      needsNew <-
        threwSwapchainError $
          liftIO $
            runFrame vr f' $
              drawTriangle vr renderPass pipeline currentFBs f'
      sc' <-
        if needsNew
          then do
            newSize <- liftIO getDrawableSize
            sc' <- recreateSwapchain vr newSize currentSC
            newFBs <- Framebuffer.createFramebuffers dev renderPass (sImageViews sc') (sExtent sc')
            (_oldFbs, oldRel) <- liftIO $ readIORef fbsRef
            releaseRefCounted oldRel
            liftIO $ writeIORef scRef sc'
            liftIO $ writeIORef fbsRef newFBs
            pure sc'
          else pure currentSC
      advanceFrame vr sc' f'

    loop f =
      liftIO shouldQuit >>= \case
        True -> do
          Vk.deviceWaitIdle dev
          pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle
  :: VkResources
  -> Vk.RenderPass
  -> Vk.Pipeline
  -> Vector Vk.Framebuffer
  -> Frame
  -> ResourceT IO ()
drawTriangle vr renderPass pipeline framebuffers f = do
  let
    RecycledResources{..} = fRecycled f
    sc = fSwapchain f
    dev = vrDevice vr
    gQ = snd (qGraphics (vrQueues vr))
    oneSecond = 1e9

  (acquireResult, imageIndex) <-
    KHR.acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable Vk.NULL_HANDLE >>= \case
      r@(Vk.SUCCESS, _) -> pure r
      r@(Vk.SUBOPTIMAL_KHR, _) -> pure r
      (Vk.TIMEOUT, _) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
      _ -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR

  (_, [commandBuffer]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = rrCommandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate

  let renderPassBeginInfo =
        zero
          { Vk.renderPass = renderPass
          , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
          , Vk.renderArea = Vk.Rect2D{Vk.offset = zero, Vk.extent = sExtent sc}
          , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 0)]
          }

  Vk.useCommandBuffer
    commandBuffer
    zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    do
      Vk.cmdUseRenderPass commandBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
        let Vk.Extent2D w h = sExtent sc
        Vk.cmdSetViewport
          commandBuffer
          0
          [ Vk.Viewport
              { Vk.x = 0
              , Vk.y = 0
              , Vk.width = realToFrac w
              , Vk.height = realToFrac h
              , Vk.minDepth = 0
              , Vk.maxDepth = 1
              }
          ]
        Vk.cmdSetScissor
          commandBuffer
          0
          [Vk.Rect2D{Vk.offset = Vk.Offset2D 0 0, Vk.extent = sExtent sc}]
        Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdDraw commandBuffer 3 1 0 0

  let submitInfo =
        zero
          { Vk.waitSemaphores = [rrImageAvailable]
          , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          , Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          , Vk.signalSemaphores = [rrRenderFinished, fHostTimeline f]
          }
          ::& zero
            { Vk12.waitSemaphoreValues = [1]
            , Vk12.signalSemaphoreValues = [1, fIndex f]
            }
            :& ()
  liftIO $
    queueSubmitFrame
      gQ
      f
      [SomeStruct submitInfo]
      (fHostTimeline f)
      (fIndex f)

  presentResult <-
    KHR.queuePresentKHR
      gQ
      zero
        { KHR.waitSemaphores = [rrRenderFinished]
        , KHR.swapchains = [sSwapchain sc]
        , KHR.imageIndices = [imageIndex]
        }

  case (acquireResult, presentResult) of
    (Vk.SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    (_, Vk.SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    _ -> pure ()

----------------------------------------------------------------
-- Render pass + pipeline (long-lived)
----------------------------------------------------------------

createRenderPass
  :: (MonadResource m) => Vk.Device -> Vk.Format -> m (ReleaseKey, Vk.RenderPass)
createRenderPass dev imageFormat =
  Vk.withRenderPass
    dev
    zero
      { Vk.attachments = [attachmentDescription]
      , Vk.subpasses = [subpass]
      , Vk.dependencies = [subpassDependency]
      }
    Nothing
    allocate
  where
    attachmentDescription :: Vk.AttachmentDescription
    attachmentDescription =
      zero
        { Vk.format = imageFormat
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
    subpass :: Vk.SubpassDescription
    subpass =
      zero
        { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.colorAttachments =
            [ zero
                { Vk.attachment = 0
                , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        }
    subpassDependency :: Vk.SubpassDependency
    subpassDependency =
      zero
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = 0
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.srcAccessMask = zero
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstAccessMask =
            Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT
              .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        }

createGraphicsPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> m (ReleaseKey, Vk.Pipeline)
createGraphicsPipeline dev renderPass = do
  (shaderKeys, shaderStages) <- V.unzip <$> createShaders dev
  (layoutKey, pipelineLayout) <- Vk.withPipelineLayout dev zero Nothing allocate
  let
    pipelineCreateInfo :: Vk.GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.stages = shaderStages
        , Vk.vertexInputState = Just zero
        , Vk.inputAssemblyState =
            Just
              zero
                { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                , Vk.primitiveRestartEnable = False
                }
        , Vk.viewportState =
            Just $
              SomeStruct
                zero
                  { Vk.viewportCount = 1
                  , Vk.scissorCount = 1
                  }
        , Vk.rasterizationState =
            Just $
              SomeStruct
                zero
                  { Vk.depthClampEnable = False
                  , Vk.rasterizerDiscardEnable = False
                  , Vk.lineWidth = 1
                  , Vk.polygonMode = Vk.POLYGON_MODE_FILL
                  , Vk.cullMode = Vk.CULL_MODE_NONE
                  , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
                  , Vk.depthBiasEnable = False
                  }
        , Vk.multisampleState =
            Just $
              SomeStruct
                zero
                  { Vk.sampleShadingEnable = False
                  , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
                  , Vk.minSampleShading = 1
                  , Vk.sampleMask = [maxBound]
                  }
        , Vk.depthStencilState = Nothing
        , Vk.colorBlendState =
            Just $
              SomeStruct
                zero
                  { Vk.logicOpEnable = False
                  , Vk.attachments =
                      [ zero
                          { Vk.colorWriteMask =
                              Vk.COLOR_COMPONENT_R_BIT
                                .|. Vk.COLOR_COMPONENT_G_BIT
                                .|. Vk.COLOR_COMPONENT_B_BIT
                                .|. Vk.COLOR_COMPONENT_A_BIT
                          , Vk.blendEnable = False
                          }
                      ]
                  }
        , Vk.dynamicState =
            Just
              zero
                { Vk.dynamicStates =
                    [ Vk.DYNAMIC_STATE_VIEWPORT
                    , Vk.DYNAMIC_STATE_SCISSOR
                    ]
                }
        , Vk.layout = pipelineLayout
        , Vk.renderPass = renderPass
        , Vk.subpass = 0
        , Vk.basePipelineHandle = zero
        }
  (key, (_, [graphicsPipeline])) <-
    Vk.withGraphicsPipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
      allocate
  release layoutKey
  traverse_ release shaderKeys
  pure (key, graphicsPipeline)

createShaders
  :: (MonadResource m)
  => Vk.Device
  -> m (V.Vector (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo))
createShaders dev = do
  (fragKey, fragModule) <- Vk.withShaderModule dev zero{Vk.code = fragCode} Nothing allocate
  (vertKey, vertModule) <- Vk.withShaderModule dev zero{Vk.code = vertCode} Nothing allocate
  let
    vertShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = vertModule
        , Vk.name = "main"
        }
    fragShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = fragModule
        , Vk.name = "main"
        }
  pure
    [ (vertKey, SomeStruct vertShaderStageCreateInfo)
    , (fragKey, SomeStruct fragShaderStageCreateInfo)
    ]
  where
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
          fragColor   = colors[gl_VertexIndex];
        }
      |]
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
