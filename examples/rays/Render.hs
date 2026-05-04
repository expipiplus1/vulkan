{-# LANGUAGE OverloadedLists #-}

module Render
  ( RenderState(..)
  , renderFrame
  ) where

import           Camera
import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Vector                    ( (!) )
import qualified Data.Vector                   as V
import           Data.Word
import           Foreign.Ptr                    ( Ptr
                                                , plusPtr
                                                )
import           Foreign.Storable
import           Frame                          ( Frame(..)
                                                , numConcurrentFrames
                                                , queueSubmitFrame
                                                )
import           GHC.Clock                      ( getMonotonicTime )
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           Init                           ( RTInfo(..) )
import           Linear.Matrix
import           Linear.Quaternion
import           Linear.V3
import           Swapchain                      ( Swapchain(..) )
import           UnliftIO.Exception             ( throwString )
import           VkResources                    ( Queues(..)
                                                , RecycledResources(..)
                                                , VkResources(..)
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Core10
import qualified Vulkan.Core10                 as Extent2D (Extent2D(..))
import qualified Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Exception               ( VulkanException(..) )
import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )
import           Vulkan.Zero

-- | Long-lived per-app render state. Built once during setup; threaded into
-- 'renderFrame' each frame.
data RenderState = RenderState
  { rsPipeline                 :: Pipeline
  , rsPipelineLayout           :: PipelineLayout
  , rsDescriptorSets           :: V.Vector DescriptorSet
    -- ^ One per concurrent-frame slot. Picked by @fIndex `mod` numConcurrentFrames@.
  , rsShaderBindingTableAddress :: DeviceAddress
  , rsCameraMatricesBuffer     :: Buffer
  , rsCameraMatricesAllocation :: Allocation
  , rsCameraMatricesBufferData :: Ptr CameraMatrices
  , rsRTInfo                   :: RTInfo
  }

renderFrame
  :: VkResources
  -> RenderState
  -> Frame
  -> ResourceT IO ()
renderFrame vr rs f = do
  let RecycledResources {..}    = fRecycled f
      sc                        = fSwapchain f
      dev                       = vrDevice vr
      Queues (_, gQ)            = vrQueues vr
      RTInfo {..}               = rsRTInfo rs
      slot                      = fromIntegral (fIndex f) `mod` numConcurrentFrames
      descriptorSet             = rsDescriptorSets rs ! slot
      cameraMatricesOffset      = fromIntegral slot
                                  * fromIntegral (sizeOf (undefined :: CameraMatrices))
      oneSecond                 = 1e9

  -- Acquire next image
  (acquireResult, imageIndex) <-
    acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable NULL_HANDLE
      >>= \case
            r@(SUCCESS,        _) -> pure r
            r@(SUBOPTIMAL_KHR, _) -> pure r
            (TIMEOUT, _) -> timeoutError "Timed out (1s) acquiring next image"
            _            -> throwString "Unexpected Result from acquireNextImageKHR"

  -- Bind the per-slot descriptor set's image view + camera buffer slot.
  updateDescriptorSets
    dev
    [ SomeStruct zero
      { dstSet          = descriptorSet
      , dstBinding      = 1
      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
      , descriptorCount = 1
      , imageInfo       = [ DescriptorImageInfo
                              { sampler     = NULL_HANDLE
                              , imageView   = sImageViews sc ! fromIntegral imageIndex
                              , imageLayout = IMAGE_LAYOUT_GENERAL
                              }
                          ]
      }
    , SomeStruct zero
      { dstSet          = descriptorSet
      , dstBinding      = 3
      , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
      , descriptorCount = 1
      , bufferInfo      = [ DescriptorBufferInfo
                              { buffer = rsCameraMatricesBuffer rs
                              , offset = cameraMatricesOffset
                              , range  = fromIntegral
                                           (sizeOf (undefined :: CameraMatrices))
                              }
                          ]
      }
    ]
    []

  -- Update camera matrices for this frame.
  time <- realToFrac <$> liftIO getMonotonicTime
  let spin       = axisAngle (V3 0 1 0) (sin time + 1)
      forwards   = axisAngle (V3 0 0 1) 0
      camera     = Camera (V3 0 0 (-10)) (spin * forwards) (16 / 9) 1.4
      cameraMats = CameraMatrices
        { cmViewInverse = transpose $ inv44 (viewMatrix camera)
        , cmProjInverse = transpose $ inv44 (projectionMatrix camera)
        }
  liftIO $ poke
    (rsCameraMatricesBufferData rs `plusPtr` fromIntegral cameraMatricesOffset)
    cameraMats
  flushAllocation
    (vrAllocator vr)
    (rsCameraMatricesAllocation rs)
    cameraMatricesOffset
    (fromIntegral (sizeOf (undefined :: CameraMatrices)))

  -- Allocate per-frame command buffer from the recycled pool.
  let commandBufferAllocateInfo = zero
        { commandPool        = rrCommandPool
        , level              = COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = 1
        }
  (_, ~[commandBuffer]) <- withCommandBuffers dev commandBufferAllocateInfo allocate

  useCommandBuffer
      commandBuffer
      zero { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ recordCommandBuffer
        commandBuffer
        rs
        sc
        descriptorSet
        imageIndex

  -- Submit and record GPU work for the frame's wait thread.
  let submitInfo =
        zero { Core10.waitSemaphores = [rrImageAvailable]
             , waitDstStageMask      = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
             , commandBuffers        = [commandBufferHandle commandBuffer]
             , signalSemaphores      = [rrRenderFinished, fHostTimeline f]
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
-- Command buffer recording
----------------------------------------------------------------

recordCommandBuffer
  :: MonadIO m
  => CommandBuffer
  -> RenderState
  -> Swapchain
  -> DescriptorSet
  -> Word32
  -> m ()
recordCommandBuffer commandBuffer rs sc descriptorSet imageIndex = do
  let RTInfo {..}             = rsRTInfo rs
      image                   = sImages sc ! fromIntegral imageIndex
      imageWidth              = Extent2D.width  (sExtent sc)
      imageHeight             = Extent2D.height (sExtent sc)
      imageSubresourceRange   = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }
      numRayGenShaderGroups = 1
      rayGenRegion          = StridedDeviceAddressRegionKHR
        { deviceAddress = rsShaderBindingTableAddress rs
        , stride        = fromIntegral rtiShaderGroupBaseAlignment
        , size          = fromIntegral rtiShaderGroupBaseAlignment
                            * numRayGenShaderGroups
        }
      numHitShaderGroups = 1
      hitRegion          = StridedDeviceAddressRegionKHR
        { deviceAddress = rsShaderBindingTableAddress rs
                            + (1 * fromIntegral rtiShaderGroupBaseAlignment)
        , stride = fromIntegral rtiShaderGroupBaseAlignment
        , size = fromIntegral rtiShaderGroupBaseAlignment * numHitShaderGroups
        }
      numMissShaderGroups = 1
      missRegion          = StridedDeviceAddressRegionKHR
        { deviceAddress = rsShaderBindingTableAddress rs
                            + (2 * fromIntegral rtiShaderGroupBaseAlignment)
        , stride = fromIntegral rtiShaderGroupBaseAlignment
        , size = fromIntegral rtiShaderGroupBaseAlignment * numMissShaderGroups
        }
      callableRegion = zero

  -- Transition image to general (write target for raygen).
  cmdPipelineBarrier
    commandBuffer
    PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    zero
    []
    []
    [ SomeStruct zero { srcAccessMask    = zero
                      , dstAccessMask    = ACCESS_SHADER_WRITE_BIT
                      , oldLayout        = IMAGE_LAYOUT_UNDEFINED
                      , newLayout        = IMAGE_LAYOUT_GENERAL
                      , image            = image
                      , subresourceRange = imageSubresourceRange
                      }
    ]

  cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_RAY_TRACING_KHR (rsPipeline rs)
  cmdBindDescriptorSets
    commandBuffer
    PIPELINE_BIND_POINT_RAY_TRACING_KHR
    (rsPipelineLayout rs)
    0
    [descriptorSet]
    []

  cmdPipelineBarrier
    commandBuffer
    PIPELINE_STAGE_TOP_OF_PIPE_BIT
    PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    zero
    []
    [ SomeStruct
        zero { srcAccessMask = ACCESS_HOST_WRITE_BIT
             , dstAccessMask = ACCESS_SHADER_READ_BIT
             , buffer        = rsCameraMatricesBuffer rs
             , offset        = 0    -- TODO: per-slot
             , size          = WHOLE_SIZE
             }
    ]
    []

  cmdTraceRaysKHR commandBuffer
                  rayGenRegion
                  missRegion
                  hitRegion
                  callableRegion
                  imageWidth
                  imageHeight
                  1

  cmdPipelineBarrier
    commandBuffer
    PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ SomeStruct zero { srcAccessMask    = ACCESS_SHADER_WRITE_BIT
                      , dstAccessMask    = zero
                      , oldLayout        = IMAGE_LAYOUT_GENERAL
                      , newLayout        = IMAGE_LAYOUT_PRESENT_SRC_KHR
                      , image            = image
                      , subresourceRange = imageSubresourceRange
                      }
    ]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
