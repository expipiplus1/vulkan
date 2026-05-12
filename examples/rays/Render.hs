{-# LANGUAGE OverloadedLists #-}

module Render
  ( RenderState (..)
  , renderFrame
  ) where

import Camera
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Word
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable
import GHC.Clock (getMonotonicTime)
import Init (RTInfo (..))
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import VkResources (VkResources (..), vrContext)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Extent2D (Extent2D (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_ray_tracing_pipeline as RT
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

{- | Long-lived per-app render state. Built once during setup; threaded into
'renderFrame' each frame.
-}
data RenderState = RenderState
  { rsPipeline :: Vk.Pipeline
  , rsPipelineLayout :: Vk.PipelineLayout
  , rsDescriptorSets :: V.Vector Vk.DescriptorSet
  -- ^ One per concurrent-frame slot.
  , rsShaderBindingTableAddress :: Vk.DeviceAddress
  , rsCameraMatricesBuffer :: Vk.Buffer
  , rsCameraMatricesAllocation :: VMA.Allocation
  , rsCameraMatricesBufferData :: Ptr CameraMatrices
  , rsRTInfo :: RTInfo
  }

renderFrame
  :: VkResources
  -> RenderState
  -> Frame
  -> ResourceT IO ()
renderFrame vr rs f = do
  let
    sc = fSwapchain f
    dev = vrDevice vr
    slot = fromIntegral (fIndex f) `mod` 2
    descriptorSet = rsDescriptorSets rs ! slot
    cameraMatricesOffset = fromIntegral slot * fromIntegral (sizeOf (undefined :: CameraMatrices))

  (acquireResult, imageIndex) <- acquireFrameImage (vrContext vr) f

  -- Bind the per-slot descriptor set's image view + camera buffer slot.
  Vk.updateDescriptorSets
    dev
    [ SomeStruct
        zero
          { Vk.dstSet = descriptorSet
          , Vk.dstBinding = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.descriptorCount = 1
          , Vk.imageInfo =
              [ Vk.DescriptorImageInfo
                  { Vk.sampler = Vk.NULL_HANDLE
                  , Vk.imageView = sImageViews sc ! fromIntegral imageIndex
                  , Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL
                  }
              ]
          }
    , SomeStruct
        zero
          { Vk.dstSet = descriptorSet
          , Vk.dstBinding = 3
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
          , Vk.descriptorCount = 1
          , Vk.bufferInfo =
              [ Vk.DescriptorBufferInfo
                  { Vk.buffer = rsCameraMatricesBuffer rs
                  , Vk.offset = cameraMatricesOffset
                  , Vk.range = fromIntegral (sizeOf (undefined :: CameraMatrices))
                  }
              ]
          }
    ]
    []

  -- Update camera matrices for this frame.
  time <- realToFrac <$> liftIO getMonotonicTime
  let
    spin = axisAngle (V3 0 1 0) (sin time + 1)
    forwards = axisAngle (V3 0 0 1) 0
    camera = Camera (V3 0 0 (-10)) (spin * forwards) (16 / 9) 1.4
    cameraMats =
      CameraMatrices
        { cmViewInverse = transpose $ inv44 (viewMatrix camera)
        , cmProjInverse = transpose $ inv44 (projectionMatrix camera)
        }
  liftIO $
    poke
      (rsCameraMatricesBufferData rs `plusPtr` fromIntegral cameraMatricesOffset)
      cameraMats
  VMA.flushAllocation
    (vrAllocator vr)
    (rsCameraMatricesAllocation rs)
    cameraMatricesOffset
    (fromIntegral (sizeOf (undefined :: CameraMatrices)))

  commands <- recordCommands (vrContext vr) f \cb ->
    recordCommandBuffer cb rs sc descriptorSet imageIndex

  queueSubmitFrame (vrContext vr) f [commands]
  presentFrameImage (vrContext vr) f acquireResult imageIndex

----------------------------------------------------------------
-- Command buffer recording
----------------------------------------------------------------

recordCommandBuffer
  :: (MonadIO m)
  => Vk.CommandBuffer
  -> RenderState
  -> Swapchain
  -> Vk.DescriptorSet
  -> Word32
  -> m ()
recordCommandBuffer commandBuffer rs sc descriptorSet imageIndex = do
  let
    RTInfo{..} = rsRTInfo rs
    image = sImages sc ! fromIntegral imageIndex
    imageWidth = Extent2D.width (sExtent sc)
    imageHeight = Extent2D.height (sExtent sc)
    imageSubresourceRange =
      Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }
    numRayGenShaderGroups = 1
    rayGenRegion =
      RT.StridedDeviceAddressRegionKHR
        { RT.deviceAddress = rsShaderBindingTableAddress rs
        , RT.stride = fromIntegral rtiShaderGroupBaseAlignment
        , RT.size =
            fromIntegral rtiShaderGroupBaseAlignment
              * numRayGenShaderGroups
        }
    numHitShaderGroups = 1
    hitRegion =
      RT.StridedDeviceAddressRegionKHR
        { RT.deviceAddress =
            rsShaderBindingTableAddress rs
              + (1 * fromIntegral rtiShaderGroupBaseAlignment)
        , RT.stride = fromIntegral rtiShaderGroupBaseAlignment
        , RT.size = fromIntegral rtiShaderGroupBaseAlignment * numHitShaderGroups
        }
    numMissShaderGroups = 1
    missRegion =
      RT.StridedDeviceAddressRegionKHR
        { deviceAddress =
            rsShaderBindingTableAddress rs
              + (2 * fromIntegral rtiShaderGroupBaseAlignment)
        , stride = fromIntegral rtiShaderGroupBaseAlignment
        , size = fromIntegral rtiShaderGroupBaseAlignment * numMissShaderGroups
        }
    callableRegion = zero

  -- Transition image to general (write target for raygen).
  Vk.cmdPipelineBarrier
    commandBuffer
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    zero
    []
    []
    [ SomeStruct
        zero
          { Vk.srcAccessMask = zero
          , Vk.dstAccessMask = Vk.ACCESS_SHADER_WRITE_BIT
          , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
          , Vk.newLayout = Vk.IMAGE_LAYOUT_GENERAL
          , Vk.image = image
          , Vk.subresourceRange = imageSubresourceRange
          }
    ]

  Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR (rsPipeline rs)
  Vk.cmdBindDescriptorSets
    commandBuffer
    Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR
    (rsPipelineLayout rs)
    0
    [descriptorSet]
    []

  Vk.cmdPipelineBarrier
    commandBuffer
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    zero
    []
    [ SomeStruct
        zero
          { Vk.srcAccessMask = Vk.ACCESS_HOST_WRITE_BIT
          , Vk.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT
          , Vk.buffer = rsCameraMatricesBuffer rs
          , Vk.offset = 0 -- TODO: per-slot
          , Vk.size = Vk.WHOLE_SIZE
          }
    ]
    []

  RT.cmdTraceRaysKHR
    commandBuffer
    rayGenRegion
    missRegion
    hitRegion
    callableRegion
    imageWidth
    imageHeight
    1

  Vk.cmdPipelineBarrier
    commandBuffer
    Vk.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ SomeStruct
        zero
          { Vk.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT
          , Vk.dstAccessMask = zero
          , Vk.oldLayout = Vk.IMAGE_LAYOUT_GENERAL
          , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
          , Vk.image = image
          , Vk.subresourceRange = imageSubresourceRange
          }
    ]
