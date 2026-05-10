{-# LANGUAGE TypeApplications #-}

module Main where

import AccelerationStructure (createTLAS)
import Camera (CameraMatrices)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import Init (deviceRequirements, getDeviceRTProps, instanceRequirements)
import qualified Pipeline
import Render (RenderState (..), renderFrame)
import qualified SDL
import Scene (makeSceneBuffers)
import VkResources (VkResources (..), vrContext)
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo (..), getBufferDeviceAddress)
import Vulkan.Utils.Frame (Frame (..))
import Vulkan.Utils.Swapchain (defaultSwapchainConfig)
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnFrame, noWindowState, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import Window.SDL2 (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  withSDL
  win <- createWindow "Vulkan ⚡ Haskell" 1280 720
  (vr, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = "Vulkan ⚡ Haskell"
        , wcInstanceReqs = instanceRequirements
        , wcDeviceReqs = deviceRequirements
        , wcVmaFlags = VMA.ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
        , wcSwapchainConfig = defaultSwapchainConfig
        }
      (sdl2Adapter win)
  let
    phys = vrPhysicalDevice vr
    dev = vrDevice vr
    vma = vrAllocator vr

  -- Scene + acceleration structure
  sceneBuffers <- makeSceneBuffers vma
  (_, tlas) <- createTLAS vr sceneBuffers

  -- RT pipeline + descriptor sets
  rtInfo <- getDeviceRTProps phys
  (_, descSetLayout) <- Pipeline.createRTDescriptorSetLayout dev
  (_, pipelineLayout) <- Pipeline.createRTPipelineLayout dev descSetLayout
  (_, pipeline, numGroups) <- Pipeline.createPipeline dev pipelineLayout
  (_, sbtBuffer) <- Pipeline.createShaderBindingTable dev vma rtInfo pipeline numGroups
  sbtAddress <- getBufferDeviceAddress dev zero{buffer = sbtBuffer}
  descSets <- Pipeline.createRTDescriptorSets dev descSetLayout tlas sceneBuffers 2

  (_, (cmBuffer, cmAlloc, cmAllocInfo)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = 2 * fromIntegral (sizeOf (undefined :: CameraMatrices))
        , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        , VMA.requiredFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        }
      allocate
  let cmBufferData = castPtr @() @CameraMatrices (VMA.mappedData cmAllocInfo)

  let renderState =
        RenderState
          { rsPipeline = pipeline
          , rsPipelineLayout = pipelineLayout
          , rsDescriptorSets = descSets
          , rsShaderBindingTableAddress = sbtAddress
          , rsCameraMatricesBuffer = cmBuffer
          , rsCameraMatricesAllocation = cmAlloc
          , rsCameraMatricesBufferData = cmBufferData
          , rsRTInfo = rtInfo
          }

  SDL.showWindow win
  start <- SDL.time @Double

  runWindowLoop
    (vrContext vr)
    initialSC
    (drawableSize win)
    (shouldQuit win)
    WindowLoop
      { wlMkState = noWindowState
      , wlRender = \() f -> renderFrame vr renderState f
      , wlOnFrame = noOnFrame
      , wlOnExit = \f -> liftIO $ do
          end <- SDL.time
          let mean = realToFrac (fIndex f) / (end - start) :: Double
          putStrLn $ "Average: " <> show mean
      }
