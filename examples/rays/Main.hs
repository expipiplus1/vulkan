{-# LANGUAGE TypeApplications #-}

module Main where

import AccelerationStructure (createTLAS)
import Camera (CameraMatrices)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Foldable (for_)
import Data.IORef
import Data.Word (Word64)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import Frame (Frame (..), advanceFrame, initialFrame, runFrame)
import Init (deviceRequirements, getDeviceRTProps, instanceRequirements)
import qualified Pipeline
import Render (RenderState (..), renderFrame)
import qualified SDL
import Scene (makeSceneBuffers)
import Swapchain (recreateSwapchain, threwSwapchainError)
import Utils (loopJust)
import VkResources (VkResources (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo (..), getBufferDeviceAddress)
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

  scRef <- liftIO $ newIORef initialSC
  initial <- initialFrame vr initialSC

  liftIO $ for_ descSets (\_ -> pure ()) -- descSets is used; silence unused
  SDL.showWindow win
  start <- SDL.time @Double

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      let f' = f{fSwapchain = currentSC}
      needsNew <-
        threwSwapchainError $
          liftIO $
            runFrame vr f' $
              renderFrame vr renderState f'
      sc' <-
        if needsNew
          then do
            newSize <- liftIO $ drawableSize win
            sc' <- recreateSwapchain vr newSize currentSC
            liftIO $ writeIORef scRef sc'
            pure sc'
          else pure currentSC
      advanceFrame vr sc' f'

    loop f =
      shouldQuit win >>= \case
        True -> do
          end <- SDL.time
          let
            frames = fIndex f :: Word64
            mean = realToFrac frames / (end - start) :: Double
          liftIO $ putStrLn $ "Average: " <> show mean
          pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial
