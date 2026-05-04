{-# LANGUAGE TypeApplications #-}

module Main where

import           AccelerationStructure          ( createTLAS )
import           Camera                         ( CameraMatrices )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.Word                      ( Word64 )
import           Foreign.Ptr                    ( castPtr )
import           Foreign.Storable               ( sizeOf )
import           Frame                          ( Frame(..)
                                                , advanceFrame
                                                , initialFrame
                                                , numConcurrentFrames
                                                , runFrame
                                                )
import           Init                           ( PhysicalDeviceInfo(..)
                                                , createDevice
                                                , createInstance
                                                , createVMA
                                                )
import qualified Pipeline
import           Render                         ( RenderState(..)
                                                , renderFrame
                                                )
import qualified SDL
import           Scene                          ( makeSceneBuffers )
import           Swapchain                      ( allocSwapchain
                                                , recreateSwapchain
                                                , threwSwapchainError
                                                )
import           Utils                          ( loopJust )
import           VkResources                    ( mkVkResources )
import           Vulkan.Core10
import           Vulkan.Zero                    ( zero )
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
                                                ( BufferDeviceAddressInfo(..)
                                                , getBufferDeviceAddress
                                                )
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )
import           Window.SDL2                    ( RefreshLimit(..)
                                                , createWindow
                                                , drawableSize
                                                , shouldQuit
                                                , withSDL
                                                )

main :: IO ()
main = runResourceT $ do
  withSDL
  win                        <- createWindow "Vulkan ⚡ Haskell" 1280 720
  inst                       <- Init.createInstance win
  (phys, pdi, dev, qs, surf) <- Init.createDevice inst win
  vma                        <- createVMA inst phys dev
  vr                         <- liftIO $ mkVkResources inst phys dev vma qs

  -- Initial swapchain
  initialSize <- liftIO $ drawableSize win
  initialSC   <- allocSwapchain vr NULL_HANDLE initialSize surf

  -- Scene + acceleration structure
  sceneBuffers <- makeSceneBuffers vma
  (_, tlas)    <- createTLAS vr sceneBuffers

  -- RT pipeline + descriptor sets
  let rtInfo = pdiRTInfo pdi
  (_, descSetLayout)         <- Pipeline.createRTDescriptorSetLayout dev
  (_, pipelineLayout)        <- Pipeline.createRTPipelineLayout dev descSetLayout
  (_, pipeline, numGroups)   <- Pipeline.createPipeline dev pipelineLayout
  (_, sbtBuffer)             <- Pipeline.createShaderBindingTable dev vma rtInfo pipeline numGroups
  sbtAddress                 <- getBufferDeviceAddress dev zero { buffer = sbtBuffer }
  descSets                   <- Pipeline.createRTDescriptorSets
                                  dev
                                  descSetLayout
                                  tlas
                                  sceneBuffers
                                  (fromIntegral numConcurrentFrames)

  -- Camera matrices buffer (one slot per concurrent frame).
  let cmSize = fromIntegral numConcurrentFrames
                * fromIntegral (sizeOf (undefined :: CameraMatrices))
  (_, (cmBuffer, cmAlloc, cmAllocInfo)) <- VMA.withBuffer
    vma
    zero { size  = cmSize
         , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
         }
    zero
      { flags         = ALLOCATION_CREATE_MAPPED_BIT
      , usage         = MEMORY_USAGE_CPU_TO_GPU
      , requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
      }
    allocate
  let cmBufferData = castPtr @() @CameraMatrices (mappedData cmAllocInfo)

  let renderState = RenderState
        { rsPipeline                  = pipeline
        , rsPipelineLayout            = pipelineLayout
        , rsDescriptorSets            = descSets
        , rsShaderBindingTableAddress = sbtAddress
        , rsCameraMatricesBuffer      = cmBuffer
        , rsCameraMatricesAllocation  = cmAlloc
        , rsCameraMatricesBufferData  = cmBufferData
        , rsRTInfo                    = rtInfo
        }

  scRef   <- liftIO $ newIORef initialSC
  initial <- initialFrame vr initialSC

  liftIO $ for_ descSets (\_ -> pure ())  -- descSets is used; silence unused

  SDL.showWindow win
  start <- SDL.time @Double

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      let f' = f { fSwapchain = currentSC }
      needsNew <- threwSwapchainError $ liftIO $ runFrame vr f' $
        renderFrame vr renderState f'
      sc' <- if needsNew
        then do
          newSize <- liftIO $ drawableSize win
          sc'     <- recreateSwapchain vr newSize currentSC
          liftIO $ writeIORef scRef sc'
          pure sc'
        else pure currentSC
      advanceFrame vr sc' f'

    loop f = shouldQuit NoLimit >>= \case
      True -> do
        end <- SDL.time
        let frames = fIndex f :: Word64
            mean   = realToFrac frames / (end - start) :: Double
        liftIO $ putStrLn $ "Average: " <> show mean
        pure Nothing
      False -> Just <$> perFrame f

  loopJust loop initial
