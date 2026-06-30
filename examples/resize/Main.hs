{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Exception (handle, mask_)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits ((.|.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Julia (JuliaPipeline (..), createJuliaDescriptorSets, createJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2
import qualified SDL
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo (..))
import Vulkan.Exception
import Vulkan.Utils.Barrier (imageBarrier)
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, allocateTimelineSemaphore, presentFrameImage, queueSubmitFrame, recordCommands)
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = prettyError . runResourceT $ do
  withSDL

  let
    initWidth = 1280
    initHeight = 720

  sdlWindow <- createWindow "Haskell ❤️ Vulkan" initWidth initHeight
  SDL.showWindow sdlWindow

  (vc, vma, initialSC) <- withWindowedVk windowConfig (sdl2Adapter sdlWindow)
  let dev = vcDevice vc

  juliaPL <- createJuliaPipeline dev

  -- Pick the synchronization strategy once, from the queue layout. If the
  -- compute slot landed in the graphics family we can keep everything on one
  -- queue and sync with a pipeline barrier; otherwise we run compute on the
  -- async queue and hand the offscreen image across with a timeline semaphore
  -- plus a queue-family ownership transfer.
  computeSync <-
    if fst (qGraphics (vcQueues vc)) == fst (qCompute (vcQueues vc))
      then pure SharedQueue
      else do
        (_, readyTimeline) <- allocateTimelineSemaphore dev 0
        lastBlitDone <- liftIO (newIORef 0)
        pure (AsyncCompute readyTimeline lastBlitDone)

  runWindowLoop
    vc
    initialSC
    (drawableSize sdlWindow)
    (shouldQuit sdlWindow)
    WindowLoop
      { wlMkState = createBindings dev vma juliaPL
      , wlRender = \bindings f -> renderJulia vc juliaPL computeSync bindings f
      , wlOnFrame = \start end -> reportFrameTime (end - start)
      , wlOnExit = noOnExit
      }

windowConfig :: WindowedConfig
windowConfig =
  WindowedConfig
    { appName = "Haskell ❤️ Vulkan"
    , instanceReqs = []
    , deviceReqs = []
    , vmaFlags = zero
    , swapchainConfig =
        defaultSwapchainConfig
          { scRequiredUsageFlags =
              -- TRANSFER_DST for the blit; COLOR_ATTACHMENT so the
              -- swapchain helper can still build (view-compatible) image
              -- views for each image.
              [ Vk.IMAGE_USAGE_TRANSFER_DST_BIT
              , Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
              ]
          , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_BLIT_DST_BIT]
          }
    }

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

----------------------------------------------------------------
-- Synchronization strategy
----------------------------------------------------------------

{- | How compute hands its result to the blit, decided once from the queue
layout (see 'main').
-}
data ComputeSync
  = {- | Compute shares the graphics queue family: one command buffer, an
    intra-queue pipeline barrier between the dispatch and the blit.
    -}
    SharedQueue
  | {- | Compute runs on a distinct queue family: two command buffers on two
    queues, handed over with this timeline semaphore and a queue-family
    ownership transfer of the offscreen image. The 'IORef' tracks the
    timeline value of the last frame that actually submitted a blit, so the
    next frame's compute can wait for that read to finish before reusing the
    single shared buffer — without deadlocking when a frame aborts (e.g. an
    out-of-date swapchain on resize) and never signals its value.
    -}
    AsyncCompute Vk.Semaphore (IORef Word64)

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { bOffscreenImage :: Vk.Image
  {- ^ The single compute render target; blitted to whichever swapchain image
  the frame acquires.
  -}
  , bOffscreenView :: Vk.ImageView
  , bJuliaDescriptorSet :: Vk.DescriptorSet
  }

createBindings
  :: Vk.Device
  -> VMA.Allocator
  -> JuliaPipeline
  -> Swapchain
  -> ResourceT IO (Bindings, ReleaseKey)
createBindings dev allocator jp sc = do
  -- A single offscreen RGBA8 storage image (+ view). Compute writes here; a
  -- blit then copies (and converts RGBA→BGRA) to the acquired swapchain image.
  -- It is recreated per swapchain only because its extent tracks the window.
  (imageKey, (image, _, _)) <-
    VMA.withImage allocator (offscreenImageInfo (sExtent sc)) offscreenAllocInfo allocate
  (viewKey, view) <- Vk.withImageView dev (offscreenViewInfo image) Nothing allocate

  (poolKey, juliaSets) <-
    createJuliaDescriptorSets dev (jpDescriptorSetLayout jp) [view]

  -- runWindowLoop fires exactly one release key on resize: free the pool (and
  -- its sets) first, then the view, then the image.
  bindingsKey <- register (mapM_ release ([poolKey, viewKey, imageKey] :: [ReleaseKey]))

  pure
    ( Bindings
        { bOffscreenImage = image
        , bOffscreenView = view
        , bJuliaDescriptorSet = V.head juliaSets
        }
    , bindingsKey
    )

offscreenFormat :: Vk.Format
offscreenFormat = Vk.FORMAT_R8G8B8A8_UNORM

offscreenImageInfo :: Vk.Extent2D -> Vk.ImageCreateInfo '[]
offscreenImageInfo (Vk.Extent2D w h) =
  zero
    { Vk.imageType = Vk.IMAGE_TYPE_2D
    , Vk.format = offscreenFormat
    , Vk.extent = Vk.Extent3D w h 1
    , Vk.mipLevels = 1
    , Vk.arrayLayers = 1
    , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
    , Vk.usage =
        Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
    , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
    }

offscreenAllocInfo :: VMA.AllocationCreateInfo
offscreenAllocInfo = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}

offscreenViewInfo :: Vk.Image -> Vk.ImageViewCreateInfo '[]
offscreenViewInfo image =
  zero
    { Vk.image = image
    , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
    , Vk.format = offscreenFormat
    , Vk.subresourceRange = colorSubresourceRange
    }

colorSubresourceRange :: Vk.ImageSubresourceRange
colorSubresourceRange =
  Vk.ImageSubresourceRange
    { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
    , Vk.baseMipLevel = 0
    , Vk.levelCount = 1
    , Vk.baseArrayLayer = 0
    , Vk.layerCount = 1
    }

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderJulia
  :: VulkanContext
  -> JuliaPipeline
  -> ComputeSync
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vc jp computeSync bindings f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  let swapImage = sImages sc V.! fromIntegral imageIndex

  case computeSync of
    SharedQueue -> do
      commands <- recordCommands vc f \cb -> do
        -- Offscreen → GENERAL for the compute write. The TRANSFER source stage
        -- makes this write wait for the previous frame's blit read of the same
        -- (shared) image — a barrier orders against prior submissions on one
        -- queue, so this is the cross-frame Write-after-Read hazard fence.
        beginComputeWrite cb offscreen Vk.PIPELINE_STAGE_TRANSFER_BIT
        dispatchJulia jp (sExtent sc) descriptorSet cb

        -- Offscreen → transfer source, swapchain → transfer destination.
        Vk.cmdPipelineBarrier
          cb
          Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
          Vk.PIPELINE_STAGE_TRANSFER_BIT
          zero
          []
          []
          [ offscreenToTransferSrc offscreen Vk.ACCESS_SHADER_WRITE_BIT Nothing
          , swapchainToTransferDst swapImage
          ]
        blitOffscreen (sExtent sc) offscreen swapImage cb
        swapchainToPresent swapImage cb

      queueSubmitFrame vc f imageIndex [commands]
    AsyncCompute readyTimeline lastBlitDone -> do
      let
        QueueFamilyIndex graphicsFam = fst (qGraphics (vcQueues vc))
        QueueFamilyIndex computeFam = fst (qCompute (vcQueues vc))

      -- Compute on its own queue. The per-frame pool is allocated in the
      -- frame's ResourceT scope, so it is freed only after this frame's GPU
      -- work completes — no manual ping-pong needed.
      computeCb <- do
        (_, computePool) <-
          Vk.withCommandPool
            (vcDevice vc)
            zero{CommandPoolCreateInfo.queueFamilyIndex = computeFam}
            Nothing
            allocate
        recordOnPool vc computePool \cb -> do
          -- Cross-frame Write-after-Read hazard is handled by the timeline wait below, so a plain
          -- top-of-pipe transition into GENERAL is enough here.
          beginComputeWrite cb offscreen Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
          dispatchJulia jp (sExtent sc) descriptorSet cb
          -- Release the offscreen image from the compute family.
          Vk.cmdPipelineBarrier
            cb
            Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
            Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
            zero
            []
            []
            [ offscreenToTransferSrc
                offscreen
                Vk.ACCESS_SHADER_WRITE_BIT
                (Just (computeFam, graphicsFam))
            ]

      graphicsCb <- recordCommands vc f \cb -> do
        -- Acquire the offscreen image onto the graphics family, and bring the
        -- swapchain image up as a transfer destination.
        Vk.cmdPipelineBarrier
          cb
          Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
          Vk.PIPELINE_STAGE_TRANSFER_BIT
          zero
          []
          []
          [ offscreenToTransferSrc offscreen zero (Just (computeFam, graphicsFam))
          , swapchainToTransferDst swapImage
          ]
        blitOffscreen (sExtent sc) offscreen swapImage cb
        swapchainToPresent swapImage cb

      submitAsync vc f imageIndex readyTimeline lastBlitDone computeCb graphicsCb

  presentFrameImage vc f acquireResult imageIndex
  where
    sc = fSwapchain f
    offscreen = bOffscreenImage bindings
    descriptorSet = bJuliaDescriptorSet bindings

-- | Bind the Julia pipeline, push the mouse-driven constants, and dispatch.
dispatchJulia
  :: (MonadUnliftIO m)
  => JuliaPipeline
  -> Vk.Extent2D
  -> Vk.DescriptorSet
  -> Vk.CommandBuffer
  -> m ()
dispatchJulia jp (Vk.Extent2D imageWidth imageHeight) descriptorSet cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

  P m <- SDL.getAbsoluteMouseLocation
  let
    m' :: V2 Float
    m' = fmap realToFrac m / imageSizeF
    c :: V2 Float
    c = (m' * 2) - 1
    r = 0.5 * (1 + sqrt (4 * norm c + 1))
    imageSizeF = realToFrac <$> V2 imageWidth imageHeight
    aspect = pure (recip (min (imageSizeF ^. _x) (imageSizeF ^. _y)))
    frameScale = aspect * 2 * pure r
    frameOffset = negate (imageSizeF * aspect) * pure r
    constantBytes = 4 * (2 + 2 + 2 + 1)
    escapeRadius = 12 :: Float
  allocaBytes constantBytes $ \p -> do
    liftIO $ poke (p `plusPtr` 0) frameScale
    liftIO $ poke (p `plusPtr` 8) frameOffset
    liftIO $ poke (p `plusPtr` 16) c
    liftIO $ poke (p `plusPtr` 24) escapeRadius
    Vk.cmdPushConstants
      cb
      (jpPipelineLayout jp)
      Vk.SHADER_STAGE_COMPUTE_BIT
      0
      constantBytes
      p
  Vk.cmdBindDescriptorSets
    cb
    Vk.PIPELINE_BIND_POINT_COMPUTE
    (jpPipelineLayout jp)
    0
    [descriptorSet]
    []
  Vk.cmdDispatch
    cb
    ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
    ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
    1

----------------------------------------------------------------
-- Command-buffer building blocks
----------------------------------------------------------------

{- | Transition an image UNDEFINED → GENERAL ahead of a compute write. The
caller supplies the source pipeline stage that the write must wait on.
-}
beginComputeWrite
  :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> Vk.PipelineStageFlags -> m ()
beginComputeWrite cb image srcStage =
  Vk.cmdPipelineBarrier
    cb
    srcStage
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        zero
        Vk.ACCESS_SHADER_WRITE_BIT
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_GENERAL
        image
    ]

{- | Offscreen GENERAL → TRANSFER_SRC barrier, ready for the blit. The caller
supplies the source access mask and, when the dispatch and blit run on
different queue families, @Just (srcFamily, dstFamily)@ to make this a
queue-family ownership transfer (release on the source, acquire on the dest).
-}
offscreenToTransferSrc
  :: Vk.Image
  -> Vk.AccessFlags
  -> Maybe (Word32, Word32)
  -> SomeStruct Vk.ImageMemoryBarrier
offscreenToTransferSrc image srcAccess ownership =
  let (srcFam, dstFam) =
        maybe (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED) id ownership
  in SomeStruct
       zero
         { Vk.srcAccessMask = srcAccess
         , Vk.dstAccessMask = Vk.ACCESS_TRANSFER_READ_BIT
         , Vk.oldLayout = Vk.IMAGE_LAYOUT_GENERAL
         , Vk.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
         , Vk.srcQueueFamilyIndex = srcFam
         , Vk.dstQueueFamilyIndex = dstFam
         , Vk.image = image
         , Vk.subresourceRange = colorSubresourceRange
         }

-- | Swapchain UNDEFINED → TRANSFER_DST barrier, ready to receive the blit.
swapchainToTransferDst :: Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
swapchainToTransferDst =
  imageBarrier
    Vk.IMAGE_ASPECT_COLOR_BIT
    zero
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

-- | Swapchain TRANSFER_DST → PRESENT barrier, after the blit.
swapchainToPresent :: (MonadIO m) => Vk.Image -> Vk.CommandBuffer -> m ()
swapchainToPresent image cb =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_TRANSFER_WRITE_BIT
        zero
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        image
    ]

-- | Blit the fractal onto the swapchain image (handles RGBA→BGRA).
blitOffscreen
  :: (MonadIO m) => Vk.Extent2D -> Vk.Image -> Vk.Image -> Vk.CommandBuffer -> m ()
blitOffscreen extent offscreen swapImage cb =
  Vk.cmdBlitImage
    cb
    offscreen
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    swapImage
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    [ Vk.ImageBlit
        { Vk.srcSubresource = colorSubresourceLayers
        , Vk.srcOffsets = fullExtentOffsets extent
        , Vk.dstSubresource = colorSubresourceLayers
        , Vk.dstOffsets = fullExtentOffsets extent
        }
    ]
    Vk.FILTER_NEAREST

colorSubresourceLayers :: Vk.ImageSubresourceLayers
colorSubresourceLayers =
  Vk.ImageSubresourceLayers
    { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
    , Vk.mipLevel = 0
    , Vk.baseArrayLayer = 0
    , Vk.layerCount = 1
    }

fullExtentOffsets :: Vk.Extent2D -> (Vk.Offset3D, Vk.Offset3D)
fullExtentOffsets (Vk.Extent2D w h) =
  (Vk.Offset3D 0 0 0, Vk.Offset3D (fromIntegral w) (fromIntegral h) 1)

----------------------------------------------------------------
-- Async-compute submission
----------------------------------------------------------------

{- | Record into a one-time-submit primary command buffer from a caller-supplied
pool (the async-compute path needs a compute-family pool, which the recycled
graphics pool can't provide). Mirrors 'recordCommands'.
-}
recordOnPool
  :: (MonadResource m, MonadFail m)
  => VulkanContext
  -> Vk.CommandPool
  -> (Vk.CommandBuffer -> m ())
  -> m Vk.CommandBuffer
recordOnPool vc pool record = do
  (_, [cb]) <-
    Vk.withCommandBuffers
      (vcDevice vc)
      zero
        { Vk.commandPool = pool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate
  Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} $
    record cb
  pure cb

{- | The two-queue submission for the async-compute path:

* compute runs on the compute queue, waiting on the host-timeline value of the
  last frame that actually blitted (tracked in @lastBlitDone@) so it never
  overwrites the shared offscreen image while the prior blit is still reading
  it, and signalling @readyTimeline@ at @fIndex@. Waiting on the
  last-submitted value (rather than @fIndex - 1@) keeps this deadlock-free when
  a frame aborts mid-flight and never signals its own value;
* the graphics blit waits on @readyTimeline@ (and image-available) and signals
  render-finished plus the host timeline, exactly like 'queueSubmitFrame'.
-}
submitAsync
  :: (MonadIO m)
  => VulkanContext
  -> Frame
  -> Word32
  -> Vk.Semaphore
  -> IORef Word64
  -> Vk.CommandBuffer
  -> Vk.CommandBuffer
  -> m ()
submitAsync vc Frame{..} imageIndex readyTimeline lastBlitDone computeCb graphicsCb = liftIO . mask_ $ do
  prevBlitDone <- readIORef lastBlitDone
  let computeSubmit =
        zero
          { Vk.waitSemaphores = [fHostTimeline]
          , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT]
          , Vk.commandBuffers = [Vk.commandBufferHandle computeCb]
          , Vk.signalSemaphores = [readyTimeline]
          }
          ::& zero
            { waitSemaphoreValues = [prevBlitDone]
            , signalSemaphoreValues = [fIndex]
            }
            :& ()
  Vk.queueSubmit (snd $ qCompute (vcQueues vc)) [SomeStruct computeSubmit] Vk.NULL_HANDLE

  let
    RecycledResources{rrImageAvailable} = fRecycled
    renderFinished = sRenderFinished fSwapchain V.! fromIntegral imageIndex
    graphicsSubmit =
      zero
        { Vk.waitSemaphores = [rrImageAvailable, readyTimeline]
        , Vk.waitDstStageMask =
            [ Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
            , Vk.PIPELINE_STAGE_TRANSFER_BIT
            ]
        , Vk.commandBuffers = [Vk.commandBufferHandle graphicsCb]
        , Vk.signalSemaphores = [renderFinished, fHostTimeline]
        }
        ::& zero
          { -- Binary semaphores ignore their value entry; the timeline ones use it.
            waitSemaphoreValues = [0, fIndex]
          , signalSemaphoreValues = [0, fIndex]
          }
          :& ()

  Vk.queueSubmit (snd $ qGraphics (vcQueues vc)) [SomeStruct graphicsSubmit] Vk.NULL_HANDLE
  atomicModifyIORef' fGPUWork $ \jobs -> ((fHostTimeline, fIndex) : jobs, ())
  -- This frame's blit is now in flight; the next frame's compute waits on it.
  writeIORef lastBlitDone fIndex

----------------------------------------------------------------
-- Frame timing
----------------------------------------------------------------

reportFrameTime :: (MonadIO m) => Word64 -> m ()
reportFrameTime nsec = do
  let
    frameTimeNSec = realToFrac nsec :: Double
    targetHz = 60
    frameTimeBudgetMSec = recip targetHz * 1e3
    frameTimeMSec = frameTimeNSec / 1e6
    frameBudgetPercent = ceiling (100 * frameTimeMSec / frameTimeBudgetMSec) :: Int
  when (frameBudgetPercent > 50) $
    sayErrString (show frameTimeMSec <> "ms \t" <> show frameBudgetPercent <> "%")
