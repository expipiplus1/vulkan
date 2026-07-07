{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Julia-set viewer, driven by a per-frame 'FG.FrameGraph'.

The fractal only changes when the window resizes or the cursor moves; its
steady state is "re-present the same image". Each frame declares a graph of up
to three passes — @julia@ (compute → offscreen), @blit@ (offscreen → swapchain)
and @present@ (swapchain → PRESENT_SRC) — but adds @julia@ only when the fractal
parameters changed and @blit@ only when the acquired swapchain image does not
already hold the current fractal. fragr then places exactly the barriers the
surviving passes need: a fully idle frame records /nothing/ and just re-presents.

Because the offscreen and swapchain images are imported 'ManagedImage's whose
tracked layout persists across frames, an unchanged image needs no barrier at
all — the pruning of unused nodes prunes their transitions with them. (The
cross-frame write-after-read fence on the shared offscreen image also falls out
for free: a dirty frame's compute transition carries the previous blit's
TRANSFER source stage, ordering the overwrite after that read.)

Single graphics queue for now; the same declaration is ready to 'FG.setQueue'
the compute pass onto an async-compute queue and drive it with
'FG.executeQueued'.
-}
module Main
  ( main
  ) where

import Control.Exception (handle)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import qualified Fragr as FG
import Julia (JuliaPipeline (..), createJuliaDescriptorSets, createJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2
import qualified SDL
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import Vulkan.Utils.FrameGraph.Image (ImageDesc (..), ManagedImage (..), Usage (..), newManagedImage, usageFlags)
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (VulkanContext (..))
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

  runWindowLoop
    vc
    initialSC
    (drawableSize sdlWindow)
    (shouldQuit sdlWindow)
    WindowLoop
      { wlMkState = createBindings dev vma juliaPL
      , wlRender = \bindings f -> renderJulia vc juliaPL bindings f
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
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { bOffscreen :: ManagedImage
  {- ^ The single compute target, imported so its tracked layout persists across
  frames (recreated per swapchain because its extent tracks the window).
  -}
  , bJuliaDescriptorSet :: Vk.DescriptorSet
  , bSwapImages :: Vector ManagedImage
  -- ^ One layout-tracked wrapper per swapchain image.
  , bLastConstants :: IORef (Maybe JuliaConstants)
  -- ^ Fractal parameters last computed; a change makes the frame dirty.
  , bFreshImages :: IORef IntSet
  -- ^ Swapchain image indices that already hold the current fractal.
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
  (imageKey, (image, _, _)) <-
    VMA.withImage allocator (offscreenImageInfo (sExtent sc)) offscreenAllocInfo allocate
  (viewKey, view) <- Vk.withImageView dev (offscreenViewInfo image) Nothing allocate

  (poolKey, juliaSets) <-
    createJuliaDescriptorSets dev (jpDescriptorSetLayout jp) [view]

  offscreen <- newManagedImage image Vk.IMAGE_ASPECT_COLOR_BIT
  swapImages <- traverse (\img -> newManagedImage img Vk.IMAGE_ASPECT_COLOR_BIT) (sImages sc)
  lastConstants <- liftIO (newIORef Nothing)
  freshImages <- liftIO (newIORef IntSet.empty)

  -- runWindowLoop fires exactly one release key on resize: free the pool (and
  -- its sets) first, then the view, then the image. The swapchain images belong
  -- to the swapchain, so their wrappers need no release.
  bindingsKey <- register (mapM_ release ([poolKey, viewKey, imageKey] :: [ReleaseKey]))

  pure
    ( Bindings
        { bOffscreen = offscreen
        , bJuliaDescriptorSet = V.head juliaSets
        , bSwapImages = swapImages
        , bLastConstants = lastConstants
        , bFreshImages = freshImages
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
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vc jp bindings f = do
  constants <- computeConstants (sExtent sc)
  lastConstants <- liftIO (readIORef bindings.bLastConstants)
  let dirty = Just constants /= lastConstants

  (acquireResult, imageIndex) <- acquireFrameImage vc f
  freshImages <- liftIO (readIORef bindings.bFreshImages)
  let
    ix = fromIntegral imageIndex :: Int
    -- Compute only on a parameter change; blit unless this image already holds
    -- the current fractal (a dirty frame invalidates every other image).
    needBlit = dirty || not (IntSet.member ix freshImages)
    swapManaged = bSwapImages bindings V.! ix

  graph <- FG.newFrameGraph
  offscreenH <- FG.importResource graph "offscreen" (ImageDesc "offscreen") bindings.bOffscreen
  swapchainH <- FG.importResource graph "swapchain" (ImageDesc "swapchain") swapManaged

  offscreenReady <-
    if dirty
      then FG.addPass graph "julia" (\b -> FG.writeWith b offscreenH (usageFlags StorageWrite)) \_written _resources cb ->
        dispatchJulia jp (sExtent sc) constants bindings.bJuliaDescriptorSet cb
      else pure offscreenH

  swapchainReady <-
    if needBlit
      then FG.addPass graph "blit" (mkBlitHandles offscreenReady swapchainH) \_blitted _resources cb ->
        blitOffscreen (sExtent sc) bindings.bOffscreen.image swapManaged.image cb
      else pure swapchainH

  -- Always present: writing the imported swapchain marks a side effect, so the
  -- pass survives culling and its hook brings the image to PRESENT_SRC (a no-op
  -- barrier when it is already there, i.e. an idle re-present).
  _present <- FG.addPass graph "present" (\b -> FG.writeWith b swapchainReady (usageFlags Present)) \_presented _resources _cb ->
    pure ()

  FG.compile graph
  commands <- recordCommands vc f \cb -> FG.execute graph cb ()
  queueSubmitFrame vc f imageIndex [commands]
  presentFrameImage vc f acquireResult imageIndex

  liftIO $ do
    when dirty $ writeIORef bindings.bLastConstants (Just constants)
    when needBlit $
      modifyIORef' bindings.bFreshImages $
        if dirty then const (IntSet.singleton ix) else IntSet.insert ix
  where
    sc = fSwapchain f
    mkBlitHandles offscreenReady swapchainH b = do
      _ <- FG.readWith b offscreenReady (usageFlags TransferSrc)
      FG.writeWith b swapchainH (usageFlags TransferDst)

----------------------------------------------------------------
-- Julia dispatch
----------------------------------------------------------------

-- | The mouse-and-extent-derived fractal parameters pushed to the compute shader.
data JuliaConstants = JuliaConstants
  { jcScale :: V2 Float
  , jcOffset :: V2 Float
  , jcC :: V2 Float
  , jcEscapeRadius :: Float
  }
  deriving stock (Eq)

-- | Derive the fractal parameters from the cursor position over the image.
computeConstants :: (MonadIO m) => Vk.Extent2D -> m JuliaConstants
computeConstants (Vk.Extent2D imageWidth imageHeight) = do
  P m <- SDL.getAbsoluteMouseLocation
  let
    m' :: V2 Float
    m' = fmap realToFrac m / imageSizeF
    c :: V2 Float
    c = (m' * 2) - 1
    r = 0.5 * (1 + sqrt (4 * norm c + 1))
    imageSizeF = realToFrac <$> V2 imageWidth imageHeight
    aspect = pure (recip (min (imageSizeF ^. _x) (imageSizeF ^. _y)))
  pure
    JuliaConstants
      { jcScale = aspect * 2 * pure r
      , jcOffset = negate (imageSizeF * aspect) * pure r
      , jcC = c
      , jcEscapeRadius = 12
      }

-- | Bind the Julia pipeline, push the constants, and dispatch over the image.
dispatchJulia
  :: (MonadUnliftIO m)
  => JuliaPipeline
  -> Vk.Extent2D
  -> JuliaConstants
  -> Vk.DescriptorSet
  -> Vk.CommandBuffer
  -> m ()
dispatchJulia jp (Vk.Extent2D imageWidth imageHeight) constants descriptorSet cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

  let constantBytes = 4 * (2 + 2 + 2 + 1) :: Int
  allocaBytes constantBytes $ \p -> do
    liftIO $ poke (p `plusPtr` 0) constants.jcScale
    liftIO $ poke (p `plusPtr` 8) constants.jcOffset
    liftIO $ poke (p `plusPtr` 16) constants.jcC
    liftIO $ poke (p `plusPtr` 24) constants.jcEscapeRadius
    Vk.cmdPushConstants cb (jpPipelineLayout jp) Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral constantBytes) p

  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipelineLayout jp) 0 [descriptorSet] []
  Vk.cmdDispatch
    cb
    ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
    ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
    1

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
