{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The extent-dependent scene.

'allocateTargets' builds the render targets, the bloom pyramid, and the descriptor
sets binding extent-sized views over a shared "Rendering.Static" — cheap enough to
rerun on every resize. The hi-Z pyramid ('SharedHiZ') sits outside it, shared by the
frames in flight.
-}
module Rendering.Targets
  ( SceneTargets (..)
  , Scene (..)
  , SharedHiZ (..)
  , allocateSharedHiZ
  , allocateTargets
  , debugImages
  , shadowImage
  , lumProbe
  , halfExtentOf
  , lumMipFor
  , readLuminance
  , setExposure
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, register)
import Data.Bits (shiftR, (.|.))
import Data.IORef (IORef, newIORef)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.FrameGraph.Buffer
import Vulkan.Utils.FrameGraph.Image (ManagedImage, SliceRegistry, describedImage, describedMip, forgetImage, sharedAcrossQueues)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import qualified Pipeline.Bloom as Bloom
import qualified Pipeline.Cull as Cull
import qualified Pipeline.Gamma as Gamma
import qualified Pipeline.HiZ as HiZ
import qualified Pipeline.Luminance as Luminance
import qualified Pipeline.Shade as Shade
import qualified Pipeline.Ssao as Ssao
import qualified Pipeline.Tonemap as Tonemap
import RenderTarget (allocateImage, allocateMipChain, allocateTarget)
import Rendering.Pipelines (ScenePipelines, aoFormat, colorFormat, depthFormat, hdrFormat, normalsFormat, visFormat)
import qualified Rendering.Pipelines
import Rendering.Static (SceneStatic)
import qualified Rendering.Static

{- | The scene's GPU-only images at a given extent, wrapped for layout tracking.

The chain is @vis+depth@ (raster) → @colorHDR@ (shade) → @tone@ (tonemap) →
@display@ (gamma).
-}
data SceneTargets = SceneTargets
  { vis :: ManagedImage
  , visView :: Vk.ImageView
  , depth :: ManagedImage
  , depthView :: Vk.ImageView
  , colorHDR :: ManagedImage
  , colorHDRView :: Vk.ImageView
  , tone :: ManagedImage
  , toneView :: Vk.ImageView
  , display :: ManagedImage
  , displayView :: Vk.ImageView
  , normals :: ManagedImage
  -- ^ Half-res DAIS world normals ("Pipeline.Ssao"), w = 0 in the void.
  , normalsView :: Vk.ImageView
  , ao :: ManagedImage
  -- ^ Half-res SSAO factor: gathered, blurred back in place, sampled by the resolve.
  , aoView :: Vk.ImageView
  , aoBlur :: ManagedImage
  -- ^ Scratch between the two blur axes.
  , aoBlurView :: Vk.ImageView
  }

{- | The extent-dependent scene.

The render targets, the bloom pyramid, and the descriptor sets that bind extent-sized
views. Rebuilt on resize by 'allocateTargets' over a shared 'SceneStatic'.
-}
data Scene = Scene
  { static :: SceneStatic
  , targets :: SceneTargets
  , shadeSet :: Vk.DescriptorSet
  , normalsSet :: Vk.DescriptorSet
  , aoSet :: Vk.DescriptorSet
  , aoBlurXSet :: Vk.DescriptorSet
  -- ^ ao → aoBlur; 'aoBlurYSet' brings it back.
  , aoBlurYSet :: Vk.DescriptorSet
  , lumSet :: Vk.DescriptorSet
  , toneSet :: Vk.DescriptorSet
  , gammaSet :: Vk.DescriptorSet
  , gammaDebugSet :: Vk.DescriptorSet
  -- ^ Gamma over the raw shade output: debug views skip the tonemap.
  , bloomMips :: V.Vector ManagedImage
  -- ^ One tracked subresource per mip of the single bloom image.
  , bloomExtents :: V.Vector Vk.Extent2D
  , downSets :: V.Vector Vk.DescriptorSet
  , upSets :: V.Vector Vk.DescriptorSet
  , probe :: Maybe ManagedImage
  -- ^ Snapshot of the mip the luminance pass reduces ('lumProbe'); headless only.
  , hiz :: SharedHiZ
  -- ^ The depth pyramid, shared across the frames in flight.
  , hizSets :: V.Vector Vk.DescriptorSet
  -- ^ One per per-level reduce; the remaining mips are the fused tail's.
  , hizTailSet :: Maybe Vk.DescriptorSet
  -- ^ 'Nothing' when the chain ends at the last per-level reduce.
  , cullSet :: Vk.DescriptorSet
  -- ^ The cull's buffers + the whole pyramid, sampled ("Pipeline.Cull").
  }

-- | Level sizes of a half-extent-based pyramid (bloom and hiz alike).
halfPyramidExtents :: Vk.Extent2D -> Int -> V.Vector Vk.Extent2D
halfPyramidExtents (Vk.Extent2D w0 h0) n =
  V.generate n \i -> Vk.Extent2D (max 1 ((w0 `div` 2) `shiftR` i)) (max 1 ((h0 `div` 2) `shiftR` i))

{- | The depth pyramid, shared across the frames in flight.

Graphics-only history: built from a frame's depth, sampled by the /next/ frame's
cull. One shared pyramid keeps that a 1-frame loop; a per-frame copy would read
two-frame-stale occlusion and pop harder. Extent-dependent, so 'allocateSharedHiZ'
reruns on resize and its owner frees it; the reduce/cull sets that bind it stay
per-target ('allocateTargets').
-}
data SharedHiZ = SharedHiZ
  { pyramidMips :: V.Vector ManagedImage
  -- ^ Depth-pyramid mips, each a tracked subresource.
  , pyramidReduceExtents :: V.Vector Vk.Extent2D
  -- ^ Dispatch extents of the per-level reduces only; the tail sizes itself.
  , pyramidViews :: V.Vector Vk.ImageView
  -- ^ Per-mip views, for the reduce descriptor sets.
  , pyramidFullView :: Vk.ImageView
  -- ^ Whole-chain view, sampled by the cull ('textureLod').
  , pyramidPrimed :: IORef Bool
  {- ^ False until the first cull pass records; gates the occlusion test off the
  not-yet-built pyramid.
  -}
  }

-- | Allocate the shared depth pyramid at a scene extent (half-res base to 1×1).
allocateSharedHiZ :: VMA.Allocator -> Vk.Device -> SliceRegistry -> Vk.Extent2D -> ResourceT IO SharedHiZ
allocateSharedHiZ allocator dev registry extent = do
  let
    halfExtent = halfExtentOf extent
    mipCount = HiZ.mipCount halfExtent
    allExtents = halfPyramidExtents extent mipCount
    reduceCount = 1 + V.length (V.takeWhile (not . HiZ.tailFits) allExtents)
  (_, (hizImage, pyramidViews)) <-
    allocateMipChain allocator dev HiZ.format halfExtent (fromIntegral mipCount) (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) "hiz"
  -- Resize destroys the pyramid while the not-yet-reconciled frame slots still
  -- reach its wrappers: purge them with the image, or a recycled handle clashes.
  _ <- register (forgetImage registry hizImage)
  pyramidMips <- V.generateM mipCount \i -> describedMip registry HiZ.format (allExtents V.! i) hizImage Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral i)
  pyramidFullView <- HiZ.allocateChainView dev hizImage mipCount
  pyramidPrimed <- liftIO (newIORef False)
  pure SharedHiZ{pyramidMips, pyramidReduceExtents = V.take reduceCount allExtents, pyramidViews, pyramidFullView, pyramidPrimed}

{- | Allocate the extent-dependent scene over a shared 'SceneStatic'.

The render targets, the bloom pyramid, and the descriptor sets binding extent-sized
views. No GPU submit — cheap enough to rerun on every resize. The visibility buffer is
CONCURRENT across @sharedFamilies@ (the async graphics + compute pair).

The hi-Z pyramid ('SharedHiZ') is passed in, not allocated here, so both frames in
flight share one pyramid (1-frame occlusion history); only the reduce/cull sets that
bind it are per-target.

@wantProbe@ adds the debug-only luminance probe ('lumProbe'), which costs a copy per
frame; the windowed driver never reads it.
-}
allocateTargets
  :: VMA.Allocator
  -> Vk.Device
  -> SliceRegistry
  -> ScenePipelines
  -> SceneStatic
  -> Vk.Extent2D
  -> SharedHiZ
  -> Maybe (Word32, Word32)
  -> Bool
  -> ResourceT IO Scene
allocateTargets allocator dev registry pls static extent hiz sharedFamilies wantProbe = do
  -- vis + depth also get TRANSFER_SRC so the headless driver can dump them.
  (_, (visImage, visView)) <-
    allocateTarget allocator dev visFormat extent (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT (fmap (\(g, c) -> [g, c]) sharedFamilies) "visibility"
  -- SAMPLED so the windowed driver's depth-pyramid build can read it.
  (_, (depthImage, depthView)) <-
    allocateTarget allocator dev depthFormat extent (Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) Vk.IMAGE_ASPECT_DEPTH_BIT Nothing "depth"
  -- The post chain: shade → colorHDR → tone → display. tone (windowed blit),
  -- display (headless readback) and colorHDR (windowed debug-view blit) are
  -- presentation sources, so they need TRANSFER_SRC.
  -- colorHDR is sampled by the first bloom downsample, so it needs SAMPLED too.
  (_, (colorHDRImage, colorHDRView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "colorHDR"
  (_, (toneImage, toneView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "tone"
  (_, (displayImage, displayView)) <-
    -- EXCLUSIVE on purpose: the gamma pass writes it on the compute queue and
    -- the headless readback copy reads it on graphics, so the graph hands it
    -- over with a real queue-family ownership transfer.
    allocateTarget allocator dev colorFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "display"
  vis <- sharedAcrossQueues <$> describedImage registry visFormat extent visImage Vk.IMAGE_ASPECT_COLOR_BIT
  depth <- describedImage registry depthFormat extent depthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  colorHDR <- describedImage registry hdrFormat extent colorHDRImage Vk.IMAGE_ASPECT_COLOR_BIT
  tone <- describedImage registry hdrFormat extent toneImage Vk.IMAGE_ASPECT_COLOR_BIT
  display <- describedImage registry colorFormat extent displayImage Vk.IMAGE_ASPECT_COLOR_BIT

  -- Bloom pyramid: one mipped image (base = half the scene extent); each mip is a
  -- tracked subresource and a down/up descriptor set (sharing the static sampler).
  let
    halfExtent = halfExtentOf extent
    mipExtents = halfPyramidExtents extent
    mipCount = bloomMipCount extent
    bloomExtents = mipExtents mipCount
    -- The probe copies off the metered mip, so the chain needs TRANSFER_SRC for it.
    bloomUsage = Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. (if wantProbe then Vk.IMAGE_USAGE_TRANSFER_SRC_BIT else zero)
    lumMip = lumMipFor mipCount
  (_, (bloomImage, bloomViews)) <-
    allocateMipChain allocator dev hdrFormat halfExtent (fromIntegral mipCount) bloomUsage "bloom"
  bloomMips <- V.generateM mipCount \i -> describedMip registry hdrFormat (bloomExtents V.! i) bloomImage Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral i)
  -- The luminance probe: a snapshot of the metered mip, taken before the upsample
  -- overwrites it. Written on the compute queue and read back on the graphics one, so
  -- it is CONCURRENT — the graph places plain transitions, never ownership transfers.
  probe <-
    if not wantProbe
      then pure Nothing
      else do
        (_, probeImage) <-
          allocateImage allocator dev hdrFormat (bloomExtents V.! lumMip) (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) (fmap (\(g, c) -> [g, c]) sharedFamilies) "lumProbe"
        Just <$> describedImage registry hdrFormat (bloomExtents V.! lumMip) probeImage Vk.IMAGE_ASPECT_COLOR_BIT
  downSets <- V.generateM mipCount \i -> Bloom.allocateSet dev pls.bloom.down static.sampler (if i == 0 then colorHDRView else bloomViews V.! (i - 1)) (bloomViews V.! i)
  upSets <- V.generateM (mipCount - 1) \i -> Bloom.allocateSet dev pls.bloom.up static.sampler (bloomViews V.! (i + 1)) (bloomViews V.! i)

  -- The cull's occlusion test reads the shared pyramid ('SharedHiZ'); only the
  -- reduce/cull descriptor sets are per-target (reduce mip 0 reads this depthView,
  -- so those sets can't be shared). Fed by the passes in "Rendering.Passes".
  let
    hizViews = hiz.pyramidViews
    hizReduceCount = V.length hiz.pyramidReduceExtents
    hizTailCount = V.length hiz.pyramidMips - hizReduceCount
  hizSets <- V.generateM hizReduceCount \i ->
    HiZ.allocateSet dev pls.hiz.reduce static.nearestSampler (if i == 0 then depthView else hizViews V.! (i - 1)) (hizViews V.! i)
  hizTailSet <-
    if hizTailCount <= 0
      then pure Nothing
      else
        Just
          <$> HiZ.allocateTailSet
            dev
            pls.hiz.tail
            static.nearestSampler
            (hizViews V.! (hizReduceCount - 1))
            (V.slice hizReduceCount hizTailCount hizViews)
  cullSet <-
    Cull.allocateSet
      dev
      pls.cull
      Cull.CullBuffers
        { Cull.objects = static.objectsBuffer
        , Cull.indirect = static.indirect.buffer
        , Cull.visMain = static.visMain.buffer
        , Cull.visOcc = static.visOcc.buffer
        , Cull.lights = static.lightsBuffer
        }
      static.nearestSampler
      hiz.pyramidFullView

  -- The SSAO chain ("Pipeline.Ssao"): half-res DAIS normals, the AO gather over
  -- the depth pyramid, and the two-axis bilateral blur ping-ponging ao → aoBlur
  -- → ao. All on the graphics queue; only the final AO factor crosses to the
  -- (possibly async-compute) resolve, so it alone is CONCURRENT.
  (_, (normalsImage, normalsView)) <-
    allocateTarget allocator dev normalsFormat halfExtent Vk.IMAGE_USAGE_STORAGE_BIT Vk.IMAGE_ASPECT_COLOR_BIT Nothing "normals"
  (_, (aoImage, aoView)) <-
    allocateTarget allocator dev aoFormat halfExtent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) Vk.IMAGE_ASPECT_COLOR_BIT (fmap (\(g, c) -> [g, c]) sharedFamilies) "ao"
  (_, (aoBlurImage, aoBlurView)) <-
    allocateTarget allocator dev aoFormat halfExtent Vk.IMAGE_USAGE_STORAGE_BIT Vk.IMAGE_ASPECT_COLOR_BIT Nothing "aoBlur"
  normals <- describedImage registry normalsFormat halfExtent normalsImage Vk.IMAGE_ASPECT_COLOR_BIT
  ao <- sharedAcrossQueues <$> describedImage registry aoFormat halfExtent aoImage Vk.IMAGE_ASPECT_COLOR_BIT
  aoBlur <- describedImage registry aoFormat halfExtent aoBlurImage Vk.IMAGE_ASPECT_COLOR_BIT
  normalsSet <- Ssao.allocateNormalsSet dev pls.ssao.normals visView normalsView static.vertexBuffer static.objectsBuffer static.meshTableBuffer
  aoSet <- Ssao.allocateAoSet dev pls.ssao.ao static.nearestSampler hiz.pyramidFullView normalsView aoView
  aoBlurXSet <- Ssao.allocateBlurSet dev pls.ssao.blur normalsView aoView aoBlurView
  aoBlurYSet <- Ssao.allocateBlurSet dev pls.ssao.blur normalsView aoBlurView aoView

  shadeSet <- Shade.allocateDescriptorSet dev pls.shade visView colorHDRView static.vertexBuffer static.lightsBuffer static.sampler static.shadowCubeView static.objectsBuffer static.materialsBuffer static.meshTableBuffer aoView
  lumSet <- Luminance.allocateSet dev pls.luminance (bloomViews V.! lumMip) static.lumBuffer
  toneSet <- Tonemap.allocateSet dev pls.tonemap colorHDRView toneView static.sampler (bloomViews V.! 0) static.exposureBuffer
  gammaSet <- Gamma.allocateSet dev pls.gamma toneView displayView
  gammaDebugSet <- Gamma.allocateSet dev pls.gamma colorHDRView displayView

  pure
    Scene
      { static
      , targets = SceneTargets{vis, visView, depth, depthView, colorHDR, colorHDRView, tone, toneView, display, displayView, normals, normalsView, ao, aoView, aoBlur, aoBlurView}
      , shadeSet
      , normalsSet
      , aoSet
      , aoBlurXSet
      , aoBlurYSet
      , lumSet
      , toneSet
      , gammaSet
      , gammaDebugSet
      , bloomMips
      , bloomExtents
      , downSets
      , upSets
      , probe
      , hiz
      , hizSets
      , hizTailSet
      , cullSet
      }

-- | The tracked visibility and depth images after a frame, for headless debug dumps.
debugImages :: Scene -> (ManagedImage, ManagedImage)
debugImages scene = (scene.targets.vis, scene.targets.depth)

-- | The moments slice holding light 0, for a debug face dump: the static lights', or the orbs' when there are none.
shadowImage :: Scene -> Maybe ManagedImage
shadowImage scene = scene.static.bakedMoments <|> fmap (.moments) scene.static.orbShadow

{- | The luminance probe after a frame (in @TRANSFER_DST_OPTIMAL@), with its extent.

Exactly the pixels the reduction averaged: 'lumMipIndex' of the downsample chain,
snapshotted before the upsample overwrote it. 'Nothing' unless the scene was allocated
with @wantProbe@.
-}
lumProbe :: Scene -> Maybe (ManagedImage, Vk.Extent2D)
lumProbe scene = fmap (\p -> (p, scene.bloomExtents V.! lumMipFor (V.length scene.bloomExtents))) scene.probe

{- | Half the extent, floor-rounded and clamped to 1: the SSAO targets' size and
the bloom/hi-z pyramid base.
-}
halfExtentOf :: Vk.Extent2D -> Vk.Extent2D
halfExtentOf (Vk.Extent2D w h) = Vk.Extent2D (max 1 (w `div` 2)) (max 1 (h `div` 2))

{- | Bloom mip the luminance pass reduces.

Mip 0 is already half-resolution, so mip 2 is ⅛ of the scene extent — few enough
pixels for one workgroup, pre-averaged, and still wide enough to bin later.
-}
lumMipIndex :: Int
lumMipIndex = 2

-- | 'lumMipIndex', clamped to a chain of @mipCount@ mips.
lumMipFor :: Int -> Int
lumMipFor mipCount = min lumMipIndex (mipCount - 1)

{- | Bloom mip count for an extent.

Halve from half-resolution until a level would drop below 8 px, capped at 6.
-}
bloomMipCount :: Vk.Extent2D -> Int
bloomMipCount (Vk.Extent2D w h) =
  max 1 (min 6 (length (takeWhile (>= 8) (iterate (`div` 2) (min w h `div` 2)))))

{- | Read back the last frame's geometric-mean luminance.

The luminance pass wrote it; the driver derives an exposure from it. GPU→CPU
memory may be non-coherent, so the host cache is invalidated before the peek.
-}
readLuminance :: (MonadIO m) => Scene -> m Float
readLuminance scene = liftIO do
  VMA.invalidateAllocation scene.static.allocator scene.static.lumAllocation 0 Vk.WHOLE_SIZE
  peek (castPtr (scene.static.lumMapped `plusPtr` 4))

-- | Write the tonemap exposure (the meter host pass does, or the caller per frame).
setExposure :: (MonadIO m) => Scene -> Float -> m ()
setExposure scene e = liftIO do
  poke (castPtr scene.static.exposureMapped) e
  VMA.flushAllocation scene.static.allocator scene.static.exposureAllocation 0 Vk.WHOLE_SIZE
