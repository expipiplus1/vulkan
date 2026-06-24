{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| The glitch post-processing compute pipeline, with everything it needs in
one place: its descriptor layout (scene image in, glitched image out), its
push-constant block (the elapsed time), its shader, and the dispatch helper.

The pipeline is built once and never recreated; only the descriptor sets are
per swapchain, because they bind the per-swapchain-image storage views.

The shader is the demoscene bit: datamoshed blocks, a sweeping scanline tear,
beat-pumped chromatic aberration, a poor-man's bloom, and CRT dressing
(scanlines, vignette, grain), all driven by a single time push constant.
-}
module Glitch
  ( GlitchPipeline (..)
  , createGlitchPipeline
  , createGlitchDescriptorSets
  , dispatchGlitch
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, release)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)
import Tune (Knobs (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (imageWrite)
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)
import Vulkan.Zero (zero)

data GlitchPipeline = GlitchPipeline
  { gpPipeline :: Vk.Pipeline
  , gpPipelineLayout :: Vk.PipelineLayout
  , gpDescriptorSetLayout :: Vk.DescriptorSetLayout
  -- ^ Binding 0: the scene (read); binding 1: the glitched output (write).
  }

createGlitchPipeline :: Vk.Device -> ResourceT IO GlitchPipeline
createGlitchPipeline dev = do
  (_, descriptorSetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero{Vk.bindings = [storageImageBinding 0, storageImageBinding 1]}
      Nothing
      allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = [descriptorSetLayout]
        , Vk.pushConstantRanges =
            -- time + the five performance knobs
            [Vk.PushConstantRange Vk.SHADER_STAGE_COMPUTE_BIT 0 24]
        }
      Nothing
      allocate
  (shaderKey, shader) <- shaderStage dev Vk.SHADER_STAGE_COMPUTE_BIT () compCode
  let
    pipelineCreateInfo :: Vk.ComputePipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.layout = pipelineLayout
        , Vk.stage = shader
        , Vk.basePipelineHandle = zero
        }
  (_, (_, [pipeline])) <-
    Vk.withComputePipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate
  release shaderKey
  pure
    GlitchPipeline
      { gpPipeline = pipeline
      , gpPipelineLayout = pipelineLayout
      , gpDescriptorSetLayout = descriptorSetLayout
      }
  where
    storageImageBinding binding =
      zero
        { Vk.binding = binding
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
        }

{- | One descriptor set per @(scene view, output view)@ pair, each bound to
its pair. Allocated from a fresh pool so releasing the key frees the lot.
-}
createGlitchDescriptorSets
  :: Vk.Device
  -> Vk.DescriptorSetLayout
  -> V.Vector (Vk.ImageView, Vk.ImageView)
  -- ^ Per swapchain image: the scene view to read, the output view to write.
  -> ResourceT IO (ReleaseKey, V.Vector Vk.DescriptorSet)
createGlitchDescriptorSets dev layout views = do
  let n = V.length views
  (poolKey, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = fromIntegral n
        , Vk.poolSizes =
            [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE (fromIntegral (2 * n))]
        }
      Nothing
      allocate

  -- Sets are freed automatically when the pool is destroyed.
  sets <-
    Vk.allocateDescriptorSets
      dev
      zero{Vk.descriptorPool = pool, Vk.setLayouts = V.replicate n layout}

  Vk.updateDescriptorSets
    dev
    ( V.concatMap
        ( \(set, (sceneView, outView)) ->
            [write set 0 sceneView, write set 1 outView]
        )
        (V.zip sets views)
    )
    []
  pure (poolKey, sets)
  where
    write set binding view =
      imageWrite set binding Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL view

{- | Bind the pipeline and set, push the time and the performance knobs,
and dispatch over the extent. Both images must already be in @GENERAL@
layout.
-}
dispatchGlitch
  :: (MonadIO m)
  => GlitchPipeline
  -> Vk.Extent2D
  -> Vk.DescriptorSet
  -> Float
  -- ^ Elapsed seconds (drives the motion, not the sync).
  -> Knobs
  -> Vk.CommandBuffer
  -> m ()
dispatchGlitch gp (Vk.Extent2D w h) set t knobs cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (gpPipeline gp)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE (gpPipelineLayout gp) 0 [set] []
  liftIO $ withArray [t, kBanner knobs, kBeat knobs, kEnergy knobs, kBuild knobs, kImpact knobs] \p ->
    Vk.cmdPushConstants cb (gpPipelineLayout gp) Vk.SHADER_STAGE_COMPUTE_BIT 0 24 (castPtr p)
  Vk.cmdDispatch
    cb
    ((w + workgroupX - 1) `quot` workgroupX)
    ((h + workgroupY - 1) `quot` workgroupY)
    1

-- | Keep in sync with the @local_size_x@/@local_size_y@ in 'compCode'.
workgroupX, workgroupY :: Word32
workgroupX = 8
workgroupY = 8

compCode :: ByteString
compCode =
  [comp|
    #version 450

    layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

    layout(set = 0, binding = 0, rgba8) uniform readonly image2D scene;
    layout(set = 0, binding = 1, rgba8) uniform writeonly image2D glitched;

    layout(push_constant) uniform Push {
      float time;
      float banner;
      float beat;
      float energy;
      float build;
      float impact;
    } push;

    float hash(vec2 p) {
      return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123);
    }

    // Edge-clamped read, so displaced lookups never leave the image.
    vec3 fetch(ivec2 p, ivec2 size) {
      return imageLoad(scene, clamp(p, ivec2(0), size - 1)).rgb;
    }

    void main() {
      ivec2 size = imageSize(scene);
      ivec2 p = ivec2(gl_GlobalInvocationID.xy);
      if (p.x >= size.x || p.y >= size.y) return;

      float t = push.time;

      // The tune drives the corruption: the kick envelope pumps it, the
      // snare rolls swell it into the drops. While the banner holds, the
      // displacements calm down so the text stays legible (the CRT
      // dressing and bloom stay).
      float calm = 1.0 - 0.75 * push.banner;
      float beat = clamp(push.energy * (0.15 + 0.85 * push.beat) + push.build, 0.0, 1.3) * calm;

      // Where to read the scene from; the glitches displace this.
      ivec2 q = p;

      // Datamosh blocks: a sparse, rehashed-7-times-a-second set of 24px
      // cells shoves its pixels sideways (and a little vertically).
      vec2 cell = floor(vec2(p) / 24.0);
      float cellSeed = hash(cell + floor(t * 7.0));
      if (cellSeed > 1.0 - 0.25 * beat) {
        q.x += int((cellSeed - 0.5) * 120.0);
        q.y += int((hash(cell.yx + floor(t * 7.0)) - 0.5) * 24.0);
      }

      // Scanline tear: every line wobbles a touch; a band around a sweep
      // line rolling down the frame tears hard.
      float line = float(p.y);
      float sweep = fract(t * 0.23) * float(size.y);
      float band = exp(-abs(line - sweep) / 18.0);
      q.x += int(sin(line * 0.11 + t * 9.0) * (1.0 + 60.0 * band * beat));

      // Chromatic aberration grows toward the edges and with the beat:
      // the red and blue taps slide apart horizontally.
      vec2 centered = vec2(p) / vec2(size) - 0.5;
      int shift = int(length(centered) * (6.0 * calm + 26.0 * beat));
      vec3 col = vec3(
        fetch(q + ivec2(shift, 0), size).r,
        fetch(q, size).g,
        fetch(q - ivec2(shift, 0), size).b);

      // Poor-man's bloom: a sparse golden-angle ring of taps around the
      // displaced read; anything bright bleeds outward.
      vec3 glow = vec3(0.0);
      for (int g = 0; g < 8; ++g) {
        float ga = float(g) * 2.39996323;
        ivec2 off = ivec2(round(vec2(cos(ga), sin(ga)) * (2.0 + float(g))));
        glow += max(fetch(q + off, size) - 0.6, vec3(0.0));
      }
      col += 0.22 * glow;

      // Each drop lands a short white flash.
      col = mix(col, vec3(1.1), 0.45 * push.impact);

      // CRT dressing: scanlines, vignette, grain.
      col *= 0.88 + 0.12 * sin(line * 3.14159);
      col *= 1.0 - 0.55 * dot(centered, centered);
      col += (hash(vec2(p) + fract(t) * 100.0) - 0.5) * 0.06;

      imageStore(glitched, p, vec4(col, 1.0));
    }
  |]
