{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

{-| The spark swarm, with everything it needs in one place: the simulation
compute pipeline, the graphics pipeline that draws each particle as a small
spinning eye-space triangle (pulled by @gl_VertexIndex@, no vertex input),
and their descriptor machinery.

Unlike the tunnel SSBO — a pure function of time, rewritten whole every
frame — particle state must /survive/ from one frame to the next, so it
ping-pongs between two SSBOs keyed by frame parity: the sim reads the
buffer the previous frame wrote and writes the other; the draw pulls from
the freshly written one. "Main" zero-fills both buffers on the first frame
(age 0 sends every particle through the respawn path) and owns the
barriers.

The cracktro bit: every cycle the whole swarm assembles into a scrolling
banner's worth of glowing text and bursts apart again. The message is laid
out on the CPU once — a 5×7 bitfont, each particle assigned a glyph pixel —
into a third, static SSBO of target positions ('createParticleTargets');
the sim springs particles toward their targets as the phase locks in.

The draw shares the tunnel's camera UBO ('Tunnel.CameraSlot') — same
matrix, bound into this pipeline's own per-parity sets — and takes a
mirror\/brightness /push constant/, so one pipeline draws both the swarm
and its dim, 180°-rotated echo with nothing rebound in between.
-}
module Particles
  ( ParticlePipeline (..)
  , createParticlePipeline
  , createParticleBuffers
  , createParticleTargets
  , createParticleSets
  , dispatchParticleSim
  , drawParticles
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, release)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Foreign.Marshal.Array (pokeArray, withArray)
import Foreign.Ptr (castPtr)
import Tune (Knobs (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp, frag, vert)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

data ParticlePipeline = ParticlePipeline
  { ppPipeline :: Vk.Pipeline
  -- ^ Graphics; one spinning billboard triangle per particle.
  , ppPipelineLayout :: Vk.PipelineLayout
  , ppDescriptorSetLayout :: Vk.DescriptorSetLayout
  -- ^ Set 0: binding 0 the camera UBO, binding 1 the state SSBO (read).
  , ppSimPipeline :: Vk.Pipeline
  -- ^ Compute; integrates the particle states.
  , ppSimLayout :: Vk.PipelineLayout
  , ppSimSetLayout :: Vk.DescriptorSetLayout
  {- ^ Set 0: binding 0 the previous state (read), binding 1 the next
  (write), binding 2 the glyph-pixel targets (read).
  -}
  }

-- | Keep in sync with the constants in 'simCode'.
particleCount :: Int
particleCount = 2048

-- | Keep in sync with the @local_size_x@ in 'simCode'.
workgroupSize :: Int
workgroupSize = 64

-- | vec4 posAge + vec4 velHue, std430.
bytesPerParticle :: Int
bytesPerParticle = 32

{- | Build the swarm's two pipelines: the simulation compute pass and the
billboard graphics pipeline (same attachment shape and dynamic state as the
tunnel's). Both layouts are caller-built; the graphics one rides through
'Dynamic.createPipelineFromShaders'.
-}
createParticlePipeline
  :: Vk.Device
  -> Vk.Format
  -- ^ Offscreen colour format.
  -> Vk.Format
  -- ^ Depth format.
  -> ResourceT IO ParticlePipeline
createParticlePipeline dev colorFormat depthFormat = do
  -- The graphics half.
  (_, descriptorSetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero{Vk.bindings = [cameraBinding, stateBinding]}
      Nothing
      allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]
        , PipelineLayoutCreateInfo.pushConstantRanges =
            [Vk.PushConstantRange Vk.SHADER_STAGE_VERTEX_BIT 0 16] -- mirror, brightness, banner, beat
        }
      Nothing
      allocate
  (_, pipeline) <-
    Dynamic.createPipelineFromShaders
      dev
      [colorFormat]
      (Just depthFormat)
      zero -- no vertex input: the vertex shader pulls from the state SSBO
      Nothing -- full always-on dynamic state
      (Just pipelineLayout) -- set 0: camera UBO + state SSBO
      () -- no specialization constants
      [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
      , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
      ]

  -- The simulation half.
  (_, simSetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero{Vk.bindings = [simStateBinding 0, simStateBinding 1, simStateBinding 2]}
      Nothing
      allocate
  (_, simLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = [simSetLayout]
        , Vk.pushConstantRanges =
            [Vk.PushConstantRange Vk.SHADER_STAGE_COMPUTE_BIT 0 20] -- time, dt, banner, energy, spin
        }
      Nothing
      allocate
  (shaderKey, shader) <- shaderStage dev Vk.SHADER_STAGE_COMPUTE_BIT () simCode
  let
    simCreateInfo :: Vk.ComputePipelineCreateInfo '[]
    simCreateInfo =
      zero
        { Vk.layout = simLayout
        , Vk.stage = shader
        , Vk.basePipelineHandle = zero
        }
  (_, (_, [simPipeline])) <-
    Vk.withComputePipelines dev zero [SomeStruct simCreateInfo] Nothing allocate
  release shaderKey

  pure
    ParticlePipeline
      { ppPipeline = pipeline
      , ppPipelineLayout = pipelineLayout
      , ppDescriptorSetLayout = descriptorSetLayout
      , ppSimPipeline = simPipeline
      , ppSimLayout = simLayout
      , ppSimSetLayout = simSetLayout
      }
  where
    cameraBinding =
      zero
        { Vk.binding = 0
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
        }
    stateBinding =
      zero
        { Vk.binding = 1
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
        }
    simStateBinding binding =
      zero
        { Vk.binding = binding
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
        }

{- | The two device-local state SSBOs the sim ping-pongs between.
@TRANSFER_DST@ so the first frame can @cmdFillBuffer@ them to zero.
-}
createParticleBuffers :: VMA.Allocator -> ResourceT IO (V.Vector Vk.Buffer)
createParticleBuffers vma = V.replicateM 2 do
  (_, (buffer, _, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = fromIntegral (particleCount * bytesPerParticle)
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT
        }
      zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
      allocate
  pure buffer

{- | The static glyph-pixel targets, one vec4 per particle: the banner is
laid out in a 5×7 bitfont and every particle is assigned a lit cell
(round-robin with a little jitter, so a handful of particles thicken each
pixel). Written once from the CPU through a persistently mapped buffer —
host writes are visible to all subsequently submitted commands, no barrier
needed.
-}
createParticleTargets :: VMA.Allocator -> ResourceT IO Vk.Buffer
createParticleTargets vma = do
  (_, (buffer, _, allocInfo)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = fromIntegral (particleCount * 16) -- one vec4 each
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        , AllocationCreateInfo.requiredFlags =
            Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
      allocate
  liftIO $ pokeArray (castPtr (VMA.mappedData allocInfo)) targetFloats
  pure buffer

{- | @[x, y, assigned, 0]@ per particle, in 'Tunnel'-flavoured eye space
(the sim supplies the z plane).
-}
targetFloats :: [Float]
targetFloats =
  concat
    [ let (cx, cy) = cells !! (i `mod` length cells)
      in [cellSize * cx + jitter (3 * i), cellSize * cy + jitter (3 * i + 1), 1, 0]
    | i <- [0 .. particleCount - 1]
    ]
  where
    cells = messageCells
    -- Sized so the banner spans most of the view at the sim's text plane;
    -- cells must stay wider than the billboard radius or letters blob.
    cellSize = 0.07
    jitter k = 0.02 * (hash k - 0.5)
    hash k =
      let x = sin (fromIntegral k * 12.9898) * 43758.5453
      in x - fromIntegral (floor x :: Int)

{- | The lit cells of the banner in cell coordinates: x right, y /down/
(matching the eye space), origin at the banner's centre. Each line is
centred; lines sit 9 cells apart (7-cell glyphs plus 2 of leading).
-}
messageCells :: [(Float, Float)]
messageCells =
  [ (fromIntegral (6 * ci + cx) - lineWidth line / 2, fromIntegral cy - 3 + lineOffset li)
  | (li, line) <- zip [0 :: Int ..] bannerLines
  , (ci, g) <- zip [0 :: Int ..] line
  , (cy, row) <- zip [0 :: Int ..] g
  , (cx, c) <- zip [0 :: Int ..] row
  , c == '#'
  ]
  where
    lineWidth line = fromIntegral (6 * length line - 1) :: Float
    lineOffset li = 9 * (fromIntegral li - fromIntegral (length bannerLines - 1) / 2)

bannerLines :: [[[String]]]
bannerLines = map (map glyph) ["HASKELL", "\9829VULKAN\9829"]

-- | A tiny 5×7 bitfont covering the banner's characters (and a heart).
glyph :: Char -> [String]
glyph = \case
  'A' ->
    [ ".###."
    , "#...#"
    , "#...#"
    , "#####"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'E' ->
    [ "#####"
    , "#...."
    , "#...."
    , "####."
    , "#...."
    , "#...."
    , "#####"
    ]
  'H' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#####"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'K' ->
    [ "#...#"
    , "#..#."
    , "#.#.."
    , "##..."
    , "#.#.."
    , "#..#."
    , "#...#"
    ]
  'L' ->
    [ "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#...."
    , "#####"
    ]
  'N' ->
    [ "#...#"
    , "##..#"
    , "#.#.#"
    , "#..##"
    , "#...#"
    , "#...#"
    , "#...#"
    ]
  'S' ->
    [ ".####"
    , "#...."
    , "#...."
    , ".###."
    , "....#"
    , "....#"
    , "####."
    ]
  'U' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , ".###."
    ]
  'V' ->
    [ "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , "#...#"
    , ".#.#."
    , "..#.."
    ]
  '\9829' ->
    [ ".#.#."
    , "#####"
    , "#####"
    , ".###."
    , "..#.."
    , "....."
    , "....."
    ]
  _ -> replicate 7 "....."

{- | The per-parity descriptor sets: at parity @p@ the sim reads state
buffer @1-p@ and writes buffer @p@ (the static targets ride along in both
parities); the draw reads buffer @p@ alongside parity @p@'s camera UBO.
-}
createParticleSets
  :: Vk.Device
  -> ParticlePipeline
  -> V.Vector Vk.Buffer
  -- ^ The two state buffers ('createParticleBuffers').
  -> Vk.Buffer
  -- ^ The glyph-pixel targets ('createParticleTargets').
  -> V.Vector Vk.Buffer
  -- ^ The camera UBOs by slot (@'Tunnel.csBuffer' \<$\> slots@).
  -> ResourceT IO (V.Vector Vk.DescriptorSet, V.Vector Vk.DescriptorSet)
  -- ^ (sim sets, draw sets), both indexed by frame parity.
createParticleSets dev pp stateBuffers targetBuffer cameraBuffers = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 4
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 8
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 2
            ]
        }
      Nothing
      allocate
  -- Sets are freed automatically when the pool is destroyed.
  simSets <-
    Vk.allocateDescriptorSets
      dev
      zero{Vk.descriptorPool = pool, Vk.setLayouts = V.replicate 2 (ppSimSetLayout pp)}
  drawSets <-
    Vk.allocateDescriptorSets
      dev
      zero{Vk.descriptorPool = pool, Vk.setLayouts = V.replicate 2 (ppDescriptorSetLayout pp)}
  Vk.updateDescriptorSets
    dev
    ( V.concatMap
        ( \p ->
            [ bufferWrite (simSets V.! p) 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (stateBuffers V.! (1 - p))
            , bufferWrite (simSets V.! p) 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (stateBuffers V.! p)
            , bufferWrite (simSets V.! p) 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER targetBuffer
            , bufferWrite (drawSets V.! p) 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER (cameraBuffers V.! p)
            , bufferWrite (drawSets V.! p) 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (stateBuffers V.! p)
            ]
        )
        [0, 1]
    )
    []
  pure (simSets, drawSets)

{- | Bind the sim pipeline and parity set, push time\/dt and the knobs the
sim performs (the banner springs the swarm onto its glyph targets, the
energy drives the spawn velocities), and dispatch one invocation per
particle. The caller owns the barriers on both sides.
-}
dispatchParticleSim
  :: (MonadIO m)
  => ParticlePipeline
  -> Vk.DescriptorSet
  -- ^ This parity's sim set.
  -> Float
  -- ^ Elapsed seconds (drives the motion, not the sync).
  -> Float
  -- ^ Seconds since the previous frame.
  -> Knobs
  -> Vk.CommandBuffer
  -> m ()
dispatchParticleSim pp set t dt knobs cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (ppSimPipeline pp)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE (ppSimLayout pp) 0 [set] []
  liftIO $ withArray [t, dt, kBanner knobs, kEnergy knobs, kSpin knobs] \p ->
    Vk.cmdPushConstants cb (ppSimLayout pp) Vk.SHADER_STAGE_COMPUTE_BIT 0 20 (castPtr p)
  Vk.cmdDispatch
    cb
    (fromIntegral ((particleCount + workgroupSize - 1) `quot` workgroupSize))
    1
    1

{- | Bind the billboard pipeline and parity set, push this draw's
mirror\/brightness, and draw the swarm — three pulled vertices per
particle. Call it again with different push values for an echo; nothing
needs rebinding (the second call's binds are redundant but harmless).
Dynamic state is inherited from whatever the caller set for the pass (the
depth-write toggle is the caller's call).
-}
drawParticles
  :: (MonadIO m)
  => ParticlePipeline
  -> Vk.DescriptorSet
  -- ^ This parity's draw set.
  -> Float
  -- ^ Mirror: 0 draws the swarm where it is, 1 rotates it 180° in eye xy.
  -> Float
  -- ^ Brightness factor.
  -> Knobs
  {- ^ The banner blends every particle toward the banner colour; the
  billboards pump with the kick.
  -}
  -> Vk.CommandBuffer
  -> m ()
drawParticles pp set mirror brightness knobs cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS (ppPipeline pp)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS (ppPipelineLayout pp) 0 [set] []
  liftIO $ withArray [mirror, brightness, kBanner knobs, kBeat knobs] \p ->
    Vk.cmdPushConstants cb (ppPipelineLayout pp) Vk.SHADER_STAGE_VERTEX_BIT 0 16 (castPtr p)
  Vk.cmdDraw cb (fromIntegral (3 * particleCount)) 1 0 0

----------------------------------------------------------------
-- Shaders
----------------------------------------------------------------

simCode :: ByteString
simCode =
  [comp|
    #version 450

    layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

    struct Particle {
      vec4 posAge; // eye-space position, seconds left to live
      vec4 velHue; // eye-space velocity, colour hue
    };

    layout(set = 0, binding = 0, std430) readonly buffer Prev { Particle prev[]; };
    layout(set = 0, binding = 1, std430) writeonly buffer Cur { Particle cur[]; };
    layout(set = 0, binding = 2, std430) readonly buffer Goals { vec4 goals[]; };

    layout(push_constant) uniform Push {
      float time;
      float dt;
      float banner;
      float energy;
      float spin; // +1 with / -1 against; always counter to the tunnel
    } push;

    const int particleCount = 2048;
    // The tunnel's depth range; keep in sync with Tunnel.
    const float zNear = 0.7;
    const float zFar = 14.0;
    // The banner's eye-space plane: in front of the octahedron (z = 2.8).
    const float textZ = 2.0;
    const float TAU = 6.283185307179586;

    float hash(float n) {
      return fract(sin(n) * 43758.5453123);
    }

    void main() {
      int i = int(gl_GlobalInvocationID.x);
      if (i >= particleCount) return;

      Particle p = prev[i];
      if (p.posAge.w <= 0.0) {
        // (Re)spawn down the tube on a random ring angle — the zeroed
        // first frame lands every particle here. The seed drifts with
        // time so the swarm never repeats.
        float fi = float(i) + 64.0 * floor(push.time * 3.0);
        float a = TAU * hash(fi * 12.9898);
        float r = 0.9 + 0.7 * hash(fi * 78.233);
        float z = zNear + (zFar - zNear) * (0.2 + 0.8 * hash(fi * 3.7));
        p.posAge = vec4(r * cos(a), r * sin(a), z, 2.0 + 3.0 * hash(fi * 9.1));
        // A tangential kick (counter to the tube's current twist) plus a
        // push toward the camera, harder when the tune runs hot.
        p.velHue = vec4(vec2(sin(a), -cos(a)) * 0.8 * push.spin,
                        -(1.0 + 2.0 * hash(fi * 5.3)) * (0.6 + 0.9 * push.energy),
                        fract(0.55 + 0.3 * hash(fi * 2.1)));
      } else {
        float phase = push.banner;
        vec2 xy = p.posAge.xy;
        float r = max(length(xy), 1e-3);
        vec2 tangent = vec2(-xy.y, xy.x) / r;
        vec2 radial = xy / r;
        // Swirl against the tube's twist (counter-rotation balances the
        // visuals, and reverses with it for Melody B — the swarm swings
        // around on its own inertia) and spring softly toward the wall
        // radius — released while the banner takes over.
        p.velHue.xy += (1.0 - phase) * (-1.4 * push.spin * tangent - 3.0 * (r - 1.6) * radial) * push.dt;
        vec4 goal = goals[i];
        if (phase > 0.0 && goal.z > 0.5) {
          // Spring toward this particle's glyph pixel (riding a gentle
          // wave), damped hard as the phase locks in so the letters set
          // instead of orbiting.
          vec3 d = vec3(goal.xy, textZ) - p.posAge.xyz;
          d.y += 0.1 * sin(push.time * 2.0 + goal.x * 1.5);
          p.velHue.xyz += 26.0 * phase * d * push.dt;
          p.velHue.xyz *= mix(1.0, exp(-8.0 * push.dt), phase);
          p.posAge.w = max(p.posAge.w, 1.5 * phase); // hold together mid-banner
        }
        p.posAge.xyz += p.velHue.xyz * push.dt;
        p.posAge.w -= push.dt;
        if (p.posAge.z < zNear) {
          p.posAge.w = 0.0; // flew past the camera: respawn next frame
        }
      }
      cur[i] = p;
    }
  |]

vertCode :: ByteString
vertCode =
  [vert|
    #version 450

    layout(set = 0, binding = 0) uniform Camera {
      mat4 projection;
    } camera;

    struct Particle {
      vec4 posAge;
      vec4 velHue;
    };

    layout(set = 0, binding = 1, std430) readonly buffer Particles { Particle parts[]; };

    layout(push_constant) uniform Push {
      float mirror;
      float brightness;
      float banner;
      float beat;
    } push;

    layout(location = 0) out vec3 fragColor;

    const float zNear = 0.7; // keep in sync with the sim
    const float TAU = 6.283185307179586;

    // Same construction as the hsv in the other examples' shaders.
    vec3 hsv(float h, float s, float v) {
      vec3 k = mod(vec3(5.0, 3.0, 1.0) + fract(h) * 6.0, 6.0);
      return v - v * s * clamp(min(k, 4.0 - k), 0.0, 1.0);
    }

    void main() {
      int pi = gl_VertexIndex / 3;
      int corner = gl_VertexIndex % 3;
      Particle p = parts[pi];

      // The echo draw rotates the swarm 180° in eye xy.
      vec3 center = p.posAge.xyz;
      center.xy = mix(center.xy, -center.xy, push.mirror);

      // A small eye-space billboard triangle, spinning as the particle
      // ages and pumping with the kick (held steady while lettering).
      float spin = TAU * fract(float(pi) * 0.381) + 3.0 * p.posAge.w;
      float a = spin + TAU * float(corner) / 3.0;
      float size = 0.045 * (1.0 + 0.5 * push.beat * (1.0 - push.banner));
      vec3 pos = center + size * vec3(cos(a), sin(a), 0.0);
      gl_Position = camera.projection * vec4(pos, 1.0);

      // Dim out at the end of life and just before reaching the camera;
      // while the banner holds, every particle blends to one hot colour
      // so the letters read instead of confetti.
      float life = clamp(p.posAge.w, 0.0, 1.0);
      float near = smoothstep(zNear, zNear + 0.8, p.posAge.z);
      vec3 own = hsv(p.velHue.w, 0.65, 1.0);
      vec3 hot = vec3(1.1, 0.85, 1.0);
      fragColor = push.brightness * life * near * mix(own, hot, push.banner);
    }
  |]

fragCode :: ByteString
fragCode =
  [frag|
    #version 450

    layout(location = 0) in vec3 fragColor;
    layout(location = 0) out vec4 outColor;

    void main() {
      outColor = vec4(fragColor, 1.0);
    }
  |]
