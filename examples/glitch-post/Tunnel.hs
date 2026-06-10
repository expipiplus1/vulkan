{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

{-| The offscreen wormhole, with everything it needs in one place: the
geometry compute pipeline that grows it, the graphics pipeline that draws
it, their shaders, and the camera descriptor machinery.

The geometry lives entirely on the GPU: a compute pass rebuilds the whole
vertex list in /eye space/ from the elapsed time every frame, writing an
SSBO; the graphics pipeline has no vertex input at all — its vertex shader
pulls positions and colours from that SSBO by @gl_VertexIndex@ and projects
them with a @mat4@ read from the set-0 uniform buffer ('CameraSlot'), so
interpolation across the quads is perspective-correct. The pipeline layouts
are built here and handed to the helpers — the caller-supplied-layout path.

The scene: a tunnel of checkerboarded neon ring tiles flying toward the
viewer, twisting as they come, with a slowly tumbling octahedron hanging in
the middle of the tube. The rings pass /through/ the octahedron — the depth
attachment is what sorts them, not draw order.
-}
module Tunnel
  ( TunnelPipeline (..)
  , createTunnelPipeline
  , CameraSlot (..)
  , createCameraSlots
  , createTunnelVertexBuffer
  , createTunnelGeometrySet
  , dispatchTunnelGeometry
  , tunnelProjection
  , tunnelTwistRate
  , tunnelVertexCount
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, release)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
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

data TunnelPipeline = TunnelPipeline
  { tpPipeline :: Vk.Pipeline
  -- ^ Graphics; pulls its vertices from the SSBO.
  , tpPipelineLayout :: Vk.PipelineLayout
  , tpDescriptorSetLayout :: Vk.DescriptorSetLayout
  {- ^ Set 0: binding 0 the camera UBO ('CameraSlot'), binding 1 the
  vertex SSBO (read).
  -}
  , tpGeometryPipeline :: Vk.Pipeline
  -- ^ Compute; writes the SSBO.
  , tpGeometryLayout :: Vk.PipelineLayout
  , tpGeometrySetLayout :: Vk.DescriptorSetLayout
  -- ^ Set 0, binding 0: the vertex SSBO (write).
  }

{- | Build the wormhole's two pipelines: the geometry compute pass and the
scene graphics pipeline (one colour attachment plus a depth attachment,
driven dynamically, full always-on dynamic state).

Both pipeline layouts are built here and passed to the helpers — the
graphics one exercises the caller-supplied-layout path of
'Dynamic.createPipelineFromShaders', which otherwise would default to an
empty transient layout.
-}
createTunnelPipeline
  :: Vk.Device
  -> Vk.Format
  -- ^ Offscreen colour format.
  -> Vk.Format
  -- ^ Depth format.
  -> ResourceT IO TunnelPipeline
createTunnelPipeline dev colorFormat depthFormat = do
  -- The graphics half.
  (_, descriptorSetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero{Vk.bindings = [cameraBinding, vertexPullBinding]}
      Nothing
      allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]}
      Nothing
      allocate
  (_, pipeline) <-
    Dynamic.createPipelineFromShaders
      dev
      [colorFormat]
      (Just depthFormat)
      zero -- no vertex input at all: the vertex shader pulls from the SSBO
      Nothing -- full always-on dynamic state
      (Just pipelineLayout) -- set 0: camera UBO + vertex SSBO
      () -- no specialization constants
      [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
      , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
      ]

  -- The geometry compute half.
  (_, geometrySetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero{Vk.bindings = [geometryBinding]}
      Nothing
      allocate
  (_, geometryLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = [geometrySetLayout]
        , Vk.pushConstantRanges =
            [Vk.PushConstantRange Vk.SHADER_STAGE_COMPUTE_BIT 0 20] -- time, banner, beat, energy, twist
        }
      Nothing
      allocate
  (shaderKey, shader) <- shaderStage dev Vk.SHADER_STAGE_COMPUTE_BIT () geomCode
  let
    geometryCreateInfo :: Vk.ComputePipelineCreateInfo '[]
    geometryCreateInfo =
      zero
        { Vk.layout = geometryLayout
        , Vk.stage = shader
        , Vk.basePipelineHandle = zero
        }
  (_, (_, [geometryPipeline])) <-
    Vk.withComputePipelines dev zero [SomeStruct geometryCreateInfo] Nothing allocate
  release shaderKey

  pure
    TunnelPipeline
      { tpPipeline = pipeline
      , tpPipelineLayout = pipelineLayout
      , tpDescriptorSetLayout = descriptorSetLayout
      , tpGeometryPipeline = geometryPipeline
      , tpGeometryLayout = geometryLayout
      , tpGeometrySetLayout = geometrySetLayout
      }
  where
    cameraBinding =
      zero
        { Vk.binding = 0
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
        }
    vertexPullBinding =
      zero
        { Vk.binding = 1
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
        }
    geometryBinding =
      zero
        { Vk.binding = 0
        , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
        , Vk.descriptorCount = 1
        , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
        }

----------------------------------------------------------------
-- The vertex SSBO and its descriptor sets
----------------------------------------------------------------

{- | The device-local SSBO the geometry pass writes and the vertex shader
pulls from. One buffer serves every frame: the WAR hazard against the
previous frame's reads is the caller's barrier to issue ("Main" does).
-}
createTunnelVertexBuffer :: VMA.Allocator -> ResourceT IO Vk.Buffer
createTunnelVertexBuffer vma = do
  (_, (buffer, _, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = fromIntegral (tunnelVertexCount * floatsPerVertex * sizeOf (0 :: Float))
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
      allocate
  pure buffer

-- | The geometry pass's single descriptor set, binding the vertex SSBO.
createTunnelGeometrySet
  :: Vk.Device
  -> Vk.DescriptorSetLayout
  -- ^ 'tpGeometrySetLayout'.
  -> Vk.Buffer
  -- ^ 'createTunnelVertexBuffer'.
  -> ResourceT IO Vk.DescriptorSet
createTunnelGeometrySet dev layout buffer = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
        }
      Nothing
      allocate
  [set] <-
    Vk.allocateDescriptorSets
      dev
      zero{Vk.descriptorPool = pool, Vk.setLayouts = [layout]}
  Vk.updateDescriptorSets
    dev
    [bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER buffer]
    []
  pure set

----------------------------------------------------------------
-- Camera slots (one per frame in flight)
----------------------------------------------------------------

{- | One in-flight slot of the camera UBO: the set to bind and where to poke
the matrix.
-}
data CameraSlot = CameraSlot
  { csSet :: Vk.DescriptorSet
  , csBuffer :: Vk.Buffer
  {- ^ The slot's UBO, exposed so other pipelines' sets can bind the same
  matrix (the particles do).
  -}
  , csProjection :: Ptr Float
  {- ^ Persistently mapped, host-coherent; holds the 16 'tunnelProjection'
  floats.
  -}
  }

{- | One camera UBO + descriptor set per frame in flight, persistently
mapped. The caller writes 'tunnelProjection' into the slot keyed by frame
parity before recording: frame @N+2@ cannot begin recording until frame @N@
retires, so overwriting @N@'s slot is safe while @N+1@ still renders from
its own.

Each set also binds the vertex SSBO at binding 1 for the vertex shader to
pull from.
-}
createCameraSlots
  :: VMA.Allocator
  -> Vk.Device
  -> Vk.DescriptorSetLayout
  -- ^ 'tpDescriptorSetLayout'.
  -> Int
  -- ^ Slot count = frames in flight.
  -> Vk.Buffer
  -- ^ 'createTunnelVertexBuffer'.
  -> ResourceT IO (V.Vector CameraSlot)
createCameraSlots vma dev layout n vertexBuffer = do
  buffers <- V.replicateM n do
    (_, (buffer, _, allocInfo)) <-
      VMA.withBuffer
        vma
        zero
          { Vk.size = 64 -- one mat4
          , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
          }
        zero
          { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
          , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
          , AllocationCreateInfo.requiredFlags =
              Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
          }
        allocate
    pure (buffer, castPtr (VMA.mappedData allocInfo))
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = fromIntegral n
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER (fromIntegral n)
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (fromIntegral n)
            ]
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
        ( \(set, (buffer, _)) ->
            [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER buffer
            , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER vertexBuffer
            ]
        )
        (V.zip sets buffers)
    )
    []
  pure $
    V.zipWith
      (\set (buffer, ptr) -> CameraSlot{csSet = set, csBuffer = buffer, csProjection = ptr})
      sets
      buffers

{- | The camera projection for the given viewport aspect ratio (width /
height), as the 16 floats of a column-major @mat4@ ready to poke into
'csProjection'.

Eye space here is Vulkan-flavoured: x right, y down (matching NDC), z
straight into the screen. The matrix divides x and y by the
(aspect-corrected) eye z and maps eye z hyperbolically so @zNear@ lands on
depth 0 and @zFar@ on 1 for the @LESS@ test.
-}
tunnelProjection :: Float -> [Float]
tunnelProjection aspect =
  [ 1 / aspect
  , 0
  , 0
  , 0
  , 0
  , 1
  , 0
  , 0
  , 0
  , 0
  , zFar / range
  , 1
  , 0
  , 0
  , -(zFar * zNear) / range
  , 0
  ]
  where
    range = zFar - zNear

----------------------------------------------------------------
-- The geometry pass
----------------------------------------------------------------

-- | Keep in sync with the constants at the top of 'geomCode'.
ringCount, segmentCount, faceCount, floatsPerVertex :: Int
ringCount = 28
segmentCount = 20
faceCount = 8
floatsPerVertex = 6

-- | Tiles per ring is half the segments — the checkerboard skips the rest.
tileCount :: Int
tileCount = ringCount * segmentCount `div` 2

{- | What 'dispatchTunnelGeometry' writes and @cmdDraw@ should consume:
six vertices per tile, three per octahedron face (triangle list).
-}
tunnelVertexCount :: Int
tunnelVertexCount = tileCount * 6 + faceCount * 3

-- | Keep in sync with the @local_size_x@ in 'geomCode'.
workgroupSize :: Int
workgroupSize = 64

{- | Radians per second of whole-tube rotation at full spin: the caller
integrates @'tunnelTwistRate' * 'Tune.kSpin'@ into the angle handed to
'dispatchTunnelGeometry' (an angle, not a rate, so reversing the spin
can't snap the rotation).
-}
tunnelTwistRate :: Float
tunnelTwistRate = 0.4

{- | Bind the geometry pipeline and SSBO set, push the time, the
accumulated twist angle, and the knobs the tunnel performs (banner dims
it, the kick thumps the octahedron, the energy lights the tiles), and
dispatch one invocation per tile or face. The caller owns the barriers on
both sides of the dispatch.
-}
dispatchTunnelGeometry
  :: (MonadIO m)
  => TunnelPipeline
  -> Vk.DescriptorSet
  -- ^ 'createTunnelGeometrySet'.
  -> Float
  -- ^ Elapsed seconds (drives the motion, not the sync).
  -> Float
  -- ^ Accumulated whole-tube twist angle (see 'tunnelTwistRate').
  -> Knobs
  -> Vk.CommandBuffer
  -> m ()
dispatchTunnelGeometry tp set t twist knobs cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (tpGeometryPipeline tp)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE (tpGeometryLayout tp) 0 [set] []
  liftIO $ withArray [t, kBanner knobs, kBeat knobs, kEnergy knobs, twist] \p ->
    Vk.cmdPushConstants cb (tpGeometryLayout tp) Vk.SHADER_STAGE_COMPUTE_BIT 0 20 (castPtr p)
  Vk.cmdDispatch
    cb
    (fromIntegral ((tileCount + faceCount + workgroupSize - 1) `quot` workgroupSize))
    1
    1

{- | The eye-space depth range mapped onto the @[0,1]@ depth buffer.
Keep in sync with 'geomCode'.
-}
zNear, zFar :: Float
zNear = 0.7
zFar = 14

----------------------------------------------------------------
-- Shaders
----------------------------------------------------------------

geomCode :: ByteString
geomCode =
  [comp|
    #version 450

    layout(local_size_x = 64, local_size_y = 1, local_size_z = 1) in;

    layout(set = 0, binding = 0, std430) writeonly buffer Vertices { float verts[]; };

    layout(push_constant) uniform Push {
      float time;
      float banner;
      float beat;
      float energy;
      float twist; // accumulated whole-tube rotation angle
    } push;

    const int ringCount = 28;
    const int segmentCount = 20;
    const int tileCount = ringCount * segmentCount / 2;
    const int faceCount = 8;
    const float zNear = 0.7;
    const float zFar = 14.0;
    const float TAU = 6.283185307179586;

    // Same construction as the hsv in the other examples' shaders.
    vec3 hsv(float h, float s, float v) {
      vec3 k = mod(vec3(5.0, 3.0, 1.0) + fract(h) * 6.0, 6.0);
      return v - v * s * clamp(min(k, 4.0 - k), 0.0, 1.0);
    }

    void putVertex(int i, vec3 p, vec3 c) {
      verts[i * 6 + 0] = p.x;
      verts[i * 6 + 1] = p.y;
      verts[i * 6 + 2] = p.z;
      verts[i * 6 + 3] = c.r;
      verts[i * 6 + 4] = c.g;
      verts[i * 6 + 5] = c.b;
    }

    void main() {
      int id = int(gl_GlobalInvocationID.x);
      float t = push.time;

      if (id < tileCount) {
        // One checkerboard tile: skipping every other segment (offset by the
        // ring parity) is what makes the tube read as a rotating lattice.
        int k = id / (segmentCount / 2);
        int j = 2 * (id % (segmentCount / 2)) + (k % 2);

        // 0 = at the camera, 1 = the far end; rings cycle toward the viewer.
        float cyc = fract(float(k) / float(ringCount) - 0.06 * t);
        float z0 = zNear + (zFar - zNear) * cyc;
        float z1 = min(zFar, z0 + 0.5);
        // The tube twists along its length and slowly rotates; the whole-
        // tube angle arrives pre-integrated so its direction can reverse
        // smoothly (Melody B).
        float twist = 1.8 * cyc + push.twist;
        float radius = 1.7 + 0.2 * sin(3.0 * t + TAU * cyc);
        // 0.92 instead of 1 leaves a thin gap between tiles.
        float a0 = TAU * float(j) / float(segmentCount) + twist;
        float a1 = TAU * (float(j) + 0.92) / float(segmentCount) + twist;
        vec2 r0 = radius * vec2(cos(a0), sin(a0));
        vec2 r1 = radius * vec2(cos(a1), sin(a1));
        // Hue runs along the tube; tiles dim with distance and fade to black
        // just before wrapping past the camera so the pop is invisible.
        // The tube lights up with the section energy and steps back while
        // the banner holds the stage.
        vec3 colour = (0.35 + 0.65 * push.energy) * (1.0 - 0.55 * push.banner) * hsv(
          float(k) / float(ringCount) + 0.06 * t,
          0.85,
          pow(max(0.0, 1.0 - cyc), 1.6) * smoothstep(0.0, 0.06, cyc));

        int base = id * 6;
        putVertex(base + 0, vec3(r0, z0), colour);
        putVertex(base + 1, vec3(r1, z0), colour);
        putVertex(base + 2, vec3(r1, z1), colour);
        putVertex(base + 3, vec3(r0, z0), colour);
        putVertex(base + 4, vec3(r1, z1), colour);
        putVertex(base + 5, vec3(r0, z1), colour);
      } else if (id < tileCount + faceCount) {
        // One face of the octahedron tumbling in the tube's middle; it
        // breathes idly and thumps with the kick.
        int n = id - tileCount;
        float r = 0.42 + 0.05 * sin(2.3 * t) + 0.22 * push.beat;
        float ax = 0.7 * t;
        float ay = 0.9 * t;
        vec3 corners[6] = vec3[6](
          vec3(r, 0, 0), vec3(-r, 0, 0),
          vec3(0, r, 0), vec3(0, -r, 0),
          vec3(0, 0, r), vec3(0, 0, -r));
        ivec3 faces[8] = ivec3[8](
          ivec3(0, 2, 4), ivec3(2, 1, 4), ivec3(1, 3, 4), ivec3(3, 0, 4),
          ivec3(2, 0, 5), ivec3(1, 2, 5), ivec3(3, 1, 5), ivec3(0, 3, 5));
        // Per-face brightness stands in for lighting; it crawls with time.
        // Dimmed with the banner like the tiles — it sits right behind it.
        vec3 colour = (0.75 + 0.35 * push.energy) * (1.0 - 0.55 * push.banner)
          * hsv(0.93, 0.35, 0.45 + 0.55 * fract(float(n) * 0.381 + 0.15 * t));

        for (int v = 0; v < 3; ++v) {
          vec3 p = corners[faces[n][v]];
          // Tumble around x then y, then hang it down the tube.
          p.yz = vec2(cos(ax) * p.y - sin(ax) * p.z, sin(ax) * p.y + cos(ax) * p.z);
          p.xz = vec2(cos(ay) * p.x + sin(ay) * p.z, cos(ay) * p.z - sin(ay) * p.x);
          p.z += 2.8;
          putVertex(tileCount * 6 + n * 3 + v, p, colour);
        }
      }
    }
  |]

vertCode :: ByteString
vertCode =
  [vert|
    #version 450

    layout(set = 0, binding = 0) uniform Camera {
      mat4 projection;
    } camera;

    layout(set = 0, binding = 1, std430) readonly buffer Vertices { float verts[]; };

    layout(location = 0) out vec3 fragColor;

    void main() {
      int i = gl_VertexIndex * 6;
      vec3 pos = vec3(verts[i + 0], verts[i + 1], verts[i + 2]);
      // The projection puts w = eye z, so fragColor interpolates
      // perspective-correctly.
      gl_Position = camera.projection * vec4(pos, 1.0);
      fragColor = vec3(verts[i + 3], verts[i + 4], verts[i + 5]);
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
