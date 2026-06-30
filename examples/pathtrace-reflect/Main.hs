{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| A single-frame, procedural compute path tracer whose /entire/ shader
interface is derived from the compiled SPIR-V at build time by
@vulkan-utils-spirv@ — exercising every reflected resource kind at once:

  * the @Camera@ uniform block        -> a generated record (std140);
  * the @Scene@ input storage buffer  -> a descriptor binding holding the leaf
    sphere geometry; its element record 'Sphere' is generated from the SSBO's
    array element type;
  * the @Image@ output storage buffer -> a descriptor binding;
  * the @BvhNode@ @buffer_reference@ type -> a generated record whose children are
    @DeviceAddress BvhNode@; the host builds a BVH, links the nodes by device
    address, and the shader hops those addresses to intersect the scene;
  * the @Frame@ push-constant block    -> a generated record + range, carrying the
    root node's device address; and
  * the @SAMPLES@ / @MAX_BOUNCES@ specialization constants -> the pipeline's
    'Vk.SpecializationInfo'.

The scene (a "Ray Tracing in One Weekend" arrangement) is generated on the host
from a command-line seed: the spheres go into the @Scene@ buffer and a BVH over
them into a separate device-address-linked node buffer.
-}
module Main
  ( main
  )
where

import qualified Codec.Picture as JP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Control.Monad.Trans.State (State, evalState, runState, state)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word64)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)
import qualified Geomancy
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec3 (Vec3, emap2, vec3, pattern WithVec3)
import qualified Geomancy.Vec3 as V3
import Geomancy.Vec4 (Vec4, fromVec3, vec4, pattern WithVec4)
import Graphics.Gl.Block (Std140 (..), Std430 (..))
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWaitFor, withHeadlessVk)
import ImageReadback (captureImageRGBA8, savePng)
import Options.Applicative
import System.Random (StdGen, mkStdGen, uniformR)
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo (..), PhysicalDeviceBufferDeviceAddressFeatures (..), getBufferDeviceAddress)
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Requirements.TH (reqs)
import Vulkan.Utils.Shader (shaderModuleStage)
import qualified Vulkan.Utils.SpirV.Array as Array
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress (..))
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Specialization (allocateSpecializationInfo)
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

-- | The compiled shader, embedded for runtime module creation.
compCode :: ByteString
compCode = $(embedFile "pathtrace-reflect/shader.comp.spv")

-- Generate the records from the same SPIR-V the runtime loads:
--   * @Camera@  (std140 UBO);
--   * @Frame@   (std430 push constant); and
--   * @Sphere@  (std430), the element type of the @Scene@ SSBO's @Sphere[]@ —
--     generated even though the runtime-array @Scene@/@Image@ blocks themselves
--     aren't (they're @[Sphere]@ / raw @vec4@ texels on the host).
reflectShaderTypes "pathtrace-reflect/shader.comp.spv"

main :: IO ()
main = do
  opts <- execParser optionsInfo
  runResourceT $ do
    HeadlessVk{..} <-
      withHeadlessVk
        HeadlessConfig
          { appName = "Haskell Vulkan pathtrace-reflect example"
          , instanceReqs = []
          , deviceReqs = bdaDeviceReqs
          , -- the BVH node buffer is reached by device address
            vmaFlags = VMA.ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT
          }
    let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)

    image <- render allocator device computeQueueFamilyIndex opts
    Vk.deviceWaitIdle device
    liftIO $ savePng opts.output image
    liftIO $ putStrLn ("Wrote " <> opts.output)

{- | Enable buffer device addresses so the BVH nodes can be linked on the host
and traversed by 'DeviceAddress' on the GPU.
-}
bdaDeviceReqs :: [DeviceRequirement]
bdaDeviceReqs =
  [reqs| PhysicalDeviceBufferDeviceAddressFeatures.bufferDeviceAddress |]

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> Options
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev computeQueueFamilyIndex opts = do
  let
    width = opts.width
    height = opts.height
    workgroup = 16 :: Int

    -- The runtime-sized Scene SSBO is a std430 array of the reflected 'Sphere';
    -- the BVH bounds each sphere from its reflected centerRadius (a 'Vec4').
    spheres :: VS.Vector Sphere
    spheres = VS.fromList (buildScene opts)
    sphereCount = VS.length spheres

    -- One BVH leaf per sphere, flattened to an array with the root at index 0.
    bvhFlats = flattenBvh (buildBvh (zip [0 ..] (map sphereAabb (VS.toList spheres))))

    aspect = fromIntegral width / fromIntegral height
    camera = buildCamera aspect opts.fov (vec3 13 2 3) (vec3 0 0 0) (vec3 0 1 0)

  -- Reflect the embedded module once; reuse it for the descriptor set layout,
  -- push-constant range and specialization info.
  reflected <- reflectBytes compCode

  -- Output storage buffer: one RGBA32F texel per pixel, mapped GPU_TO_CPU.
  (_, (outBuffer, outAllocation, outInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral $ width * height * 4 * sizeOf (0 :: Float)
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }
      allocate

  -- Input storage buffer: the scene's spheres, written from the host.
  (_, (sceneBuffer, _sceneAllocation, sceneInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (sphereCount * Array.std430Stride (Proxy @Sphere))
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate
  liftIO $ Array.pokeStd430 (VMA.mappedData sceneInfo) spheres

  -- Uniform buffer holding the reflected 'Camera', written from the host.
  (_, (camBuffer, _camAllocation, camInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (sizeOf camera)
        , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate
  liftIO $ poke (castPtr (VMA.mappedData camInfo)) camera

  -- BVH node buffer, reached purely by device address (not a descriptor).
  -- Allocate it, learn its base address, then write each node with its
  -- children linked by address — the generated 'BvhNode' record's
  -- @DeviceAddress BvhNode@ fields carry the pointers the shader hops.
  let
    nodeStride = Array.std430Stride (Proxy @BvhNode)
    nodeCount = length bvhFlats
  (_, (nodeBuffer, _nodeAllocation, nodeInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (nodeCount * nodeStride)
        , Vk.usage =
            Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
              .|. Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate
  bvhBase <- getBufferDeviceAddress dev zero{buffer = nodeBuffer}
  liftIO $
    Array.pokeStd430
      (VMA.mappedData nodeInfo)
      (VS.fromList (map (toBvhNode bvhBase nodeStride) bvhFlats))

  -- The reflected 'Frame' push constant carries the root node's address
  -- (the flattened BVH puts the root at index 0, i.e. the buffer base).
  let frame =
        Frame
          { root = DeviceAddress bvhBase
          , resolution = uvec2 (fromIntegral width) (fromIntegral height)
          , seed = opts.seed
          , rowOffset = 0 -- set per band below
          }

  -- Descriptor set layout generated from the reflected module (UBO + 2 SSBOs).
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate

  (_, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 2
            ]
        }
      Nothing
      allocate
  [descriptorSet] <-
    Vk.allocateDescriptorSets
      dev
      zero
        { Vk.descriptorPool = descriptorPool
        , Vk.setLayouts = [descriptorSetLayout]
        }

  Vk.updateDescriptorSets
    dev
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER camBuffer
    , bufferWrite descriptorSet 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER sceneBuffer
    , bufferWrite descriptorSet 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
    ]
    []

  -- Pipeline. The specialization info (SAMPLES, MAX_BOUNCES) is built from the
  -- reflected constant ids and lives for this resource scope.
  mSpec <- allocateSpecializationInfo reflected (opts.samples, opts.bounces)
  (_, shader) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT mSpec compCode
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]
        , PipelineLayoutCreateInfo.pushConstantRanges =
            V.fromList (pushConstantRanges reflected)
        }
      Nothing
      allocate
  let
    pipelineCreateInfo :: Vk.ComputePipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.layout = pipelineLayout
        , Vk.stage = shader
        , Vk.basePipelineHandle = zero
        }
  (_, (_, [computePipeline])) <-
    Vk.withComputePipelines dev zero [SomeStruct pipelineCreateInfo] Nothing allocate

  -- Command buffer, reset and re-recorded per band (hence the pool's
  -- RESET_COMMAND_BUFFER flag).
  (_, commandPool) <-
    Vk.withCommandPool
      dev
      zero
        { CommandPoolCreateInfo.queueFamilyIndex = computeQueueFamilyIndex
        , CommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        }
      Nothing
      allocate
  (_, [cb]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = commandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate
  computeQueue <- Vk.getDeviceQueue dev computeQueueFamilyIndex 0
  let timeoutNanos = round (opts.timeout * 1e9)

  -- Split the dispatch into horizontal bands so no single compute submission runs
  -- long enough to trip the GPU's hang-recovery watchdog — which resets the queue
  -- and drops the unfinished (bottom) workgroups, corrupting that region. Each
  -- band is an independent short submission; a low-cost render stays one dispatch.
  let
    samples = max 1 (fromIntegral opts.samples) :: Int
    bounces = max 1 (fromIntegral opts.bounces) :: Int
    -- A conservative per-submission budget in ray-bounces (well under what tripped
    -- the watchdog here, with margin for heavier scenes).
    budget = 6 * 1000 * 1000 * 1000 :: Int
    bandRows = max 1 (min height (budget `div` (width * samples * bounces)))
  for_ ([0, bandRows .. height - 1] :: [Int]) $ \row0 -> do
    let
      rowsThis = min bandRows (height - row0)
      bandFrame = frame{rowOffset = fromIntegral row0}
    Vk.resetCommandBuffer cb zero
    Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE computePipeline
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pipelineLayout 0 [descriptorSet] []
      -- Push the reflected 'Frame' (std430) with this band's row offset.
      liftIO $ with bandFrame $ \pFrame ->
        Vk.cmdPushConstants
          cb
          pipelineLayout
          Vk.SHADER_STAGE_COMPUTE_BIT
          0
          (fromIntegral (sizeOf bandFrame))
          (castPtr pFrame)
      Vk.cmdDispatch
        cb
        (ceiling (realToFrac width / realToFrac @_ @Float workgroup))
        (ceiling (realToFrac rowsThis / realToFrac @_ @Float workgroup))
        1
    submitAndWaitFor timeoutNanos dev computeQueue cb $
      "Timed out waiting for compute band at row "
        <> show row0
        <> " after "
        <> show opts.timeout
        <> "s (raise --timeout)"

  let
    pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
    pixelAddr x y =
      plusPtr
        (VMA.mappedData outInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  captureImageRGBA8 allocator outAllocation width height $ \x y -> do
    let ptr = pixelAddr x y
    [r, g, b, a] <- fmap (\f -> round (min 1 f * 255)) <$> peekArray 4 (lowerArrayPtr ptr)
    pure $ JP.PixelRGBA8 r g b a

--------------------------------------------------------------------------------
-- Scene
--------------------------------------------------------------------------------

{- | Build the reflected 'Camera' for a simple look-at camera with unit focal
length (no defocus blur).
-}
buildCamera :: Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Camera
buildCamera aspect vfovDeg lookFrom lookAt vup =
  Camera
    { origin = lookFrom
    , lowerLeft = lowerLeft
    , horizontal = horizontal
    , vertical = vertical
    , skyTop = vec3 0.5 0.7 1.0
    , skyBottom = vec3 1.0 1.0 1.0
    }
  where
    theta = vfovDeg * pi / 180
    viewportH = 2 * tan (theta / 2)
    viewportW = aspect * viewportH
    w = V3.normalize (lookFrom - lookAt)
    u = V3.normalize (V3.cross vup w)
    v = V3.cross w u
    horizontal = u V3.^* viewportW
    vertical = v V3.^* viewportH
    lowerLeft = lookFrom - (horizontal V3.^* 0.5) - (vertical V3.^* 0.5) - w

{- | The full scene: ground, three feature spheres, and @--spheres@ random small
ones laid out on a grid (seeded by @--seed@).
-}
buildScene :: Options -> [Sphere]
buildScene opts = ground : features ++ small
  where
    ground = lambertian (vec3 0 (-1000) 0) 1000 (vec3 0.5 0.5 0.5)
    features =
      [ dielectric (vec3 0 1 0) 1 1.5
      , lambertian (vec3 (-4) 1 0) 1 (vec3 0.4 0.2 0.1)
      , metal (vec3 4 1 0) 1 (vec3 0.7 0.6 0.5) 0
      ]
    small = evalState (smallSpheres opts.spheres) (mkStdGen (fromIntegral opts.seed))

{- | Up to @n@ small spheres scattered over a grid, skipping any too close to the
feature spheres. Materials are randomly diffuse\/metal\/glass.
-}
smallSpheres :: Int -> State StdGen [Sphere]
smallSpheres n = catMaybes <$> mapM gen (take n grid)
  where
    -- A square field of unit cells centred on the origin, sized to hold @n@
    -- spheres so they expand toward the horizon as the count grows. (@n = 484@
    -- reproduces the classic @[-11 .. 10]^2@ "Ray Tracing in One Weekend" grid.)
    side = ceiling (sqrt (fromIntegral (max 1 n) :: Double)) :: Int
    lo = negate (side `div` 2)
    grid = [(a, b) | a <- [lo .. lo + side - 1], b <- [lo .. lo + side - 1]]
    gen (a, b) = do
      rx <- rF (0, 0.9)
      rz <- rF (0, 0.9)
      let
        center = vec3 (fromIntegral a + rx) 0.2 (fromIntegral b + rz)
        d = center - vec3 4 0.2 0
      -- Skip spheres too close to the metal feature sphere (compare squared).
      if V3.dot d d <= 0.9 * 0.9
        then pure Nothing
        else do
          choose <- rF (0, 1)
          fmap Just $
            if choose < (0.8 :: Float)
              then do
                c1 <- rF (0, 1)
                c2 <- rF (0, 1)
                c3 <- rF (0, 1)
                pure $ lambertian center 0.2 (vec3 (c1 * c1) (c2 * c2) (c3 * c3))
              else
                if choose < 0.95
                  then do
                    c1 <- rF (0.5, 1)
                    c2 <- rF (0.5, 1)
                    c3 <- rF (0.5, 1)
                    fz <- rF (0, 0.5)
                    pure $ metal center 0.2 (vec3 c1 c2 c3) fz
                  else pure $ dielectric center 0.2 1.5
    rF range = state (uniformR range)

-- Sphere constructors (material encoding matches the shader). The center is a
-- 'Vec3' and the albedo a 'Vec3'; @centerRadius@ packs the radius into w.

mkSphere :: Vec3 -> Float -> Vec4 -> Vec4 -> Sphere
mkSphere center r = Sphere (fromVec3 center r)

lambertian :: Vec3 -> Float -> Vec3 -> Sphere
lambertian c r albedo = mkSphere c r (fromVec3 albedo 1) (vec4 0 0 0 0)

metal :: Vec3 -> Float -> Vec3 -> Float -> Sphere
metal c r albedo fuzz = mkSphere c r (fromVec3 albedo 1) (vec4 1 fuzz 0 0)

dielectric :: Vec3 -> Float -> Float -> Sphere
dielectric c r ior = mkSphere c r (vec4 1 1 1 1) (vec4 2 0 ior 0)

--------------------------------------------------------------------------------
-- BVH (built on the host; linked and traversed on the GPU by device address)
--------------------------------------------------------------------------------

-- | An axis-aligned bounding box (min, max corners).
data Aabb = Aabb !Vec3 !Vec3

-- | Bound a sphere from its reflected @centerRadius@ (xyz centre, w radius).
sphereAabb :: Sphere -> Aabb
sphereAabb sphere = case centerRadius sphere of
  WithVec4 cx cy cz r ->
    Aabb (vec3 (cx - r) (cy - r) (cz - r)) (vec3 (cx + r) (cy + r) (cz + r))

aabbUnion :: Aabb -> Aabb -> Aabb
aabbUnion (Aabb amin amax) (Aabb bmin bmax) =
  Aabb (emap2 min amin bmin) (emap2 max amax bmax)

-- | A binary BVH over sphere indices; each leaf bounds one sphere.
data Bvh = BvhLeaf Int Aabb | BvhSplit Aabb Bvh Bvh

-- | Build a BVH by recursively median-splitting the longest axis.
buildBvh :: [(Int, Aabb)] -> Bvh
buildBvh [] = error "buildBvh: empty scene"
buildBvh [(i, bb)] = BvhLeaf i bb
buildBvh xs = BvhSplit bb (buildBvh l) (buildBvh r)
  where
    bb = foldr1 aabbUnion (map snd xs)
    Aabb lo hi = bb
    axis = longestAxis (hi - lo)
    (l, r) = splitAt (length xs `div` 2) (sortOn (axisKey axis . snd) xs)

longestAxis :: Vec3 -> Int
longestAxis (WithVec3 x y z)
  | x >= y && x >= z = 0
  | y >= z = 1
  | otherwise = 2

-- | Twice the box centre on an axis (the constant factor is irrelevant to sorting).
axisKey :: Int -> Aabb -> Float
axisKey axis (Aabb (WithVec3 mnx mny mnz) (WithVec3 mxx mxy mxz)) = case axis of
  0 -> mnx + mxx
  1 -> mny + mxy
  _ -> mnz + mxz

{- | A flattened node: bounds, child array indices (-1 if none), and the leaf's
sphere index (-1 for an internal node).
-}
data BvhFlat = BvhFlat Vec3 Vec3 Int Int Int

{- | Flatten the tree to an array in pre-order so the root lands at index 0;
children are referenced by array index, later resolved to device addresses.
-}
flattenBvh :: Bvh -> [BvhFlat]
flattenBvh tree = map snd (sortOn fst nodes)
  where
    (_, (_, nodes)) = runState (go tree) (0 :: Int, [])
    fresh = state (\(n, acc) -> (n, (n + 1, acc)))
    emit i f = state (\(n, acc) -> ((), (n, (i, f) : acc)))
    go (BvhLeaf i (Aabb mn mx)) = do
      idx <- fresh
      emit idx (BvhFlat mn mx (-1) (-1) i)
      pure idx
    go (BvhSplit (Aabb mn mx) l r) = do
      idx <- fresh
      li <- go l
      ri <- go r
      emit idx (BvhFlat mn mx li ri (-1))
      pure idx

{- | Realise a flattened node as the reflected 'BvhNode' record, resolving child
indices to device addresses within the node buffer (base + index * stride).
-}
toBvhNode :: Word64 -> Int -> BvhFlat -> BvhNode
toBvhNode base stride (BvhFlat mn mx li ri si) =
  BvhNode
    { boundsMin = fromVec3 mn 0
    , boundsMax = fromVec3 mx 0
    , left = childAddr li
    , right = childAddr ri
    , sphereIndex = fromIntegral si
    }
  where
    childAddr i
      | i < 0 = DeviceAddress 0
      | otherwise = DeviceAddress (base + fromIntegral (i * stride))

--------------------------------------------------------------------------------
-- Command line
--------------------------------------------------------------------------------

data Options = Options
  { width :: Int
  , height :: Int
  , samples :: Word32
  , bounces :: Word32
  , spheres :: Int
  , seed :: Word32
  , fov :: Float
  , timeout :: Double
  -- ^ GPU wait budget, seconds
  , output :: FilePath
  }

optionsInfo :: ParserInfo Options
optionsInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "pathtrace-reflect - reflection-driven compute path tracer"
        <> progDesc
          "Render one frame of a procedural sphere scene with a compute path \
          \tracer whose entire interface is derived from SPIR-V reflection."
    )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option auto (long "width" <> metavar "N" <> value 600 <> showDefault <> help "Image width")
    <*> option auto (long "height" <> metavar "N" <> value 400 <> showDefault <> help "Image height")
    <*> option auto (long "samples" <> metavar "N" <> value 32 <> showDefault <> help "Samples per pixel (spec constant)")
    <*> option auto (long "bounces" <> metavar "N" <> value 12 <> showDefault <> help "Max ray bounces (spec constant)")
    <*> option auto (long "spheres" <> metavar "N" <> value 64 <> showDefault <> help "Random spheres in the scene")
    <*> option auto (long "seed" <> metavar "N" <> value 1 <> showDefault <> help "Scene and sampling seed")
    <*> option auto (long "fov" <> metavar "DEG" <> value 20 <> showDefault <> help "Vertical field of view")
    <*> option auto (long "timeout" <> metavar "SEC" <> value 60 <> showDefault <> help "GPU wait budget in seconds")
    <*> strOption (long "output" <> metavar "FILE" <> value "pathtrace-reflect.png" <> showDefault <> help "Output PNG path")
