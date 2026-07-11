{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The headless frame: upload the scene, camera and device-address-linked BVH,
then path-trace in horizontal bands and read the image back.
-}
module Render
  ( Options (..)
  , render
  ) where

import qualified Codec.Picture as JP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import qualified Data.Vector.Storable as VS
import Data.Word (Word32)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec3 (vec3)
import HeadlessBoot (submitAndWaitFor)
import ImageReadback (captureImageRGBA8)
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo (..), getBufferDeviceAddress)
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import qualified Vulkan.Utils.SpirV.Array as Array
import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress (..))

import Pathtracer (BvhNode, Frame (..), Sphere)
import qualified Pathtracer
import qualified Scene

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
    spheres = VS.fromList (Scene.buildScene opts.spheres opts.seed)
    sphereCount = VS.length spheres

    -- One BVH leaf per sphere, flattened to an array with the root at index 0.
    bvhFlats = Scene.flattenBvh (Scene.buildBvh (zip [0 ..] (map Scene.sphereAabb (VS.toList spheres))))

    aspect = fromIntegral width / fromIntegral height
    camera = Scene.buildCamera aspect opts.fov (vec3 13 2 3) (vec3 0 0 0) (vec3 0 1 0)

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
      (VS.fromList (map (Scene.toBvhNode bvhBase nodeStride) bvhFlats))

  -- The reflected 'Frame' push constant carries the root node's address
  -- (the flattened BVH puts the root at index 0, i.e. the buffer base).
  let frame =
        Frame
          { root = DeviceAddress bvhBase
          , resolution = uvec2 (fromIntegral width) (fromIntegral height)
          , seed = opts.seed
          , rowOffset = 0 -- set per band below
          }

  pathtracer <- Pathtracer.allocatePipeline dev opts.samples opts.bounces

  -- One descriptor set, its pool sized from the reflected bindings.
  descriptorSet <- Pipeline.allocateSet dev pathtracer 0

  Vk.updateDescriptorSets
    dev
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER camBuffer
    , bufferWrite descriptorSet 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER sceneBuffer
    , bufferWrite descriptorSet 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
    ]
    []

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
      Pipeline.bind cb pathtracer
      Pipeline.bindSet cb pathtracer 0 descriptorSet
      -- Push the reflected 'Frame' (std430) with this band's row offset.
      Pipeline.push cb pathtracer bandFrame
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
