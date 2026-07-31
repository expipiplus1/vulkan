{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | The single headless frame: dispatch the Julia pipeline and read the image back.
module Render
  ( render
  ) where

import qualified Codec.Picture as JP
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Word (Word32)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec2 (vec2)
import HeadlessBoot (submitAndWait)
import ImageReadback (captureImageRGBA8)
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Julia (Params (..))
import qualified Julia

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev computeQueueFamilyIndex = do
  let
    width, height :: Int
    width = 512
    height = width

    -- Push-constant block (reflected as 'Params').
    params :: Params
    params =
      Params
        { center = vec2 (-0.8) 0.156
        , resolution = uvec2 (fromIntegral width) (fromIntegral height)
        }

    -- Specialization constants, in ascending constant_id order:
    -- id 0 = maxIterations (uint), id 1 = escapeRadius (float).
    maxIterations :: Word32
    maxIterations = 1000
    escapeRadius :: Float
    escapeRadius = 2.0

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

  julia <- Julia.allocatePipeline dev maxIterations escapeRadius

  -- One descriptor set, its pool sized from the reflected bindings.
  descriptorSet <- Pipeline.allocateSet dev julia 0

  Vk.updateDescriptorSets
    dev
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
    ]
    []

  (_, commandPool) <-
    Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = computeQueueFamilyIndex} Nothing allocate
  (_, [cb]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = commandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate

  Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
    Pipeline.bind cb julia
    Pipeline.bindSet cb julia 0 descriptorSet
    -- Push the reflected 'Params' (std430) for this dispatch.
    Pipeline.push cb julia params
    Vk.cmdDispatch
      cb
      (ceiling (realToFrac width / realToFrac @_ @Float Julia.workgroup))
      (ceiling (realToFrac height / realToFrac @_ @Float Julia.workgroup))
      1

  computeQueue <- Vk.getDeviceQueue dev computeQueueFamilyIndex 0
  submitAndWait dev computeQueue cb "Timed out waiting for compute"

  let
    pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
    pixelAddr x y =
      plusPtr
        (VMA.mappedData outInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  captureImageRGBA8 allocator outAllocation width height $ \x y -> do
    let ptr = pixelAddr x y
    [r, g, b, a] <- fmap (\f -> round (f * 255)) <$> peekArray 4 (lowerArrayPtr ptr)
    pure $ JP.PixelRGBA8 r g b a
