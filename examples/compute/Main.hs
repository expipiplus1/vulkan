{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main
  )
where

import qualified Codec.Picture as JP
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Word (Word32)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (captureImageRGBA8, savePng)
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan compute example"
        , instanceReqs = []
        , deviceReqs = []
        , vmaFlags = zero
        }
  let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)

  image <- render allocator device computeQueueFamilyIndex
  Vk.deviceWaitIdle device
  savePng "julia.png" image

-- | Render the Julia set
render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev computeQueueFamilyIndex = do
  let
    width, height, workgroupX, workgroupY :: Int
    width = 512
    height = width
    workgroupX = 32
    workgroupY = 4

  -- Create a buffer into which to render. Mapped + GPU_TO_CPU so the host can
  -- read the image back.
  (_, (buffer, bufferAllocation, bufferAllocationInfo)) <-
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

  (descriptorSet, descriptorSetLayout) <- do
    (_, descriptorPool) <-
      Vk.withDescriptorPool
        dev
        zero
          { Vk.maxSets = 1
          , Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
          }
        Nothing
        allocate

    (_, descriptorSetLayout) <-
      Vk.withDescriptorSetLayout
        dev
        zero
          { Vk.bindings =
              [ zero
                  { Vk.binding = 0
                  , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
                  , Vk.descriptorCount = 1
                  , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
                  }
              ]
          }
        Nothing
        allocate

    -- Don't use `withDescriptorSets`: the set is freed when the pool is.
    [descriptorSet] <-
      Vk.allocateDescriptorSets
        dev
        zero
          { Vk.descriptorPool = descriptorPool
          , Vk.setLayouts = [descriptorSetLayout]
          }
    pure (descriptorSet, descriptorSetLayout)

  Vk.updateDescriptorSets
    dev
    [ SomeStruct
        zero
          { Vk.dstSet = descriptorSet
          , Vk.dstBinding = 0
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
          , Vk.descriptorCount = 1
          , Vk.bufferInfo = [Vk.DescriptorBufferInfo buffer 0 Vk.WHOLE_SIZE]
          }
    ]
    []

  (_, shader) <- shaderStage dev Vk.SHADER_STAGE_COMPUTE_BIT () compCode
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]}
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
    Vk.withComputePipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
      allocate

  let commandPoolCreateInfo =
        zero
          { CommandPoolCreateInfo.queueFamilyIndex = computeQueueFamilyIndex
          }
  (_, commandPool) <- Vk.withCommandPool dev commandPoolCreateInfo Nothing allocate
  let commandBufferAllocateInfo =
        zero
          { Vk.commandPool = commandPool
          , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vk.commandBufferCount = 1
          }
  (_, [cb]) <- Vk.withCommandBuffers dev commandBufferAllocateInfo allocate

  Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
    Vk.cmdBindPipeline
      cb
      Vk.PIPELINE_BIND_POINT_COMPUTE
      computePipeline
    Vk.cmdBindDescriptorSets
      cb
      Vk.PIPELINE_BIND_POINT_COMPUTE
      pipelineLayout
      0
      [descriptorSet]
      []
    Vk.cmdDispatch
      cb
      (ceiling (realToFrac width / realToFrac @_ @Float workgroupX))
      (ceiling (realToFrac height / realToFrac @_ @Float workgroupY))
      1

  computeQueue <- Vk.getDeviceQueue dev computeQueueFamilyIndex 0
  submitAndWait dev computeQueue cb "Timed out waiting for compute"

  -- TODO: speed this bit up, it's hopelessly slow
  let
    pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
    pixelAddr x y =
      plusPtr
        (VMA.mappedData bufferAllocationInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  captureImageRGBA8 allocator bufferAllocation width height $ \x y -> do
    let ptr = pixelAddr x y
    [r, g, b, a] <-
      fmap (\f -> round (f * 255))
        <$> peekArray 4 (lowerArrayPtr ptr)
    pure $ JP.PixelRGBA8 r g b a

compCode =
  [comp|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        const int width = 512;
        const int height = width;
        const int workgroup_x = 32;
        const int workgroup_y = 4;

        // r^2 - r = |c|
        const vec2 c = vec2(-0.8, 0.156);
        const float r = 0.5 * (1 + sqrt (4 * dot(c,c) + 1));

        layout (local_size_x = workgroup_x, local_size_y = workgroup_y, local_size_z = 1 ) in;
        layout(std140, binding = 0) buffer buf
        {
           vec4 imageData[];
        };


        // From https://iquilezles.org/www/articles/palettes/palettes.htm
        //
        // Traditional Julia blue and orange
        vec3 color(const float t) {
          const vec3 a = vec3(0.5);
          const vec3 b = vec3(0.5);
          const vec3 c = vec3(8);
          const vec3 d = vec3(0.5, 0.6, 0.7);
          return a + b * cos(6.28318530718 * (c * t + d));
        }

        // complex multiplication
        vec2 mulC(const vec2 a, const vec2 b) {
          return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
        }

        vec2 f(const vec2 z) {
          return mulC(z,z) + c;
        }

        // Algorithm from https://en.wikipedia.org/wiki/Julia_set
        void main() {
          vec2 z = vec2
            ( float(gl_GlobalInvocationID.y) / float(height) * 2 * r - r
            , float(gl_GlobalInvocationID.x) / float(width) * 2 * r - r
            );

          uint iteration = 0;
          const int max_iteration = 1000;

          while (dot(z,z) < dot(r,r) && iteration < max_iteration) {
            z = f(z);
            iteration++;
          }

          const uint i = width * gl_GlobalInvocationID.y + gl_GlobalInvocationID.x;
          if (iteration == max_iteration) {
            imageData[i] = vec4(0,0,0,1);
          } else {
            imageData[i] = vec4(color(float(iteration) / float(max_iteration)),1);
          }
        }
      |]
