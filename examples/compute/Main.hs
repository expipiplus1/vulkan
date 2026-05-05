{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main
  ( main
  )
where

import qualified Codec.Picture as JP
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Identity (Identity (..))
import qualified Data.Vector as V
import Data.Word
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (sizeOf)
import Say

import qualified Vma
import Vulkan.CStruct.Extends
import Vulkan.CStruct.Utils
  ( FixedArray
  , lowerArrayPtr
  )
import Vulkan.Core10 as Vk hiding
  ( withBuffer
  , withImage
  )
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Requirement (InstanceRequirement (..))
import Vulkan.Utils.Debug (debugCallbackPtr)
import qualified Vulkan.Utils.Init.Headless as Init
import Vulkan.Utils.Initialization
  ( createDeviceFromRequirements
  , physicalDeviceName
  , pickPhysicalDevice
  )
import Vulkan.Utils.QueueAssignment
  ( QueueFamilyIndex (..)
  , QueueSpec (..)
  , assignQueues
  , isComputeQueueFamily
  )
import Vulkan.Utils.ShaderQQ.GLSL.Glslang
import Vulkan.Zero
import VulkanMemoryAllocator as VMA hiding
  ( getPhysicalDeviceProperties
  )
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))

----------------------------------------------------------------
-- The program
----------------------------------------------------------------

main :: IO ()
main = runResourceT $ do
  inst <- Main.createInstance
  (phys, computeQueueFamilyIndex, dev) <- Main.createDevice inst
  allocator <- Vma.createVMA zero myApiVersion inst phys dev

  image <-
    render allocator dev computeQueueFamilyIndex
      `finally` deviceWaitIdle dev
  let filename = "julia.png"
  sayErr $ "Writing " <> filename
  liftIO $ BSL.writeFile filename (JP.encodePng image)

-- | Render the Julia set
render
  :: Allocator
  -> Device
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
        { size = fromIntegral $ width * height * 4 * sizeOf (0 :: Float)
        , usage = BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { AllocationCreateInfo.flags = ALLOCATION_CREATE_MAPPED_BIT
        , usage = MEMORY_USAGE_GPU_TO_CPU
        }
      allocate

  -- Create a descriptor set and layout for this buffer
  (descriptorSet, descriptorSetLayout) <- do
    (_, descriptorPool) <-
      withDescriptorPool
        dev
        zero
          { maxSets = 1
          , poolSizes = [DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
          }
        Nothing
        allocate

    (_, descriptorSetLayout) <-
      withDescriptorSetLayout
        dev
        zero
          { bindings =
              [ zero
                  { binding = 0
                  , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
                  , descriptorCount = 1
                  , stageFlags = SHADER_STAGE_COMPUTE_BIT
                  }
              ]
          }
        Nothing
        allocate

    -- Don't use `withDescriptorSets`: the set is freed when the pool is.
    [descriptorSet] <-
      allocateDescriptorSets
        dev
        zero
          { descriptorPool = descriptorPool
          , setLayouts = [descriptorSetLayout]
          }
    pure (descriptorSet, descriptorSetLayout)

  updateDescriptorSets
    dev
    [ SomeStruct
        zero
          { dstSet = descriptorSet
          , dstBinding = 0
          , descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER
          , descriptorCount = 1
          , bufferInfo = [DescriptorBufferInfo buffer 0 WHOLE_SIZE]
          }
    ]
    []

  -- Create our shader and compute pipeline
  shader <- createShader dev
  (_, pipelineLayout) <-
    withPipelineLayout
      dev
      zero{PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]}
      Nothing
      allocate
  let
    pipelineCreateInfo :: ComputePipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { layout = pipelineLayout
        , stage = shader
        , basePipelineHandle = zero
        }
  (_, (_, [computePipeline])) <-
    withComputePipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
      allocate

  -- Create a command buffer
  let commandPoolCreateInfo =
        zero
          { CommandPoolCreateInfo.queueFamilyIndex = computeQueueFamilyIndex
          }
  (_, commandPool) <- withCommandPool dev commandPoolCreateInfo Nothing allocate
  let commandBufferAllocateInfo =
        zero
          { commandPool = commandPool
          , level = COMMAND_BUFFER_LEVEL_PRIMARY
          , commandBufferCount = 1
          }
  (_, [commandBuffer]) <- withCommandBuffers dev commandBufferAllocateInfo allocate

  -- Fill command buffer
  useCommandBuffer
    commandBuffer
    zero{CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    $ do
      cmdBindPipeline
        commandBuffer
        PIPELINE_BIND_POINT_COMPUTE
        computePipeline
      cmdBindDescriptorSets
        commandBuffer
        PIPELINE_BIND_POINT_COMPUTE
        pipelineLayout
        0
        [descriptorSet]
        []
      cmdDispatch
        commandBuffer
        (ceiling (realToFrac width / realToFrac @_ @Float workgroupX))
        (ceiling (realToFrac height / realToFrac @_ @Float workgroupY))
        1

  -- Create a fence so we can know when render is finished
  (_, fence) <- withFence dev zero Nothing allocate
  let submitInfo = zero{commandBuffers = [commandBufferHandle commandBuffer]}
  computeQueue <- getDeviceQueue dev computeQueueFamilyIndex 0
  queueSubmit computeQueue [SomeStruct submitInfo] fence
  let fenceTimeout = 1e9 -- 1 second
  waitForFences dev [fence] True fenceTimeout >>= \case
    TIMEOUT -> throwString "Timed out waiting for compute"
    _ -> pure ()

  -- If the buffer allocation is not HOST_COHERENT this will ensure the changes
  -- are present on the CPU.
  invalidateAllocation allocator bufferAllocation 0 WHOLE_SIZE

  -- TODO: speed this bit up, it's hopelessly slow
  let
    pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
    pixelAddr x y =
      plusPtr
        (mappedData bufferAllocationInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  liftIO $
    JP.withImage
      width
      height
      ( \x y -> do
          let ptr = pixelAddr x y
          [r, g, b, a] <-
            fmap (\f -> round (f * 255))
              <$> peekArray 4 (lowerArrayPtr ptr)
          pure $ JP.PixelRGBA8 r g b a
      )

-- | Create a compute shader
createShader
  :: Device
  -> ResourceT IO (SomeStruct PipelineShaderStageCreateInfo)
createShader dev = do
  let compCode =
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
  (_, compModule) <- withShaderModule dev zero{code = compCode} Nothing allocate
  let compShaderStageCreateInfo =
        zero
          { stage = SHADER_STAGE_COMPUTE_BIT
          , module' = compModule
          , name = "main"
          }
  pure $ SomeStruct compShaderStageCreateInfo

----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

-- | Create an instance with a debug messenger and validation layer.
createInstance :: (MonadResource m) => m Instance
createInstance = do
  inst <-
    Init.withInstance
      (Just zero{applicationName = Nothing, apiVersion = myApiVersion})
      [ RequireInstanceExtension
          { instanceExtensionLayerName = Nothing
          , instanceExtensionName = EXT_DEBUG_UTILS_EXTENSION_NAME
          , instanceExtensionMinVersion = minBound
          }
      ]
      [ RequireInstanceLayer
          { instanceLayerName = "VK_LAYER_KHRONOS_validation"
          , instanceLayerMinVersion = minBound
          }
      ]
  let debugMessengerCreateInfo =
        zero
          { messageSeverity =
              DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
          , messageType =
              DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
          , pfnUserCallback = debugCallbackPtr
          }
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

createDevice
  :: (MonadResource m, MonadThrow m)
  => Instance
  -> m (PhysicalDevice, Word32, Device)
createDevice inst = do
  mPd <- pickPhysicalDevice inst hasComputeQueue id
  (_, phys) <-
    maybe
      (throwString "Unable to find appropriate PhysicalDevice")
      pure
      mPd
  sayErr . ("Using device: " <>) =<< physicalDeviceName phys

  mAssign <-
    assignQueues
      phys
      (Identity (QueueSpec 1 (\_ q -> pure (isComputeQueueFamily q))))
  (qInfos, getQs) <-
    maybe
      (throwString "Unable to assign compute queue")
      pure
      mAssign
  dev <-
    createDeviceFromRequirements
      []
      []
      phys
      zero{queueCreateInfos = SomeStruct <$> qInfos}
  Identity (QueueFamilyIndex computeFamilyIdx, _q) <- liftIO (getQs dev)
  pure (phys, computeFamilyIdx, dev)
  where
    hasComputeQueue :: (MonadIO m) => PhysicalDevice -> m (Maybe Word64)
    hasComputeQueue phys = do
      qProps <- getPhysicalDeviceQueueFamilyProperties phys
      if V.any isComputeQueueFamily qProps
        then do
          heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
          pure (Just (sum (DI.size <$> heaps)))
        else pure Nothing
