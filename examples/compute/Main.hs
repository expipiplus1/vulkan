{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Codec.Picture                as JP (Image, PixelRGBA8 (..), withImage, encodePng)
import Control.Exception.Safe       (finally, throwString)
import Control.Monad.IO.Class       (MonadIO(..))
import Control.Monad.Trans.Maybe    (MaybeT(..))
import Control.Monad.Trans.Resource (allocate, runResourceT, ResourceT)
import Data.ByteString              (StrictByteString, ByteString)
import Data.ByteString.Lazy         as BSL (writeFile)
import Data.List                    (partition, maximumBy)
import Data.Maybe                   (catMaybes)
import Data.Ord                     (comparing)
import Data.Text                    as T (pack)
import Data.Text.Encoding           (decodeUtf8)
import Data.Vector                  as Vec (toList, fromList, filter, indexed, (!?))
import Foreign                      (Word32, Word64, Ptr, Bits(..), Storable(..), peekArray, castFunPtr, plusPtr)
import Say                          (sayErr)

import Vulkan.CStruct.Extends
import Vulkan.CStruct.Utils          (FixedArray, lowerArrayPtr)
import Vulkan.Core10                 as Vk hiding (withBuffer, withImage)
import Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import Vulkan.Core10                 as CommandPoolCreateInfo (CommandPoolCreateInfo(..))
import Vulkan.Core10                 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo(..))
import Vulkan.Core10                 as ShaderModuleCreateInfo (ShaderModuleCreateInfo(..))
import Vulkan.Core10                 as MemoryHeap (MemoryHeap(size)) 
import Vulkan.Dynamic                (DeviceCmds(..), InstanceCmds(..))
import Vulkan.Extensions
import Vulkan.Utils.Debug            (debugCallbackPtr)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)
import Vulkan.Zero                   (Zero(..))
import VulkanMemoryAllocator         as VMA hiding (getPhysicalDeviceProperties)
import VulkanMemoryAllocator         as AllocationCreateInfo (AllocationCreateInfo(..))

----------------------------------------------------------------
-- The program
----------------------------------------------------------------

main :: IO ()
main = runResourceT $ do
  -- Create Instance, PhysicalDevice, Device and Allocator
  inst <- initializeInstance

  (_, devs) <- enumeratePhysicalDevices inst
  (pdi, phys) <-
    fmap (maximumBy (comparing fst) . catMaybes)
    $ mapM (\dev -> (fmap (, dev) <$> physicalDeviceInfo dev)) (toList devs)

  sayErr . ("Using device: " <>) . decodeUtf8 . deviceName =<< getPhysicalDeviceProperties phys
  (_, device) <- withDevice phys (deviceQueueCreateInfo pdi) Nothing allocate
  (_, allocator)   <- withAllocator (allocatorCreateInfo device phys inst) allocate

  -- Run our application
  -- Wait for the device to become idle before tearing down any resourecs.
  finally (do
      -- Render the Julia set
      image <- calculateImage device allocator pdi
      sayErr "Writing file"
      liftIO $ BSL.writeFile "julia.png" (JP.encodePng image)
    )
    (deviceWaitIdle device)

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

initializeInstance :: ResourceT IO Instance
initializeInstance = do
  availableLayers <- map layerName . toList . snd <$> enumerateInstanceLayerProperties

  let (layers, missingLayers) = partition (`elem` availableLayers) ["VK_LAYER_KHRONOS_validation"]
  sayErr $ "Missing optional layer: " <> (T.pack . show) missingLayers

  availableExtensions <- map extensionName . toList . snd <$> enumerateInstanceExtensionProperties Nothing

  let (optExtsHave, optMissing) = partition (`elem` availableExtensions) [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
  sayErr $ "Missing optional extension: " <> (T.pack . show) optMissing

  let (reqExtsHave, reqMissing) = partition (`elem` availableExtensions) [EXT_DEBUG_UTILS_EXTENSION_NAME]
  sayErr $ "Missing required extension: " <> (T.pack . show) reqMissing

  (_, inst) <- withInstance (instanceCreateInfo layers (reqExtsHave <> optExtsHave)) Nothing allocate
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

calculateImage :: Device -> Allocator -> PhysicalDeviceInfo -> ResourceT IO (JP.Image PixelRGBA8)
calculateImage device allocator pdi = do
  -- Some things to reuse, make sure these are the same as the values in the
  -- compute shader. TODO: reduce this duplication.
  let width, height, workgroupX, workgroupY :: Int
      width      = 512
      height     = width
      workgroupX = 32
      workgroupY = 4

  -- Create a buffer into which to render
  (_, (buffer, bufferAllocation, bufferAllocationInfo)) <-
    withBuffer allocator (bufferCreateInfo width height) allocationCreateInfo allocate

  -- Create a descriptor set and layout for this buffer
  (descriptorSet, descriptorSetLayout) <- do
    (_, descriptorPool) <- withDescriptorPool device descriptorPoolCreateInfo Nothing allocate
    (_, descriptorSetLayout) <- withDescriptorSetLayout device descriptorSetLayoutCreateInfo Nothing allocate

    -- Allocate a descriptor set from the pool with that layout
    -- Don't use `withDescriptorSets` here as the set will be cleaned up when
    -- the pool is destroyed.
    [descriptorSet] <- allocateDescriptorSets device (descriptorSetAllocateInfo descriptorPool descriptorSetLayout)
    pure (descriptorSet, descriptorSetLayout)

  -- Assign the buffer in this descriptor set
  updateDescriptorSets device [descriptor descriptorSet buffer] []

  -- Create our shader and compute pipeline
  (_, shaderModule) <- withShaderModule device zero{ShaderModuleCreateInfo.code = shaderCode} Nothing allocate

  (_, pipelineLayout) <- withPipelineLayout device (pipelineLayoutCreateInfo descriptorSetLayout) Nothing allocate
  (_, (_, [computePipeline])) <- withComputePipelines device zero [pipelineCreateInfo pipelineLayout shaderModule] Nothing allocate

  -- Create a command buffer
  (_, commandPool) <- withCommandPool device (commandPoolCreateInfo pdi) Nothing allocate

  (_, [commandBuffer]) <- withCommandBuffers device (commandBufferAllocateInfo commandPool) allocate

  -- Fill command buffer
  useCommandBuffer commandBuffer сommandBufferBeginInfo $
    do
    cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_COMPUTE computePipeline
    cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_COMPUTE pipelineLayout 0 [descriptorSet] []
    cmdDispatch
      commandBuffer
      (ceiling (realToFrac width / realToFrac @_ @Float workgroupX))
      (ceiling (realToFrac height / realToFrac @_ @Float workgroupY))
      1

  -- Create a fence so we can know when render is finished
  (_, fence) <- withFence device zero Nothing allocate

  -- Submit the command buffer and wait for it to execute
  computeQueue <- getDeviceQueue device (pdiComputeQueueFamilyIndex pdi) 0
  queueSubmit computeQueue [submitInfo commandBuffer] fence

  let fenceTimeout = 1e9 -- 1 second
  waitForFences device [fence] True fenceTimeout >>= \case
    TIMEOUT -> throwString "Timed out waiting for compute"
    _       -> pure ()

  -- If the buffer allocation is not HOST_COHERENT this will ensure the changes
  -- are present on the CPU.
  invalidateAllocation allocator bufferAllocation 0 WHOLE_SIZE

  -- TODO: speed this bit up, it's hopelessly slow
  let pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
      pixelAddr x y = plusPtr (mappedData bufferAllocationInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  liftIO $ JP.withImage width height
    (\x y -> do
      let ptr = pixelAddr x y
      [r, g, b, a] <- fmap (\f -> round (f * 255))
        <$> peekArray 4 (lowerArrayPtr ptr)
      pure $ JP.PixelRGBA8 r g b a
    )


shaderCode :: ByteString
shaderCode = [comp|
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

deviceQueueCreateInfo :: PhysicalDeviceInfo -> DeviceCreateInfo '[]
deviceQueueCreateInfo pdi = 
  let queueCreateInfo = SomeStruct zero{queueFamilyIndex = pdiComputeQueueFamilyIndex pdi, queuePriorities  = [1]}
  in zero{queueCreateInfos = [queueCreateInfo]}

allocatorCreateInfo :: Device -> PhysicalDevice -> Instance -> AllocatorCreateInfo
allocatorCreateInfo dev@(Device _ DeviceCmds{..}) phys inst@(Instance _ InstanceCmds{..}) = zero
  { flags            = zero
  , physicalDevice   = physicalDeviceHandle phys
  , device           = deviceHandle dev
  , instance'        = instanceHandle inst
  , vulkanApiVersion = myApiVersion
  , vulkanFunctions  = Just zero
    { vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
    , vkGetDeviceProcAddr   = castFunPtr pVkGetDeviceProcAddr
    }
  }

-- Use ALLOCATION_CREATE_MAPPED_BIT and MEMORY_USAGE_GPU_TO_CPU to make sure
-- it's readable on the host and starts in the mapped state
bufferCreateInfo :: Int -> Int -> BufferCreateInfo '[]
bufferCreateInfo width height = zero
  { size  = fromIntegral $ width * height * 4 * sizeOf (0 :: Float)
  , usage = BUFFER_USAGE_STORAGE_BUFFER_BIT
  }

allocationCreateInfo :: AllocationCreateInfo
allocationCreateInfo = zero
  { AllocationCreateInfo.flags = ALLOCATION_CREATE_MAPPED_BIT
  , usage = MEMORY_USAGE_GPU_TO_CPU
  }

descriptorPoolCreateInfo :: DescriptorPoolCreateInfo '[]
descriptorPoolCreateInfo = zero
  { maxSets   = 1
  , poolSizes = [DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
  }

descriptorSetLayoutCreateInfo :: DescriptorSetLayoutCreateInfo '[]
descriptorSetLayoutCreateInfo = zero
  { bindings = [
    zero
      { binding         = 0
      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
      , descriptorCount = 1
      , stageFlags      = SHADER_STAGE_COMPUTE_BIT
      }
    ]
  }

descriptorSetAllocateInfo :: DescriptorPool -> DescriptorSetLayout -> DescriptorSetAllocateInfo '[]
descriptorSetAllocateInfo descriptorPool descriptorSetLayout = zero
  { descriptorPool = descriptorPool
  , setLayouts     = [descriptorSetLayout]
  }

descriptor :: DescriptorSet -> Buffer -> SomeStruct WriteDescriptorSet
descriptor descriptorSet buffer = SomeStruct zero
  { dstSet          = descriptorSet
  , dstBinding      = 0
  , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
  , descriptorCount = 1
  , bufferInfo      = [DescriptorBufferInfo buffer 0 WHOLE_SIZE]
  }

pipelineLayoutCreateInfo :: DescriptorSetLayout -> PipelineLayoutCreateInfo
pipelineLayoutCreateInfo descriptorSetLayout = zero{PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout]}

pipelineCreateInfo :: PipelineLayout -> ShaderModule -> SomeStruct ComputePipelineCreateInfo
pipelineCreateInfo pipelineLayout shaderModule = SomeStruct zero
  { layout             = pipelineLayout
  , stage              = compShaderStageCreateInfo shaderModule
  , basePipelineHandle = zero
  }

compShaderStageCreateInfo :: ShaderModule -> SomeStruct PipelineShaderStageCreateInfo
compShaderStageCreateInfo compModule = SomeStruct zero
  { stage   = SHADER_STAGE_COMPUTE_BIT
  , module' = compModule
  , name    = "main"
  }

commandPoolCreateInfo :: PhysicalDeviceInfo -> CommandPoolCreateInfo
commandPoolCreateInfo pdi = zero{CommandPoolCreateInfo.queueFamilyIndex = pdiComputeQueueFamilyIndex pdi}

commandBufferAllocateInfo :: CommandPool -> CommandBufferAllocateInfo
commandBufferAllocateInfo commandPool = zero{commandPool = commandPool, level = COMMAND_BUFFER_LEVEL_PRIMARY, commandBufferCount = 1}

debugMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }

instanceCreateInfo :: [StrictByteString] -> [StrictByteString] -> InstanceCreateInfo      [DebugUtilsMessengerCreateInfoEXT, ValidationFeaturesEXT]
instanceCreateInfo layers extensions = zero
  { applicationInfo = Just zero{applicationName = Nothing, apiVersion = myApiVersion}
  , enabledLayerNames     = Vec.fromList layers
  , enabledExtensionNames = Vec.fromList extensions
  }
  ::& debugMessengerCreateInfo
  :&  ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
  :&  ()

сommandBufferBeginInfo :: CommandBufferBeginInfo '[]
сommandBufferBeginInfo = zero{CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}

submitInfo :: CommandBuffer -> SomeStruct SubmitInfo
submitInfo commandBuffer = SomeStruct zero{commandBuffers = [commandBufferHandle commandBuffer]}

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory             :: Word64
  , pdiComputeQueueFamilyIndex :: Word32
    -- ^ The queue family index of the first compute queue
  }
  deriving (Eq, Ord)

physicalDeviceInfo :: MonadIO m => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  pdiTotalMemory <- sum . fmap MemoryHeap.size . memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
  pdiComputeQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isComputeQueue q = (QUEUE_COMPUTE_BIT .&. queueFlags q /= zeroBits) && (queueCount q > 0)
        computeQueueIndices = fromIntegral . fst <$> Vec.filter
          (isComputeQueue . snd)
          (Vec.indexed queueFamilyProperties)
    MaybeT (pure $ computeQueueIndices Vec.!? 0)
  pure PhysicalDeviceInfo { .. }
