{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| A compute example whose shader interface is derived from the compiled
SPIR-V at build time by @vulkan-utils-spirv@:

  * 'reflectShaderTypes' generates the 'Params' push-constant record (with a
    gl-block std430 'Storable') from @shader.comp.spv@;
  * 'singleDescriptorSetLayoutInfo' builds the descriptor set layout for the
    output SSBO from the same module at runtime;
  * 'pushConstantRanges' builds the pipeline layout's push-constant range; and
  * 'allocateSpecializationInfo' builds the pipeline's specialization info
    (@maxIterations@, @escapeRadius@) from the reflected constant ids.

Compare with the @compute@ example, which hand-writes all of this.
-}
module Main
  ( main
  )
where

import qualified Codec.Picture as JP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (sizeOf)
import qualified Geomancy
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec2 (vec2)
import Graphics.Gl.Block (Std430 (..))
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (captureImageRGBA8, savePng)
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Specialization (allocateSpecializationInfo)
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

-- | The compiled shader, embedded for runtime module creation.
compCode :: ByteString
compCode = $(embedFile "compute-reflect/shader.comp.spv")

-- Generate the @Params@ push-constant record (and its std430 'Storable') from
-- the same SPIR-V the runtime loads.
reflectShaderTypes "compute-reflect/shader.comp.spv"

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan compute-reflect example"
        , instanceReqs = []
        , deviceReqs = []
        , vmaFlags = zero
        }
  let QueueFamilyIndex computeQueueFamilyIndex = fst (qCompute queues)

  image <- render allocator device computeQueueFamilyIndex
  Vk.deviceWaitIdle device
  savePng "julia-reflect.png" image

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev computeQueueFamilyIndex = do
  let
    width, height, workgroup :: Int
    width = 512
    height = width
    workgroup = 16

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

  -- Reflect the embedded module once; reuse it for the descriptor set layout
  -- and the push-constant range.
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

  -- Descriptor set layout generated from the reflected module (the output SSBO).
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate

  (_, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1
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
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
    ]
    []

  -- Pipeline. The specialization info (maxIterations, escapeRadius) is built
  -- from the reflected constant ids and lives for this resource scope.
  mSpec <- allocateSpecializationInfo reflected (maxIterations, escapeRadius)
  (_, shader) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT mSpec compCode
  -- Pipeline layout: descriptor set + push-constant range, both from reflection.
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
    Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE computePipeline
    Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pipelineLayout 0 [descriptorSet] []
    -- Push the reflected 'Params' (std430) for this dispatch.
    liftIO $ with params $ \pParams ->
      Vk.cmdPushConstants
        cb
        pipelineLayout
        Vk.SHADER_STAGE_COMPUTE_BIT
        0
        (fromIntegral (sizeOf params))
        (castPtr pParams)
    Vk.cmdDispatch
      cb
      (ceiling (realToFrac width / realToFrac @_ @Float workgroup))
      (ceiling (realToFrac height / realToFrac @_ @Float workgroup))
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
