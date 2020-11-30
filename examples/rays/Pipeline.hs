{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Pipeline
  ( createPipeline
  , createRTPipelineLayout
  , createRTDescriptorSetLayout
  , createRTDescriptorSets
  , createShaderBindingTable
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           Foreign                        ( nullPtr )
import           Foreign.Marshal.Utils          ( moveBytes )
import           Foreign.Ptr                    ( Ptr
                                                , plusPtr
                                                )
import           MonadVulkan
import           Say
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
import           Vulkan.Utils.ShaderQQ
import           Vulkan.Zero
import           VulkanMemoryAllocator
import Scene

-- Create the most vanilla ray tracing pipeline, returns the number of shader
-- groups
createPipeline :: PipelineLayout -> V (ReleaseKey, Pipeline, Word32)
createPipeline pipelineLayout = do
  (shaderKeys, shaderStages) <- V.unzip <$> sequence
    [ createRayGenerationShader
    , createRayIntShader
    , createRayMissShader
    , createRayHitShader
    ]

  let genGroup = RayTracingShaderGroupCreateInfoKHR
        RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
        0 -- The index of our general shader
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        nullPtr
      intGroup = RayTracingShaderGroupCreateInfoKHR
        RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
        SHADER_UNUSED_KHR
        3 -- closest hit
        SHADER_UNUSED_KHR
        1 -- intersection
        nullPtr
      missGroup = RayTracingShaderGroupCreateInfoKHR
        RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
        2
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        nullPtr
      shaderGroups = [genGroup, intGroup, missGroup]

  let pipelineCreateInfo :: RayTracingPipelineCreateInfoKHR '[]
      pipelineCreateInfo = zero { flags                        = zero
                                , stages                       = shaderStages
                                , groups                       = shaderGroups
                                , maxPipelineRayRecursionDepth = 1
                                , layout                       = pipelineLayout
                                }
  (key, (_, ~[rtPipeline])) <- withRayTracingPipelinesKHR'
    zero
    [SomeStruct pipelineCreateInfo]

  traverse_ release shaderKeys

  pure (key, rtPipeline, fromIntegral (V.length shaderGroups))

createRTPipelineLayout :: DescriptorSetLayout -> V (ReleaseKey, PipelineLayout)
createRTPipelineLayout descriptorSetLayout =
  withPipelineLayout' zero { setLayouts = [descriptorSetLayout] }

createRTDescriptorSetLayout :: V (ReleaseKey, DescriptorSetLayout)
createRTDescriptorSetLayout = withDescriptorSetLayout' zero
  { bindings = [ zero
                 { binding         = 0
                 , descriptorType  = DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                 , descriptorCount = 1
                 , stageFlags      = SHADER_STAGE_RAYGEN_BIT_KHR
                 }
               , zero { binding         = 1
                      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
                      , descriptorCount = 1
                      , stageFlags      = SHADER_STAGE_RAYGEN_BIT_KHR
                      }
               , zero
                 { binding         = 2
                 , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
                 , descriptorCount = 1
                 , stageFlags      = SHADER_STAGE_INTERSECTION_BIT_KHR
                                       .|. SHADER_STAGE_CLOSEST_HIT_BIT_KHR
                 }
               , zero { binding         = 3
                      , descriptorType  = DESCRIPTOR_TYPE_UNIFORM_BUFFER
                      , descriptorCount = 1
                      , stageFlags      = SHADER_STAGE_RAYGEN_BIT_KHR
                      }
               ]
  }

createRTDescriptorSets
  :: DescriptorSetLayout
  -> AccelerationStructureKHR
  -> SceneBuffers
  -> Word32
  -> V (Vector DescriptorSet)
createRTDescriptorSets descriptorSetLayout tlas SceneBuffers {..} numDescriptorSets
  = do
    let numImagesPerSet                 = 1
        numAccelerationStructuresPerSet = 1
        numStorageBuffersPerSet         = 1
        numUniformBuffersPerSet         = 1
    -- Create a descriptor pool
    (_, descriptorPool) <- withDescriptorPool' zero
      { maxSets   = numDescriptorSets
      , poolSizes =
        [ DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_IMAGE
                             (numDescriptorSets * numImagesPerSet)
        , DescriptorPoolSize
          DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
          (numDescriptorSets * numAccelerationStructuresPerSet)
        , DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_BUFFER
                             (numDescriptorSets * numStorageBuffersPerSet)
        , DescriptorPoolSize DESCRIPTOR_TYPE_UNIFORM_BUFFER
                             (numDescriptorSets * numUniformBuffersPerSet)
        ]
      }

    -- Allocate a descriptor set from the pool with that layout
    -- Don't use `withDescriptorSets` here as the set will be cleaned up when
    -- the pool is destroyed.
    sets <- allocateDescriptorSets' zero
      { descriptorPool = descriptorPool
      , setLayouts     = V.replicate (fromIntegral numDescriptorSets)
                                     descriptorSetLayout
      }

    -- Put the static accelerationStructure into the set
    for_ sets $ \set -> updateDescriptorSets'
      [ SomeStruct
      $   zero { dstSet          = set
               , dstBinding      = 0
               , descriptorType  = DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
               , descriptorCount = 1
               }
      ::& zero { accelerationStructures = [tlas] }
      :&  ()
      , SomeStruct $ zero
        { dstSet          = set
        , dstBinding      = 2
        , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
        , descriptorCount = 1
        , bufferInfo      = [ DescriptorBufferInfo { buffer = sceneSpheres
                                                   , offset = 0
                                                   , range  = WHOLE_SIZE
                                                   }
                            ]
        }
      ]
      []

    pure sets

createRayGenerationShader
  :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
createRayGenerationShader = do
  let code = $(compileShaderQ (Just "spirv1.4") "rgen" [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        layout(binding = 0, set = 0) uniform accelerationStructureEXT topLevelAS;
        layout(binding = 1, set = 0, rgba8) uniform writeonly image2D image;
        layout(location = 0) rayPayloadEXT vec3 prd;


        layout(set = 0, binding = 3) uniform CameraBuffer
        {
          mat4x4 viewInverse;
          mat4x4 projInverse;
        };

        void main()
        {
            const vec2 pixelCenter = vec2(gl_LaunchIDEXT.xy) + vec2(0.5);
            const vec2 inUV = pixelCenter/vec2(gl_LaunchSizeEXT.xy);
            const vec2 scr = inUV * 2.0 - 1.0;
            const vec3 origin    = (viewInverse * vec4(0,0,0,1)).xyz;
            const vec3 direction =
              normalize((viewInverse * vec4((projInverse * vec4(scr,1,1)).xyz, 0)).xyz);
            const uint  rayFlags = gl_RayFlagsOpaqueEXT;
            const float tMin     = 0.001;
            const float tMax     = 10000.0;
            const uint cullMask = 0xff;
            const uint sbtRecordOffset = 0;
            const uint sbtRecordStride = 1;
            const uint missIndex = 0;
            traceRayEXT(topLevelAS,
                        rayFlags,
                        cullMask,
                        sbtRecordOffset,
                        sbtRecordStride,
                        missIndex,
                        origin.xyz,
                        tMin,
                        direction.xyz,
                        tMax,
                        0);
            imageStore(image, ivec2(gl_LaunchIDEXT.xy), vec4(prd, 1.0));
        }
      |])

  (key, module') <- withShaderModule' zero { code }
  let shaderStageCreateInfo =
        zero { stage = SHADER_STAGE_RAYGEN_BIT_KHR, module', name = "main" }
  pure (key, SomeStruct shaderStageCreateInfo)

createRayHitShader :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
createRayHitShader = do
  let code = $(compileShaderQ (Just "spirv1.4") "rchit" [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        layout(set = 0, location = 0) rayPayloadInEXT vec3 hitValue;

        struct Sphere {
          vec4 s;
          vec4 color;
        };

        layout(std430, set = 0, binding = 2) readonly buffer Spheres
        {
          Sphere spheres[];
        };

        void main()
        {
          const int i = gl_PrimitiveID;
          const Sphere sphere = spheres[i];
          hitValue = vec3(sphere.color.xyz);
        }
      |])

  (key, module') <- withShaderModule' zero { code }
  let shaderStageCreateInfo =
        zero { stage = SHADER_STAGE_CLOSEST_HIT_BIT_KHR, module', name = "main" }
  pure (key, SomeStruct shaderStageCreateInfo)

createRayIntShader :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
createRayIntShader = do
  let code = $(compileShaderQ (Just "spirv1.4") "rint" [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        struct Sphere {
          vec4 s;
          vec4 color;
        };

        layout(std430, set = 0, binding = 2) readonly buffer Spheres
        {
          Sphere spheres[];
        };

        void main()
        {
          const int i = gl_PrimitiveID;
          const vec3 o = gl_WorldRayOriginEXT;
          const vec3 d = gl_WorldRayDirectionEXT;
          const Sphere sphere = spheres[i];
          const vec3 s = sphere.s.xyz;
          const float r = sphere.s.w;

          const vec3 diff = o - s;

          const float x = (dot(d, diff) * dot(d, diff)) - (dot(diff, diff) - r*r);
          if (x < 0)
            return;

          const float m = -(dot(d, diff));
          reportIntersectionEXT(m - sqrt(x), 0);
          reportIntersectionEXT(m + sqrt(x), 0);
        }
      |])

  (key, module') <- withShaderModule' zero { code }
  let shaderStageCreateInfo =
        zero { stage = SHADER_STAGE_INTERSECTION_BIT_KHR, module', name = "main" }
  pure (key, SomeStruct shaderStageCreateInfo)

createRayMissShader :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
createRayMissShader = do
  let code = $(compileShaderQ (Just "spirv1.4") "rmiss" [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        layout(location = 0) rayPayloadInEXT vec3 hitValue;

        void main()
        {
            hitValue = vec3(0.1, 0.15, 0.15);
        }
      |])

  (key, module') <- withShaderModule' zero { code }
  let shaderStageCreateInfo =
        zero { stage = SHADER_STAGE_MISS_BIT_KHR, module', name = "main" }
  pure (key, SomeStruct shaderStageCreateInfo)
----------------------------------------------------------------
-- Shader binding table
----------------------------------------------------------------

createShaderBindingTable :: Pipeline -> Word32 -> V (ReleaseKey, Buffer)
createShaderBindingTable pipeline numGroups = do
  RTInfo {..} <- getRTInfo
  let handleSize    = rtiShaderGroupHandleSize
      baseAlignment = rtiShaderGroupBaseAlignment
      handleStride  = max handleSize baseAlignment
      -- Make the buffer big enough for all the groups, with spacing between
      -- them equal to their alignment
      sbtSize       = fromIntegral $ handleStride * (numGroups - 1) + handleSize

  sayErrShow (handleStride, rtiShaderGroupBaseAlignment)

  (bufferReleaseKey, (sbtBuffer, sbtAllocation, _sbtAllocationInfo)) <-
    withBuffer'
      zero { usage = BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, size = sbtSize }
      zero
        { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                            .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
  nameObject' sbtBuffer "SBT"

  (memKey, mem) <- withMappedMemory' sbtAllocation
  getRayTracingShaderGroupHandlesKHR' pipeline 0 numGroups sbtSize mem
  unpackObjects numGroups handleSize handleStride mem
  release memKey
  pure (bufferReleaseKey, sbtBuffer)

-- | Move densely packed objects so that they have a desired stride
unpackObjects
  :: MonadIO m
  => Word32
  -- ^ Num objects
  -> Word32
  -- ^ Object size, the initial stride
  -> Word32
  -- ^ Desired stride
  -> Ptr ()
  -- ^ Initial, packed data, in a buffer big enough for the unpacked data
  -> m ()
unpackObjects numObjs size desiredStride buf = do
  let
    objectInitalPosition n = buf `plusPtr` fromIntegral (size * n)
    objectFinalPosition n = buf `plusPtr` fromIntegral (desiredStride * n)
    moveObject n = moveBytes (objectFinalPosition n)
                             (objectInitalPosition n)
                             (fromIntegral size)
    -- Move the object last to first
    indicesToMove = drop 1 [numObjs, numObjs - 1 .. 1]
  liftIO $ traverse_ moveObject indicesToMove
