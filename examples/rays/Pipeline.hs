-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pipeline
  ( createPipeline
  , createRTPipelineLayout
  , createRTDescriptorSetLayout
  , createRTDescriptorSets
  , createShaderBindingTable
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Foldable (for_, traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Foreign (nullPtr)
import Foreign.Marshal.Utils (moveBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Init (RTInfo (..))
import Say (sayErrShow)
import Scene (SceneBuffers (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_acceleration_structure as RT
import qualified Vulkan.Extensions.VK_KHR_ray_tracing_pipeline as RT
import Vulkan.Utils.Debug (nameObject)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

-- | Create the RT pipeline; returns the number of shader groups.
createPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.PipelineLayout
  -> m (ReleaseKey, Vk.Pipeline, Word32)
createPipeline dev pipelineLayout = do
  (shaderKeys, V.fromList -> shaderStages) <-
    unzip
      <$> sequence
        [ createRayGenerationShader dev
        , createRayIntShader dev
        , createRayMissShader dev
        , createRayHitShader dev
        ]

  let
    genGroup =
      RT.RayTracingShaderGroupCreateInfoKHR
        RT.RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
        0
        RT.SHADER_UNUSED_KHR
        RT.SHADER_UNUSED_KHR
        RT.SHADER_UNUSED_KHR
        nullPtr
    intGroup =
      RT.RayTracingShaderGroupCreateInfoKHR
        RT.RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
        RT.SHADER_UNUSED_KHR
        3
        RT.SHADER_UNUSED_KHR
        1
        nullPtr
    missGroup =
      RT.RayTracingShaderGroupCreateInfoKHR
        RT.RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
        2
        RT.SHADER_UNUSED_KHR
        RT.SHADER_UNUSED_KHR
        RT.SHADER_UNUSED_KHR
        nullPtr
    shaderGroups = V.fromList [genGroup, intGroup, missGroup]

  let
    pipelineCreateInfo :: RT.RayTracingPipelineCreateInfoKHR '[]
    pipelineCreateInfo =
      zero
        { RT.flags = zero
        , RT.stages = shaderStages
        , RT.groups = shaderGroups
        , RT.maxPipelineRayRecursionDepth = 1
        , RT.layout = pipelineLayout
        }
  (key, (_, V.toList -> [rtPipeline])) <-
    RT.withRayTracingPipelinesKHR
      dev
      Vk.NULL_HANDLE
      Vk.NULL_HANDLE
      (V.singleton $ SomeStruct pipelineCreateInfo)
      Nothing
      allocate

  traverse_ release shaderKeys

  pure (key, rtPipeline, fromIntegral (V.length shaderGroups))

createRTPipelineLayout
  :: (MonadResource m)
  => Vk.Device -> Vk.DescriptorSetLayout -> m (ReleaseKey, Vk.PipelineLayout)
createRTPipelineLayout dev descriptorSetLayout =
  Vk.withPipelineLayout
    dev
    zero{Vk.setLayouts = V.singleton descriptorSetLayout}
    Nothing
    allocate

createRTDescriptorSetLayout
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, Vk.DescriptorSetLayout)
createRTDescriptorSetLayout dev =
  Vk.withDescriptorSetLayout
    dev
    zero
      { Vk.bindings =
          V.fromList
            [ zero
                { Vk.binding = 0
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                , Vk.descriptorCount = 1
                , Vk.stageFlags = Vk.SHADER_STAGE_RAYGEN_BIT_KHR
                }
            , zero
                { Vk.binding = 1
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
                , Vk.descriptorCount = 1
                , Vk.stageFlags = Vk.SHADER_STAGE_RAYGEN_BIT_KHR
                }
            , zero
                { Vk.binding = 2
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
                , Vk.descriptorCount = 1
                , Vk.stageFlags =
                    Vk.SHADER_STAGE_INTERSECTION_BIT_KHR
                      .|. Vk.SHADER_STAGE_CLOSEST_HIT_BIT_KHR
                }
            , zero
                { Vk.binding = 3
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                , Vk.descriptorCount = 1
                , Vk.stageFlags = Vk.SHADER_STAGE_RAYGEN_BIT_KHR
                }
            ]
      }
    Nothing
    allocate

createRTDescriptorSets
  :: (MonadResource m)
  => Vk.Device
  -> Vk.DescriptorSetLayout
  -> RT.AccelerationStructureKHR
  -> SceneBuffers
  -> Word32
  -> m (Vector Vk.DescriptorSet)
createRTDescriptorSets dev descriptorSetLayout tlas SceneBuffers{..} numDescriptorSets = do
  let
    numImagesPerSet = 1
    numAccelerationStructuresPerSet = 1
    numStorageBuffersPerSet = 1
    numUniformBuffersPerSet = 1
  (_, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = numDescriptorSets
        , Vk.poolSizes =
            V.fromList
              [ Vk.DescriptorPoolSize
                  Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
                  (numDescriptorSets * numImagesPerSet)
              , Vk.DescriptorPoolSize
                  Vk.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                  (numDescriptorSets * numAccelerationStructuresPerSet)
              , Vk.DescriptorPoolSize
                  Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
                  (numDescriptorSets * numStorageBuffersPerSet)
              , Vk.DescriptorPoolSize
                  Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                  (numDescriptorSets * numUniformBuffersPerSet)
              ]
        }
      Nothing
      allocate

  sets <-
    Vk.allocateDescriptorSets
      dev
      zero
        { Vk.descriptorPool = descriptorPool
        , Vk.setLayouts =
            V.replicate
              (fromIntegral numDescriptorSets)
              descriptorSetLayout
        }
  let
    updates :: Vk.DescriptorSet -> Vector (SomeStruct Vk.WriteDescriptorSet)
    updates set =
      V.fromList
        [ SomeStruct $!
            zero
              { Vk.dstSet = set
              , Vk.dstBinding = 0
              , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
              , Vk.descriptorCount = 1
              }
              ::& zero{RT.accelerationStructures = V.singleton tlas}
                :& ()
        , SomeStruct
            zero
              { Vk.dstSet = set
              , Vk.dstBinding = 2
              , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
              , Vk.descriptorCount = 1
              , Vk.bufferInfo =
                  V.singleton
                    Vk.DescriptorBufferInfo
                      { Vk.buffer = sceneSpheres
                      , Vk.offset = 0
                      , Vk.range = Vk.WHOLE_SIZE
                      }
              }
        ]

  for_ sets $ \set ->
    Vk.updateDescriptorSets dev (updates set) mempty

  pure sets

createRayGenerationShader
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
createRayGenerationShader dev = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code} Nothing allocate
  let shaderStageCreateInfo = zero{Vk.stage = Vk.SHADER_STAGE_RAYGEN_BIT_KHR, Vk.module', Vk.name = "main"}
  pure (key, SomeStruct shaderStageCreateInfo)
  where
    code =
      $( compileShaderQ
           (Just "spirv1.4")
           "rgen"
           Nothing
           [glsl|
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
            const float tMax     = 10e10;
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
      |]
       )

createRayHitShader
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
createRayHitShader dev = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code} Nothing allocate
  let shaderStageCreateInfo = zero{Vk.stage = Vk.SHADER_STAGE_CLOSEST_HIT_BIT_KHR, Vk.module', Vk.name = "main"}
  pure (key, SomeStruct shaderStageCreateInfo)
  where
    code =
      $( compileShaderQ
           (Just "spirv1.4")
           "rchit"
           Nothing
           [glsl|
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
      |]
       )

createRayIntShader
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
createRayIntShader dev = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code} Nothing allocate
  let shaderStageCreateInfo = zero{Vk.stage = Vk.SHADER_STAGE_INTERSECTION_BIT_KHR, Vk.module', Vk.name = "main"}
  pure (key, SomeStruct shaderStageCreateInfo)
  where
    code =
      $( compileShaderQ
           (Just "spirv1.4")
           "rint"
           Nothing
           [glsl|
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
      |]
       )

createRayMissShader
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
createRayMissShader dev = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code} Nothing allocate
  let shaderStageCreateInfo = zero{Vk.stage = Vk.SHADER_STAGE_MISS_BIT_KHR, Vk.module', Vk.name = "main"}
  pure (key, SomeStruct shaderStageCreateInfo)
  where
    code =
      $( compileShaderQ
           (Just "spirv1.4")
           "rmiss"
           Nothing
           [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        layout(location = 0) rayPayloadInEXT vec3 hitValue;

        void main()
        {
            hitValue = vec3(0.1, 0.15, 0.15);
        }
      |]
       )

----------------------------------------------------------------
-- Shader binding table
----------------------------------------------------------------

createShaderBindingTable
  :: (MonadResource m)
  => Vk.Device
  -> VMA.Allocator
  -> RTInfo
  -> Vk.Pipeline
  -> Word32
  -> m (ReleaseKey, Vk.Buffer)
createShaderBindingTable dev vma RTInfo{..} pipeline numGroups = do
  let
    handleSize = rtiShaderGroupHandleSize
    baseAlignment = rtiShaderGroupBaseAlignment
    handleStride = max handleSize baseAlignment
    sbtSize = fromIntegral $ handleStride * (numGroups - 1) + handleSize

  sayErrShow (handleStride, baseAlignment)

  (bufferReleaseKey, (sbtBuffer, sbtAllocation, _)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.usage = Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        , Vk.size = sbtSize
        }
      zero
        { VMA.requiredFlags =
            Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
              .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
      allocate
  nameObject dev sbtBuffer "SBT"

  (memKey, mem) <- VMA.withMappedMemory vma sbtAllocation allocate
  RT.getRayTracingShaderGroupHandlesKHR dev pipeline 0 numGroups sbtSize mem
  unpackObjects numGroups handleSize handleStride mem
  release memKey
  pure (bufferReleaseKey, sbtBuffer)

unpackObjects
  :: (MonadIO m)
  => Word32
  -> Word32
  -> Word32
  -> Ptr ()
  -> m ()
unpackObjects numObjs size desiredStride buf = do
  let
    objectInitalPosition n = buf `plusPtr` fromIntegral (size * n)
    objectFinalPosition n = buf `plusPtr` fromIntegral (desiredStride * n)
    moveObject n =
      moveBytes
        (objectFinalPosition n)
        (objectInitalPosition n)
        (fromIntegral size)
    indicesToMove = drop 1 [numObjs, numObjs - 1 .. 1]
  liftIO $ traverse_ moveObject indicesToMove
