{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Julia-set compute shader pipeline. The pipeline + descriptor set layout
are created once and never re-created; the descriptor sets are bound to
swapchain image views, so they need to be recreated whenever the swapchain
changes.
-}
module Julia
  ( JuliaPipeline (..)
  , createJuliaPipeline
  , createJuliaDescriptorSets
  , juliaWorkgroupX
  , juliaWorkgroupY
  ) where

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)
import Vulkan.Zero (zero)

import Julia.Constants

data JuliaPipeline = JuliaPipeline
  { jpPipeline :: Vk.Pipeline
  , jpPipelineLayout :: Vk.PipelineLayout
  , jpDescriptorSetLayout :: Vk.DescriptorSetLayout
  }

createJuliaPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> m JuliaPipeline
createJuliaPipeline dev = do
  (_, descriptorSetLayout) <-
    Vk.withDescriptorSetLayout
      dev
      zero
        { Vk.bindings =
            [ zero
                { Vk.binding = 0
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
                , Vk.descriptorCount = 1
                , Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
                }
            ]
        }
      Nothing
      allocate
  (releaseShader, shader) <- juliaShader dev
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = [descriptorSetLayout]
        , Vk.pushConstantRanges =
            [ Vk.PushConstantRange
                Vk.SHADER_STAGE_COMPUTE_BIT
                0
                ((2 + 2 + 2 + 1) * 4)
            ]
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
    Vk.withComputePipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
      allocate
  release releaseShader
  pure
    JuliaPipeline
      { jpPipeline = computePipeline
      , jpPipelineLayout = pipelineLayout
      , jpDescriptorSetLayout = descriptorSetLayout
      }

{- | One descriptor set per supplied image view, bound to that view.
Allocated from a fresh descriptor pool so that releasing this scope frees the lot.
-}
createJuliaDescriptorSets
  :: (MonadResource m)
  => Vk.Device
  -> Vk.DescriptorSetLayout
  -> Vector Vk.ImageView
  -> m (ReleaseKey, Vector Vk.DescriptorSet)
createJuliaDescriptorSets dev descriptorSetLayout imageViews = do
  (poolKey, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = fromIntegral (V.length imageViews)
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize
                Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
                (fromIntegral (V.length imageViews))
            ]
        }
      Nothing
      allocate

  -- Sets are freed automatically when the pool is destroyed.
  descriptorSets <-
    Vk.allocateDescriptorSets
      dev
      zero
        { Vk.descriptorPool = descriptorPool
        , Vk.setLayouts = V.replicate (V.length imageViews) descriptorSetLayout
        }

  Vk.updateDescriptorSets
    dev
    ( V.zipWith
        ( \set view ->
            SomeStruct
              zero
                { Vk.dstSet = set
                , Vk.dstBinding = 0
                , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
                , Vk.descriptorCount = 1
                , Vk.imageInfo =
                    [ Vk.DescriptorImageInfo
                        { Vk.sampler = Vk.NULL_HANDLE
                        , Vk.imageView = view
                        , Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL
                        }
                    ]
                }
        )
        descriptorSets
        imageViews
    )
    []

  pure (poolKey, descriptorSets)

juliaShader
  :: (MonadResource m)
  => Vk.Device
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
juliaShader dev = do
  (releaseKey, compModule) <- Vk.withShaderModule dev zero{Vk.code = compCode} Nothing allocate
  let compShaderStageCreateInfo =
        zero
          { Vk.stage = Vk.SHADER_STAGE_COMPUTE_BIT
          , Vk.module' = compModule
          , Vk.name = "main"
          }
  pure (releaseKey, SomeStruct compShaderStageCreateInfo)
  where
    compCode =
      $( compileShaderQ
           (Just "vulkan1.0")
           "comp"
           Nothing
           [glsl|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        const int workgroup_x = $juliaWorkgroupX;
        const int workgroup_y = $juliaWorkgroupY;

        layout (local_size_x = workgroup_x, local_size_y = workgroup_y, local_size_z = 1 ) in;
        layout(set = 0, binding = 0, rgba8) uniform writeonly image2D img;
        layout(push_constant) uniform Frame {
          vec2 scale;
          vec2 offset;
          vec2 c;
          float escapeRadius;
        } frame;

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
          return mulC(z,z) + frame.c;
        }

        float julia (vec2 z) {
          uint iteration = 0;
          const int max_iteration = 200;
          float smooth_ = exp(-length(z));

          while (dot(z,z) < frame.escapeRadius && iteration < max_iteration) {
            z = f(z);
            smooth_ += exp(-length(z));
            iteration++;
          }

          if (iteration == max_iteration)
            return 0;
          else
            return smooth_ / float(max_iteration);
        }

        const int num_samples = 4;
        const vec2 samples[num_samples] =
          { vec2(0.0, 0.0)
          , vec2(0.0, 0.5)
          , vec2(0.5, 0.0)
          , vec2(0.5, 0.5)
          };

        // Algorithm from https://en.wikipedia.org/wiki/Julia_set
        void main() {
          vec3 res = vec3(0);
          for(int i = 0; i < num_samples; ++i) {
            const vec2 pix = vec2(gl_GlobalInvocationID) + samples[i];
            const vec2 z = vec2(pix) * frame.scale + frame.offset;
            res += color(julia(z));
          }
          res /= float(num_samples);
          imageStore(img, ivec2(gl_GlobalInvocationID.xy), vec4(res, 1));
        }
      |]
       )
