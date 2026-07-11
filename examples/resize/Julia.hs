{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Julia-set compute shader pipeline. The pipeline + layout are created once
and never re-created; the descriptor sets are bound to swapchain image views,
so they need to be recreated whenever the swapchain changes.

Everything is hand-written (no reflection) into a "Vulkan.Utils.Pipeline"
bundle — compare with the @compute-reflect@ example.
-}
module Julia
  ( allocateJuliaPipeline
  , allocateJuliaDescriptorSets
  , juliaWorkgroupX
  , juliaWorkgroupY
  ) where

import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (imageWrite)
import Vulkan.Utils.Pipeline (Pipeline (..))
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShaderQ, glsl)
import Vulkan.Zero (zero)

import Julia.Constants

allocateJuliaPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> m Pipeline
allocateJuliaPipeline dev = do
  set0 <-
    Pipeline.allocateSetLayout
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
  layout <-
    Pipeline.allocateLayout
      dev
      [(0, set0)]
      [Vk.PushConstantRange Vk.SHADER_STAGE_COMPUTE_BIT 0 ((2 + 2 + 2 + 1 + 1) * 4)]
  (releaseShader, shader) <- juliaShader dev
  let
    pipelineCreateInfo :: Vk.ComputePipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.layout = layout.pipelineLayout
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
  pure Pipeline{pipeline = computePipeline, bindPoint = Vk.PIPELINE_BIND_POINT_COMPUTE, layout}

{- | One descriptor set per supplied image view, bound to that view.
Allocated from a fresh descriptor pool so that releasing the key frees the lot.
-}
allocateJuliaDescriptorSets
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Pipeline
  -> Vector Vk.ImageView
  -> m (ReleaseKey, Vector Vk.DescriptorSet)
allocateJuliaDescriptorSets dev jp imageViews = do
  set0 <- Pipeline.set jp.layout 0
  (poolKey, descriptorSets) <- Pipeline.allocateDescriptorSets dev set0 (V.length imageViews)
  Vk.updateDescriptorSets
    dev
    (V.zipWith (\set view -> imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL view) descriptorSets imageViews)
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
          float time;
        } frame;

        // From https://iquilezles.org/www/articles/palettes/palettes.htm
        //
        // Traditional Julia blue and orange, with the whole palette rotated by
        // frame.time (advanced once per compute so the colours move exactly when
        // — and only when — the image is recomputed).
        vec3 color(const float t) {
          const vec3 a = vec3(0.5);
          const vec3 b = vec3(0.5);
          const vec3 c = vec3(8);
          const vec3 d = vec3(0.5, 0.6, 0.7);
          return a + b * cos(6.28318530718 * (c * t + d + frame.time));
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
