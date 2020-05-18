{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}

module Julia
  ( juliaPipeline
  , juliaWorkgroupX
  , juliaWorkgroupY
  )
where

import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word

import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Utils.ShaderQQ
import           Vulkan.Zero

import           MonadVulkan

juliaPipeline
  :: Vector ImageView -> V (Pipeline, PipelineLayout, Vector DescriptorSet)
juliaPipeline imageViews = do
  descriptorSetLayout             <- juliaDescriptorSetLayout
  descriptorSets <- juliaDescriptorSet descriptorSetLayout imageViews
  (releaseShader, shader        ) <- juliaShader
  (_            , pipelineLayout) <- withPipelineLayout' zero
    { setLayouts         = [descriptorSetLayout]
    , pushConstantRanges = [ PushConstantRange SHADER_STAGE_COMPUTE_BIT
                                               0
                                               ((2 + 2 + 2 + 1) * 4)
                           ]
    }
  let pipelineCreateInfo :: ComputePipelineCreateInfo '[]
      pipelineCreateInfo = zero { layout             = pipelineLayout
                                , stage              = shader
                                , basePipelineHandle = zero
                                }
  (_, (_, [computePipeline])) <- withComputePipelines'
    zero
    [SomeStruct pipelineCreateInfo]
  release releaseShader
  pure (computePipeline, pipelineLayout, descriptorSets)

juliaDescriptorSetLayout :: V DescriptorSetLayout
juliaDescriptorSetLayout = snd <$> withDescriptorSetLayout' zero
  { bindings = [ zero { binding         = 0
                      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
                      , descriptorCount = 1
                      , stageFlags      = SHADER_STAGE_COMPUTE_BIT
                      }
               ]
  }

juliaDescriptorSet
  :: DescriptorSetLayout -> Vector ImageView -> V (Vector DescriptorSet)
juliaDescriptorSet descriptorSetLayout imageViews = do
  -- Create a descriptor pool
  (_, descriptorPool) <- withDescriptorPool' zero
    { maxSets   = fromIntegral (V.length imageViews)
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_IMAGE
                                       (fromIntegral (V.length imageViews))
                  ]
    }

  -- Allocate a descriptor set from the pool with that layout
  -- Don't use `withDescriptorSets` here as the set will be cleaned up when
  -- the pool is destroyed.
  descriptorSets <- allocateDescriptorSets' zero
    { descriptorPool = descriptorPool
    , setLayouts     = V.replicate (V.length imageViews) descriptorSetLayout
    }

  -- Assign the buffer in this descriptor set
  updateDescriptorSets'
    (V.zipWith
      (\set view -> SomeStruct zero
        { dstSet          = set
        , dstBinding      = 0
        , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
        , descriptorCount = 1
        , imageInfo = [ DescriptorImageInfo { sampler     = NULL_HANDLE
                                            , imageView   = view
                                            , imageLayout = IMAGE_LAYOUT_GENERAL
                                            }
                      ]
        }
      )
      descriptorSets
      imageViews
    )
    []

  pure descriptorSets

-- Keep these in sync with the shader code
juliaWorkgroupX, juliaWorkgroupY :: Word32
juliaWorkgroupX = 8
juliaWorkgroupY = 8

juliaShader :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
juliaShader = do
  let compCode = [comp|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        const int workgroup_x = 8;
        const int workgroup_y = workgroup_x;

        layout (local_size_x = workgroup_x, local_size_y = workgroup_y, local_size_z = 1 ) in;
        layout(set = 0, binding = 0, rgba8) uniform writeonly image2D img;
        layout(push_constant) uniform Frame {
          vec2 scale;
          vec2 offset;
          vec2 c;
          float r;
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

        vec4 julia (vec2 z) {
          uint iteration = 0;
          const int max_iteration = 1000;

          while (dot(z,z) < dot(frame.r,frame.r) && iteration < max_iteration) {
            z = f(z);
            iteration++;
          }

          vec4 res;
          if (iteration == max_iteration) {
            res = vec4(0,0,0,1);
          } else {
            res = vec4(color(pow(float(iteration) / float(max_iteration), 1)),1);
          }
          return res;
        }

        // const int num_samples = 16;
        // const vec2 samples[num_samples] =
        //   { vec2(0.0, 0.0)
        //   , vec2(0.0, 0.25)
        //   , vec2(0.0, 0.5)
        //   , vec2(0.0, 0.75)
        //   , vec2(0.25, 0.0)
        //   , vec2(0.25, 0.25)
        //   , vec2(0.25, 0.5)
        //   , vec2(0.25, 0.75)
        //   , vec2(0.5, 0.0)
        //   , vec2(0.5, 0.25)
        //   , vec2(0.5, 0.5)
        //   , vec2(0.5, 0.75)
        //   , vec2(0.75, 0.0)
        //   , vec2(0.75, 0.25)
        //   , vec2(0.75, 0.5)
        //   , vec2(0.75, 0.75)
        //   };
        const int num_samples = 4;
        const vec2 samples[num_samples] =
          { vec2(0.0, 0.0)
          , vec2(0.0, 0.5)
          , vec2(0.5, 0.0)
          , vec2(0.5, 0.5)
          };

        // Algorithm from https://en.wikipedia.org/wiki/Julia_set
        void main() {
          vec4 res = vec4(0);
          for(int i = 0; i < num_samples; ++i) {
            const vec2 pix = vec2(gl_GlobalInvocationID) + samples[i];
            const vec2 z = vec2(pix) * frame.scale + frame.offset;
            res += julia(z);
          }
          res /= vec4(float(num_samples));
          imageStore(img, ivec2(gl_GlobalInvocationID.xy), res);
        }
      |]
  (releaseKey, compModule) <- withShaderModule' zero { code = compCode }
  let compShaderStageCreateInfo = zero { stage   = SHADER_STAGE_COMPUTE_BIT
                                       , module' = compModule
                                       , name    = "main"
                                       }
  pure (releaseKey, SomeStruct compShaderStageCreateInfo)
