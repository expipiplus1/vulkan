{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The path-tracing compute pipeline, its /entire/ interface derived from
'Shader.code' — exercising every reflected resource kind at once:

  * the @Camera@ uniform block        -> a generated record (std140);
  * the @Scene@ input storage buffer  -> a descriptor binding holding the leaf
    sphere geometry; its element record 'Sphere' is generated from the SSBO's
    array element type;
  * the @Image@ output storage buffer -> a descriptor binding;
  * the @BvhNode@ @buffer_reference@ type -> a generated record whose children
    are @DeviceAddress BvhNode@;
  * the @Frame@ push-constant block    -> a generated record + range, carrying
    the root node's device address; and
  * the @SAMPLES@ \/ @MAX_BOUNCES@ specialization constants -> the pipeline's
    'Vk.SpecializationInfo'.
-}
module Pathtracer
  ( Camera (..)
  , Frame (..)
  , Sphere (..)
  , BvhNode (..)
  , deviceRequirements
  , Pipeline (..)
  , allocatePipeline
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std140 (..), Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures (..))
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.Requirements.TH (reqs)
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Specialization (allocateSpecializationInfo)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pathtracer.Shader as Shader

-- Generate the records from the same SPIR-V the runtime loads:
--   * @Camera@  (std140 UBO);
--   * @Frame@   (std430 push constant);
--   * @Sphere@  (std430), the element type of the @Scene@ SSBO's @Sphere[]@ —
--     generated even though the runtime-array @Scene@/@Image@ blocks themselves
--     aren't (they're @[Sphere]@ / raw @vec4@ texels on the host); and
--   * @BvhNode@ (std430 buffer_reference) with @DeviceAddress BvhNode@ children.
reflectShaderTypesBytes Shader.code

{- | Enable buffer device addresses so the BVH nodes can be linked on the host
and traversed by 'DeviceAddress' on the GPU.
-}
deviceRequirements :: [DeviceRequirement]
deviceRequirements =
  [reqs| PhysicalDeviceBufferDeviceAddressFeatures.bufferDeviceAddress |]

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

{- | The pipeline, specialized to the given samples-per-pixel and bounce cap.

Descriptor set layout (UBO + 2 SSBOs), push-constant range and specialization
info all come from reflecting 'Shader.code'.
-}
allocatePipeline :: Vk.Device -> Word32 -> Word32 -> ResourceT IO Pipeline
allocatePipeline dev samples bounces = do
  -- Reflect the embedded module once; reuse it for the descriptor set layout,
  -- the push-constant range and the specialization info.
  reflected <- reflectBytes Shader.code

  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate

  mSpec <- allocateSpecializationInfo reflected (samples, bounces)
  (_, shader) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT mSpec Shader.code
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
  pure
    Pipeline
      { pipeline = computePipeline
      , pipelineLayout = pipelineLayout
      , descriptorSetLayout = descriptorSetLayout
      }
