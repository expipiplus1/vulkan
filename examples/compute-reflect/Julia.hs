{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| Julia-set compute pipeline, its interface reflected from 'Julia.Shader.code':

  * 'reflectShaderTypesBytes' generates the 'Params' push-constant record (with
    a gl-block std430 'Storable');
  * 'singleDescriptorSetLayoutInfo' builds the descriptor set layout for the
    output SSBO;
  * 'pushConstantRanges' builds the pipeline layout's push-constant range; and
  * 'allocateSpecializationInfo' builds the pipeline's specialization info
    (@maxIterations@, @escapeRadius@) from the reflected constant ids.

Compare with the @compute@ example, which hand-writes all of this.
-}
module Julia
  ( Params (..)
  , Pipeline (..)
  , allocatePipeline
  , workgroup
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Specialization (allocateSpecializationInfo)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Julia.Shader as Shader

-- Generate the @Params@ push-constant record (and its std430 'Storable') from
-- the same SPIR-V the runtime loads.
reflectShaderTypesBytes Shader.code

-- | Workgroup size on each axis (matches @local_size_x/y@ in the shader).
workgroup :: Int
workgroup = 16

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

{- | The pipeline, specialized to the given iteration cap and escape radius.

Descriptor set layout, push-constant range and specialization info all come
from reflecting 'Shader.code'.
-}
allocatePipeline :: Vk.Device -> Word32 -> Float -> ResourceT IO Pipeline
allocatePipeline dev maxIterations escapeRadius = do
  -- Reflect the embedded module once; reuse it for the descriptor set layout,
  -- the push-constant range and the specialization info.
  reflected <- reflectBytes Shader.code

  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate

  mSpec <- allocateSpecializationInfo reflected (maxIterations, escapeRadius)
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
