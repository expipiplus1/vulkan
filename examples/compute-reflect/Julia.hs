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
    a gl-block std430 'Storable'); and
  * 'allocateCompute' reflects the same bytes at load time — the descriptor set
    layout for the output SSBO, the push-constant range and the specialization
    info (@maxIterations@, @escapeRadius@) all come from them, bundled as a
    "Vulkan.Utils.Pipeline" 'Pipeline'.

Compare with the @compute@ example, which hand-writes all of this.
-}
module Julia
  ( Params (..)
  , allocatePipeline
  , workgroup
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline (Pipeline)
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Julia.Shader as Shader

-- Generate the @Params@ push-constant record (and its std430 'Storable') from
-- the same SPIR-V the runtime loads.
reflectShaderTypesBytes Shader.code

-- | Workgroup size on each axis (matches @local_size_x/y@ in the shader).
workgroup :: Int
workgroup = 16

{- | The pipeline, specialized to the given iteration cap and escape radius.

The specialization constants are packed in ascending @constant_id@ order:
id 0 = @maxIterations@ (uint), id 1 = @escapeRadius@ (float).
-}
allocatePipeline :: Vk.Device -> Word32 -> Float -> ResourceT IO Pipeline
allocatePipeline dev maxIterations escapeRadius =
  allocateCompute dev (maxIterations, escapeRadius) Shader.code
