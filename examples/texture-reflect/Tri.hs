{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The offscreen pipeline: the RGB triangle drawn into the image the cube
samples.

Reflection of 'Shader.fragCode' generates the shared @Globals@ UBO record
(the cube pass references the same struct, so it is generated once, here); the
per-stage signatures prove the vertex\/fragment composition at compile time
('composes').
-}
module Tri
  ( Globals (..)
  , composes
  , allocatePipeline
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Graphics.Gl.Block (Std140 (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Pipeline (Pipeline (..), Set)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)

import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSigBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Tri.Shader as Shader

-- Compile-time reflection: the shared @Globals@ UBO record and a stage
-- signature per shader.
reflectShaderTypesBytes Shader.fragCode
reflectStageSigBytes "VertSig" Shader.vertCode
reflectStageSigBytes "FragSig" Shader.fragCode

-- | This pipeline's vertex↔fragment composition, proved at compile time.
composes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
composes = True

{- | Colour only, no vertex input. The set 0 'Set' is shared with the cube
pipeline, making the two pipeline layouts compatible for set 0.
-}
allocatePipeline :: Vk.Device -> Vk.Format -> Set -> ResourceT IO Pipeline
allocatePipeline dev colorFormat set0 = do
  layout <- Pipeline.allocateLayout dev [(0, set0)] []
  (_, pipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.layout = Just layout.pipelineLayout
        }
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, Shader.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Shader.fragCode)]
  pure Pipeline{pipeline, bindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS, layout}
