{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The EVSM shadow pipeline.

The multiview occluder render (shaders in "Pipeline.Shadow.Occluder"), hand-built
because 'Dynamic.PipelineConfig' has no @viewMask@: 'basePipelineCreateInfo' is
grafted with a 'PipelineRenderingCreateInfo' carrying the @0x3F@ view mask on its
pNext chain. The push range spans VERTEX+FRAGMENT (the vertex indexes the
view-projections, the fragment reads the light position); the EVSM encoding
('Params') is specialized in.
-}
module Pipeline.Shadow
  ( allocateShadow
  , allocateSet
  , pushShadow
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Vector as V
import Data.Word (Word32)
import Geomancy (vec4, withVec4)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo (..))
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.Pipeline (Pipeline (..))
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.Pipeline.Internal (basePipelineCreateInfo, buildColorPipeline, withCompiledStages)
import Vulkan.Utils.SpirV.Pipeline (allocateReflectedLayout)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow.Occluder as Occluder
import Pipeline.Shadow.Params (Params)

-- Generate the @PC@ push-constant record (light position, view-projection base).
reflectShaderTypesBytes Occluder.vertCode

-- | The occluder pipeline, rendering into @shadowFormat@ moments + @depthFormat@ depth.
allocateShadow :: Vk.Device -> Params -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocateShadow dev params shadowFormat depthFormat = do
  vertModule <- reflectBytes Occluder.vertCode
  fragModule <- reflectBytes Occluder.fragCode
  -- Both stages declare the @PC@ block, so the merged range carries
  -- VERTEX|FRAGMENT without hand-widening.
  layout <- allocateReflectedLayout dev [vertModule, fragModule]
  pipeline <- buildPipeline dev params shadowFormat depthFormat layout.pipelineLayout
  pure Pipeline{pipeline, bindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS, layout}

-- The vertex stage declares no spec constants; its map entries are ignored.
buildPipeline :: Vk.Device -> Params -> Vk.Format -> Vk.Format -> Vk.PipelineLayout -> ResourceT IO Vk.Pipeline
buildPipeline dev params shadowFormat depthFormat layout =
  withCompiledStages dev params [(Vk.SHADER_STAGE_VERTEX_BIT, Occluder.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Occluder.fragCode)] \stages ->
    fmap snd $ buildColorPipeline dev (Just layout) \resolvedLayout ->
      SomeStruct $
        (basePipelineCreateInfo resolvedLayout Nothing 1 True (zero :: Vk.PipelineVertexInputStateCreateInfo '[]) dynStates stages)
          { Vk.depthStencilState =
              Just
                zero
                  { Vk.depthTestEnable = True
                  , Vk.depthWriteEnable = True
                  , Vk.depthCompareOp = Vk.COMPARE_OP_GREATER
                  }
          }
          ::& renderingCreateInfo
            :& ()
  where
    dynStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]
    renderingCreateInfo =
      zero
        { viewMask = 0x3F
        , colorAttachmentFormats = [shadowFormat]
        , depthAttachmentFormat = depthFormat
        }
        :: PipelineRenderingCreateInfo

-- | A set binding the vertex (0), mesh (1), object (2), view-proj (3) and instance-remap (4) SSBOs.
allocateSet :: Vk.Device -> Pipeline -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl verts meshes objects viewProj visible = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER viewProj
    , bufferWrite set 4 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER visible
    ]
    []
  pure set

{- | Push the shadow light + view-projection base.

The light position (fragment) and the view-projection base offset @light*6@ (vertex).
-}
pushShadow :: Vk.CommandBuffer -> Pipeline -> Shade.Light -> Word32 -> IO ()
pushShadow cb pl (Shade.Light pos _col) base =
  Pipeline.push
    cb
    pl
    PC
      { lightPos = withVec4 pos \x y z _sz -> vec4 x y z 0
      , lightBase = base
      }
