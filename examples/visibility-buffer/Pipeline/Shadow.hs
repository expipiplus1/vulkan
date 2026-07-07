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
view-projections, the fragment reads the light position).
-}
module Pipeline.Shadow
  ( Pipeline (..)
  , allocateShadow
  , allocateSet
  , pushShadow
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import qualified Geomancy
import Geomancy.Vec4 (vec4)
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo (..))
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.Pipeline.Internal (basePipelineCreateInfo, buildColorPipeline, withCompiledStages)
import Vulkan.Utils.SpirV.Descriptors (singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Shadow.Occluder as Occluder

-- Generate the @PC@ push-constant record (light position+far, view-projection base).
reflectShaderTypesBytes Occluder.vertCode

{- | Push-constant byte extent.

@vec4 lightPosFar + uint lightBase@. The std430 'Storable' trailing-pads to 32; the
range (and this push) span only the 20 real bytes, so vertex+fragment both see the
data.
-}
pushBytes :: Word32
pushBytes = 20

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

-- | The occluder pipeline, rendering into @shadowFormat@ moments + @depthFormat@ depth.
allocateShadow :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocateShadow dev shadowFormat depthFormat = do
  reflected <- reflectBytes Occluder.vertCode
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = [Vk.PushConstantRange (Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT) 0 pushBytes]}
      Nothing
      allocate
  pipeline <- buildPipeline dev shadowFormat depthFormat pipelineLayout
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout}

buildPipeline :: Vk.Device -> Vk.Format -> Vk.Format -> Vk.PipelineLayout -> ResourceT IO Vk.Pipeline
buildPipeline dev shadowFormat depthFormat layout =
  withCompiledStages dev () [(Vk.SHADER_STAGE_VERTEX_BIT, Occluder.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Occluder.fragCode)] \stages ->
    fmap snd $ buildColorPipeline dev (Just layout) \resolvedLayout ->
      SomeStruct $
        (basePipelineCreateInfo resolvedLayout Nothing 1 True (zero :: Vk.PipelineVertexInputStateCreateInfo '[]) dynStates stages)
          { Vk.depthStencilState = Just zero{Vk.depthTestEnable = True, Vk.depthWriteEnable = True, Vk.depthCompareOp = Vk.COMPARE_OP_GREATER}
          }
          ::& renderingCreateInfo
            :& ()
  where
    dynStates = V.fromList [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]
    renderingCreateInfo = zero{viewMask = 0x3F, colorAttachmentFormats = [shadowFormat], depthAttachmentFormat = depthFormat} :: PipelineRenderingCreateInfo

-- | A set binding the vertex (0), mesh (1), object (2) and view-proj (3) SSBOs.
allocateSet :: Vk.Device -> Pipeline -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl verts meshes objects viewProj = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 4]}
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER viewProj
    ]
    []
  pure set

{- | Push the shadow light + view-projection base (20 bytes).

The light position + far plane (fragment) and the view-projection base offset
@light*6@ (vertex).
-}
pushShadow :: Vk.CommandBuffer -> Vk.PipelineLayout -> (Float, Float, Float) -> Float -> Word32 -> IO ()
pushShadow cb layout (x, y, z) far base =
  with pc \p ->
    Vk.cmdPushConstants cb layout (Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT) 0 pushBytes (castPtr p)
  where
    pc = PC{lightPosFar = vec4 x y z far, lightBase = base}
