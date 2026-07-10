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
import Geomancy (vec4, withVec4)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo (..))
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.Pipeline.Internal (basePipelineCreateInfo, buildColorPipeline, withCompiledStages)
import Vulkan.Utils.SpirV.Descriptors (pushConstantsSize, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow.Occluder as Occluder
import Pipeline.Shadow.Params (Params)

-- Generate the @PC@ push-constant record (light position, view-projection base).
reflectShaderTypesBytes Occluder.vertCode

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  , pushSize :: Word32
  {- ^ Reflected @PC@ range size (< the std430 'Storable' size, which trailing-pads to
  32) — push exactly this many bytes to satisfy the layout.
  -}
  }

-- | The occluder pipeline, rendering into @shadowFormat@ moments + @depthFormat@ depth.
allocateShadow :: Vk.Device -> Params -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocateShadow dev params shadowFormat depthFormat = do
  reflected <- reflectBytes Occluder.vertCode
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  -- Reflected from the vertex stage alone, so widen the range to the stages that read it.
  let pushSize = pushConstantsSize reflected
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = [Vk.PushConstantRange (Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT) 0 pushSize]}
      Nothing
      allocate
  pipeline <- buildPipeline dev params shadowFormat depthFormat pipelineLayout
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout, pushSize}

-- The vertex stage declares no spec constants; its map entries are ignored.
buildPipeline :: Vk.Device -> Params -> Vk.Format -> Vk.Format -> Vk.PipelineLayout -> ResourceT IO Vk.Pipeline
buildPipeline dev params shadowFormat depthFormat layout =
  withCompiledStages dev params [(Vk.SHADER_STAGE_VERTEX_BIT, Occluder.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Occluder.fragCode)] \stages ->
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

-- | A set binding the vertex (0), mesh (1), object (2), view-proj (3) and instance-remap (4) SSBOs.
allocateSet :: Vk.Device -> Pipeline -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl verts meshes objects viewProj visible = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 5]}
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
    , bufferWrite set 4 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER visible
    ]
    []
  pure set

{- | Push the shadow light + view-projection base.

The light position (fragment) and the view-projection base offset @light*6@ (vertex).
-}
pushShadow :: Vk.CommandBuffer -> Pipeline -> Shade.Light -> Word32 -> IO ()
pushShadow cb pl (Shade.Light pos _col) base =
  with pc \p ->
    Vk.cmdPushConstants cb pl.pipelineLayout (Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT) 0 pl.pushSize (castPtr p)
  where
    pc =
      PC
        { lightPos = withVec4 pos \x y z _sz -> vec4 x y z 0
        , lightBase = base
        }
