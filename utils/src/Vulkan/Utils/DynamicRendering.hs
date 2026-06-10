{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

{-| The @VK_KHR_dynamic_rendering@ (Vulkan 1.3 core) drawing path: no
'Vk.RenderPass' and no framebuffers. The swapchain image's layout transitions
are handled by explicit pipeline barriers, and the rendering region is opened
with 'Vk.cmdUseRendering' against a 'Vk.RenderingInfo' pointing straight at an
image view. The pipeline carries a @PipelineRenderingCreateInfo@ instead of a
render pass.

This is one of two self-contained alternatives — see "Vulkan.Utils.RenderPass"
for the classic path. Pick one and import only it. Callers must have enabled
the @dynamicRendering@ feature on the device.
-}
module Vulkan.Utils.DynamicRendering
  ( -- * Pipeline
    createPipeline
  , createPipelineFromShaders

    -- * Device requirements
  , dynamicRenderingRequirements

    -- * Rendering
  , colorAttachmentRenderingInfo
  , renderingInfo
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures, PipelineRenderingCreateInfo (..))
import Vulkan.Requirement (DeviceRequirement)
import Vulkan.Utils.DynamicState (defaultDynamicStatesFor)
import Vulkan.Utils.Pipeline.Internal (basePipelineCreateInfo, buildColorPipeline, withCompiledStages)
import Vulkan.Utils.Pipeline.Specialization (Specialization)
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Zero (zero)

{- | The device requirements for this path: the @VK_KHR_dynamic_rendering@
extension (core since Vulkan 1.3) and the @dynamicRendering@ feature it gates.
Use it directly as — or append it to — a consumer's device requirements (e.g.
@WindowedBoot@'s @wcDeviceReqs@) so callers need not spell out the feature.
-}
dynamicRenderingRequirements :: [DeviceRequirement]
dynamicRenderingRequirements =
  [U.reqs|
    VK_KHR_dynamic_rendering
    PhysicalDeviceDynamicRenderingFeatures.dynamicRendering
  |]

{- | Build a graphics pipeline for the dynamic-rendering path: no render pass, the
attachment formats carried in a 'PipelineRenderingCreateInfo' on the pNext chain.
The attachment shape — colour formats and optional depth — selects the pipeline
kind:

  * @[fmt] Nothing@ — a single-colour pipeline (as the classic path).
  * @[fmt] (Just d)@ — colour + depth (depth driven dynamically).
  * @[] (Just d)@ — depth-only (e.g. a shadow map / z-prepass).
  * @[f0, f1, …] mDepth@ — multiple colour attachments (MRT / G-buffer).

Stencil is out of scope: no @stencilAttachmentFormat@ is declared, matching
'renderingInfo', which never supplies a stencil attachment (declaring one
without supplying it is invalid at draw time). Use a depth-only @depth@ format;
for stencil, build the 'PipelineRenderingCreateInfo' and 'Vk.RenderingInfo' by
hand. The formats MUST match the views passed to 'Vk.cmdUseRendering' at draw
time (see 'renderingInfo'). Intended to be used qualified, e.g.
@Dynamic.createPipeline@.
-}
createPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vector Vk.Format
  -- ^ Colour attachment formats (@0@..@N@).
  -> Maybe Vk.Format
  -- ^ Optional depth attachment format.
  -> Vk.PipelineVertexInputStateCreateInfo '[]
  -- ^ Vertex input (bindings + attributes); @zero@ for none.
  -> Maybe (Vector Vk.DynamicState)
  {- ^ Dynamic states; 'Nothing' defaults layout-aware to
  'Vulkan.Utils.DynamicState.depthOnlyDynamicStates' (no colour) or
  'allDynamicStates'. Drive with 'Vulkan.Utils.DynamicState.applyDynamicStates'.
  -}
  -> Maybe Vk.PipelineLayout
  {- ^ Pipeline layout for descriptor sets \/ push constants; 'Nothing' uses a
  transient empty layout (shaders take no resources). A supplied layout stays
  owned by the caller, who must keep it alive for the pipeline's lifetime.
  -}
  -> Vector (SomeStruct Vk.PipelineShaderStageCreateInfo)
  -> m (ReleaseKey, Vk.Pipeline)
createPipeline dev colorFormats depthFormat vertexInput dynamicStates pipelineLayout stages =
  buildColorPipeline dev pipelineLayout $ \layout ->
    SomeStruct $
      basePipelineCreateInfo
        layout
        Nothing
        (V.length colorFormats)
        (isJust depthFormat)
        vertexInput
        (fromMaybe (defaultDynamicStatesFor (not (V.null colorFormats))) dynamicStates)
        stages
        ::& renderingCreateInfo
          :& ()
  where
    renderingCreateInfo :: PipelineRenderingCreateInfo
    renderingCreateInfo =
      zero
        { colorAttachmentFormats = colorFormats
        , depthAttachmentFormat = fromMaybe Vk.FORMAT_UNDEFINED depthFormat
        }

{- | 'createPipeline' from @(stage, SPIR-V)@ pairs: compile each into a shader
module, build the pipeline, then release the now-redundant module handles.
-}
createPipelineFromShaders
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> [Vk.Format]
  -- ^ Colour attachment formats.
  -> Maybe Vk.Format
  -- ^ Optional depth attachment format.
  -> Vk.PipelineVertexInputStateCreateInfo '[]
  -- ^ Vertex input (bindings + attributes); @zero@ for none.
  -> Maybe (Vector Vk.DynamicState)
  -- ^ Dynamic states (see 'createPipeline').
  -> Maybe Vk.PipelineLayout
  -- ^ Pipeline layout (see 'createPipeline'); 'Nothing' for an empty one.
  -> spec
  -- ^ Specialization shared by every stage (see "Vulkan.Utils.Pipeline.Specialization"); @()@ for none.
  -> [(Vk.ShaderStageFlagBits, ByteString)]
  -> m (ReleaseKey, Vk.Pipeline)
createPipelineFromShaders dev colorFormats depthFormat vertexInput dynamicStates pipelineLayout spec shaders =
  withCompiledStages dev spec shaders $
    createPipeline dev (V.fromList colorFormats) depthFormat vertexInput dynamicStates pipelineLayout

{- | A 'Vk.RenderingInfo' targeting a single color attachment that is cleared
on load and stored on completion. The attachment is expected to already be in
@IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@ (e.g. via
'Vulkan.Utils.Barrier.transitionColorAttachment'). The single-colour special
case of 'renderingInfo'.
-}
colorAttachmentRenderingInfo
  :: Vk.Rect2D
  -- ^ Render area (typically the full swapchain extent).
  -> Vk.ImageView
  -- ^ Target color attachment view.
  -> Vk.ClearColorValue
  -- ^ Clear color applied by the @LOAD_OP_CLEAR@.
  -> Vk.RenderingInfo '[]
colorAttachmentRenderingInfo renderArea imageView clearColor =
  renderingInfo renderArea [(imageView, clearColor)] Nothing

{- | A 'Vk.RenderingInfo' over any number of colour attachments plus an optional
depth attachment, each cleared on load and stored on completion. Colour
attachments are expected in @IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@ and the depth
attachment in @IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL@ (e.g. via
'Vulkan.Utils.Barrier.transitionColorAttachment' /
'Vulkan.Utils.Barrier.transitionDepthAttachment'). The attachment shape MUST
match the pipeline ('createPipeline'). No stencil attachment is supplied,
matching 'createPipeline' never declaring one.
-}
renderingInfo
  :: Vk.Rect2D
  -- ^ Render area (typically the full swapchain extent).
  -> Vector (Vk.ImageView, Vk.ClearColorValue)
  -- ^ Colour attachment views with the clear colour applied by @LOAD_OP_CLEAR@.
  -> Maybe (Vk.ImageView, Float)
  -- ^ Optional depth attachment view with the clear depth applied by @LOAD_OP_CLEAR@.
  -> Vk.RenderingInfo '[]
renderingInfo renderArea colorTargets depthTarget =
  zero
    { Vk.renderArea = renderArea
    , Vk.layerCount = 1
    , Vk.colorAttachments = fmap colorAttachment colorTargets
    , Vk.depthAttachment = fmap depthAttachment depthTarget
    }
  where
    colorAttachment :: (Vk.ImageView, Vk.ClearColorValue) -> SomeStruct Vk.RenderingAttachmentInfo
    colorAttachment (imageView, clearColor) =
      SomeStruct
        zero
          { Vk.imageView = imageView
          , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
          , Vk.clearValue = Vk.Color clearColor
          }
    depthAttachment :: (Vk.ImageView, Float) -> SomeStruct Vk.RenderingAttachmentInfo
    depthAttachment (imageView, clearDepth) =
      SomeStruct
        zero
          { Vk.imageView = imageView
          , Vk.imageLayout = Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
          , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
          , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
          , Vk.clearValue = Vk.DepthStencil (Vk.ClearDepthStencilValue clearDepth 0)
          }
