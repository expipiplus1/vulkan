{-# LANGUAGE OverloadedLists #-}

{-| The classic render-pass drawing path: a 'Vk.RenderPass' over one or more
colour attachments (and an optional depth attachment), framebuffers over the
swapchain image views, and a vanilla pipeline that targets the render pass.

This is one of two self-contained alternatives — see
"Vulkan.Utils.DynamicRendering" for the @VK_KHR_dynamic_rendering@ path, which
needs neither a render pass nor framebuffers. Pick one and import only it.
-}
module Vulkan.Utils.RenderPass
  ( -- * Render pass
    allocateRenderPass
  , allocateColorRenderPass

    -- * Pipeline
  , PipelineConfig (..)
  , allocatePipeline
  , allocatePipelineFromShaders
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.DynamicState (defaultDynamicStatesFor)
import Vulkan.Utils.Pipeline.Internal (basePipelineCreateInfo, buildColorPipeline, withCompiledStages)
import Vulkan.Utils.Pipeline.Specialization (Specialization)
import Vulkan.Zero (Zero (..))

{- | A render pass with @colors@ colour attachments (each @(format, finalLayout)@)
and an optional depth attachment, all cleared on load and stored on completion, in
a single graphics subpass. Attachment indices are the colours @0..N-1@ then the
depth attachment at @N@ — the colour-then-depth order the framebuffer's
@attachments@ must follow. The external
dependency synchronizes colour output and, when present, the
depth fragment tests.
-}
allocateRenderPass
  :: (MonadResource m)
  => Vk.Device
  -> Vector (Vk.Format, Vk.ImageLayout)
  -- ^ Colour attachments: @(format, finalLayout)@.
  -> Maybe Vk.Format
  -- ^ Optional depth attachment format.
  -> m (ReleaseKey, Vk.RenderPass)
allocateRenderPass dev colors depth =
  Vk.withRenderPass
    dev
    zero
      { Vk.attachments = colorDescriptions <> depthDescriptions
      , Vk.subpasses = [subpass]
      , Vk.dependencies = [subpassDependency]
      }
    Nothing
    allocate
  where
    colorCount = V.length colors
    hasColor = colorCount > 0
    hasDepth = isJust depth

    colorDescriptions :: Vector Vk.AttachmentDescription
    colorDescriptions = fmap (uncurry colorAttachmentDescription) colors

    depthDescriptions :: Vector Vk.AttachmentDescription
    depthDescriptions = maybe [] (V.singleton . depthAttachmentDescription) depth

    colorReferences :: Vector Vk.AttachmentReference
    colorReferences =
      V.imap
        ( \i _ ->
            zero
              { Vk.attachment = fromIntegral i
              , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
              }
        )
        colors

    depthReference :: Maybe Vk.AttachmentReference
    depthReference
      | hasDepth =
          Just
            zero
              { Vk.attachment = fromIntegral colorCount
              , Vk.layout = Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
              }
      | otherwise = Nothing

    subpass :: Vk.SubpassDescription
    subpass =
      zero
        { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.colorAttachments = colorReferences
        , Vk.depthStencilAttachment = depthReference
        }

    subpassDependency :: Vk.SubpassDependency
    subpassDependency =
      zero
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = 0
        , Vk.srcStageMask = stageMask
        , Vk.srcAccessMask = zero
        , Vk.dstStageMask = stageMask
        , Vk.dstAccessMask = accessMask
        }

    stageMask =
      (if hasColor then Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT else zero)
        .|. ( if hasDepth
                then
                  Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
                    .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
                else zero
            )
    accessMask =
      ( if hasColor
          then Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          else zero
      )
        .|. (if hasDepth then Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT else zero)

colorAttachmentDescription :: Vk.Format -> Vk.ImageLayout -> Vk.AttachmentDescription
colorAttachmentDescription imageFormat finalLayout =
  zero
    { Vk.format = imageFormat
    , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
    , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
    , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
    , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
    , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
    , Vk.finalLayout = finalLayout
    }

depthAttachmentDescription :: Vk.Format -> Vk.AttachmentDescription
depthAttachmentDescription imageFormat =
  zero
    { Vk.format = imageFormat
    , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
    , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
    , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
    , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
    , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
    , Vk.finalLayout = Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
    }

{- | The single-colour render pass: one attachment cleared on load and stored,
ending in @finalLayout@ (e.g. @PRESENT_SRC_KHR@ for swapchains,
@TRANSFER_SRC_OPTIMAL@ for offscreen images). The common special case of
'allocateRenderPass'.
-}
allocateColorRenderPass
  :: (MonadResource m)
  => Vk.Device
  -> Vk.Format
  -- ^ Color attachment format.
  -> Vk.ImageLayout
  -- ^ Final layout.
  -> m (ReleaseKey, Vk.RenderPass)
allocateColorRenderPass dev imageFormat finalLayout =
  allocateRenderPass dev [(imageFormat, finalLayout)] Nothing

{- | Attachment + fixed-function knobs for a render-pass pipeline.

Construct with 'zero' and override what differs, e.g.
@zero { RenderPass.colorFormats = [fmt], RenderPass.depthFormat = Just d }@. The
attachment shape — 'colorFormats' count and whether 'depthFormat' is present — MUST
match the render pass; the formats themselves live in the render pass, so only the
count and depth presence are read here.
-}
data PipelineConfig = PipelineConfig
  { colorFormats :: [Vk.Format]
  -- ^ Colour attachment formats; only the count is read (must match the render pass).
  , depthFormat :: Maybe Vk.Format
  -- ^ Optional depth attachment; only its presence is read (must match the render pass).
  , vertexInput :: Vk.PipelineVertexInputStateCreateInfo '[]
  -- ^ Vertex input (bindings + attributes); 'zero' for none.
  , dynamicStates :: Maybe (Vector Vk.DynamicState)
  -- ^ Dynamic states; 'Nothing' defaults layout-aware (see "Vulkan.Utils.DynamicState").
  , layout :: Maybe Vk.PipelineLayout
  {- ^ Pipeline layout for descriptor sets \/ push constants; 'Nothing' uses a
  transient empty layout (shaders take no resources). A supplied layout stays
  owned by the caller, who must keep it alive for the pipeline's lifetime.
  -}
  }

instance Zero PipelineConfig where
  zero =
    PipelineConfig
      { colorFormats = []
      , depthFormat = Nothing
      , vertexInput = zero
      , dynamicStates = Nothing
      , layout = Nothing
      }

{- | A vanilla vertex+fragment pipeline targeting @renderPass@ (subpass 0). The
'PipelineConfig' attachment shape MUST match @renderPass@. Whatever dynamic state
is selected MUST be set before drawing. Intended to be used qualified, e.g.
@RenderPass.allocatePipeline@.
-}
allocatePipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> PipelineConfig
  -> Vector (SomeStruct Vk.PipelineShaderStageCreateInfo)
  -> m (ReleaseKey, Vk.Pipeline)
allocatePipeline dev renderPass PipelineConfig{..} stages =
  buildColorPipeline dev layout $ \resolvedLayout ->
    SomeStruct
      ( basePipelineCreateInfo
          resolvedLayout
          (Just renderPass)
          (length colorFormats)
          (isJust depthFormat)
          vertexInput
          (fromMaybe (defaultDynamicStatesFor (not (null colorFormats))) dynamicStates)
          stages
      )

{- | 'allocatePipeline' from @(stage, SPIR-V)@ pairs: compile each into a shader
module, build the pipeline, then release the now-redundant module handles.

@spec@ is one specialization shared by every stage (see
'Vulkan.Utils.Pipeline.Specialization'); pass @()@ for none.
-}
allocatePipelineFromShaders
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> Vk.RenderPass
  -> PipelineConfig
  -> spec
  -- ^ Specialization shared by every stage; @()@ for none.
  -> [(Vk.ShaderStageFlagBits, ByteString)]
  -> m (ReleaseKey, Vk.Pipeline)
allocatePipelineFromShaders dev renderPass config spec shaders =
  withCompiledStages dev spec shaders (allocatePipeline dev renderPass config)
