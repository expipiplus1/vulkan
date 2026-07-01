{-# LANGUAGE OverloadedLists #-}

{-| The Vulkan-1.3-core "always-on" dynamic state: the set of pipeline state
that can be made dynamic without any vendor or experimental extension, so a
single pipeline object serves every combination instead of permuting into one
'Vk.Pipeline' per variation. See 'Vulkan.Utils.RenderPass.allocatePipeline'
and 'Vulkan.Utils.DynamicRendering.allocatePipeline'.

Because these states are declared dynamic, the matching @cmdSet*@ MUST be issued
before each draw (an unset dynamic state is undefined behaviour). The
'DynamicState' record carries the whole set with safe defaults; amend it with
the per-frame situation (the swapchain extent via 'dynamicStateFor') and emit
the lot in one go with 'applyDynamicStates' over 'allDynamicStates'.

Defaults mirror Vulkan's zero-initialized values, deviating only where a
non-zero is required for validity ('lineWidth' @= 1@) or to match the pipeline's
baked topology class ('topology' @= TRIANGLE_LIST@). They are feature-free
(no @wideLines@, no @depthBounds@) and valid with a colour-only attachment (all
depth/stencil tests disabled).
-}
module Vulkan.Utils.DynamicState
  ( -- * The dynamic-state record
    DynamicState (..)
  , defaultDynamicState
  , dynamicStateFor

    -- * Applying it
  , applyDynamicStates

    -- * The state set (pipeline @dynamicStates@ ⇆ apply, single source of truth)
  , allDynamicStates
  , depthOnlyDynamicStates
  , defaultDynamicStatesFor
  , minimalDynamicStates
  , noDynamicStates
  , preRasterizationStates
  , fragmentTestStates
  , fragmentOutputStates

    -- * Whole-extent viewport/scissor
  , fullViewport
  , fullScissor
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Vulkan.Core10 as Rect2D (Rect2D (..))
import qualified Vulkan.Core10 as Viewport (Viewport (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Zero (zero)

-- | A viewport covering the whole extent, with depth range @0@ to @1@.
fullViewport :: Vk.Extent2D -> Vk.Viewport
fullViewport (Vk.Extent2D w h) =
  zero
    { Viewport.width = realToFrac w
    , Viewport.height = realToFrac h
    , Viewport.maxDepth = 1
    }

-- | A scissor rectangle covering the whole extent (offset at the origin).
fullScissor :: Vk.Extent2D -> Vk.Rect2D
fullScissor extent = zero{Rect2D.extent = extent}

----------------------------------------------------------------
-- The record
----------------------------------------------------------------

{- | Every Vulkan-1.3-core always-on dynamic state, as plain values. Field types
match the @cmdSet*@ argument shapes. The stencil ops apply to
@STENCIL_FACE_FRONT_AND_BACK@.
-}
data DynamicState = DynamicState
  { -- Pre-rasterization
    viewports :: Vector Vk.Viewport
  , scissors :: Vector Vk.Rect2D
  , topology :: Vk.PrimitiveTopology
  , primitiveRestart :: Bool
  , cullMode :: Vk.CullModeFlags
  , frontFace :: Vk.FrontFace
  , rasterizerDiscard :: Bool
  , lineWidth :: Float
  -- ^ @/= 1@ requires the @wideLines@ feature.
  , depthBiasEnable :: Bool
  , depthBias :: (Float, Float, Float)
  -- ^ @(constantFactor, clamp, slopeFactor)@.
  , -- Fragment-shader depth/stencil tests
    depthTest :: Bool
  , depthWrite :: Bool
  , depthCompareOp :: Vk.CompareOp
  , depthBoundsTest :: Bool
  -- ^ Requires the @depthBounds@ feature.
  , depthBounds :: (Float, Float)
  -- ^ @(min, max)@.
  , stencilTest :: Bool
  , stencilFailOp :: Vk.StencilOp
  , stencilPassOp :: Vk.StencilOp
  , stencilDepthFailOp :: Vk.StencilOp
  , stencilCompareOp :: Vk.CompareOp
  , stencilCompareMask :: Word32
  , stencilWriteMask :: Word32
  , stencilReference :: Word32
  , -- Fragment-output
    blendConstants :: (Float, Float, Float, Float)
  }

{- | Safe, feature-free defaults (see module header). 'viewports' and 'scissors'
are empty — supply them with 'dynamicStateFor' or by record update before
applying.
-}
defaultDynamicState :: DynamicState
defaultDynamicState =
  DynamicState
    { viewports = V.empty
    , scissors = V.empty
    , topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
    , primitiveRestart = False
    , cullMode = Vk.CULL_MODE_NONE
    , frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE
    , rasterizerDiscard = False
    , lineWidth = 1
    , depthBiasEnable = False
    , depthBias = (0, 0, 0)
    , depthTest = False
    , depthWrite = False
    , depthCompareOp = Vk.COMPARE_OP_NEVER
    , depthBoundsTest = False
    , depthBounds = (0, 0)
    , stencilTest = False
    , stencilFailOp = Vk.STENCIL_OP_KEEP
    , stencilPassOp = Vk.STENCIL_OP_KEEP
    , stencilDepthFailOp = Vk.STENCIL_OP_KEEP
    , stencilCompareOp = Vk.COMPARE_OP_NEVER
    , stencilCompareMask = 0
    , stencilWriteMask = 0
    , stencilReference = 0
    , blendConstants = (0, 0, 0, 0)
    }

{- | 'defaultDynamicState' with the whole-extent viewport and scissor filled in.
The common entry point; amend further by record update, e.g.

@
'applyDynamicStates' 'allDynamicStates' cb ('dynamicStateFor' ext){ 'cullMode' = Vk.CULL_MODE_BACK_BIT }
@
-}
dynamicStateFor :: Vk.Extent2D -> DynamicState
dynamicStateFor ext =
  defaultDynamicState
    { viewports = V.singleton (fullViewport ext)
    , scissors = V.singleton (fullScissor ext)
    }

----------------------------------------------------------------
-- The state set (single source of truth)
----------------------------------------------------------------

preRasterizationStates :: Vector Vk.DynamicState
preRasterizationStates =
  [ Vk.DYNAMIC_STATE_VIEWPORT_WITH_COUNT
  , Vk.DYNAMIC_STATE_SCISSOR_WITH_COUNT
  , Vk.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY
  , Vk.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE
  , Vk.DYNAMIC_STATE_CULL_MODE
  , Vk.DYNAMIC_STATE_FRONT_FACE
  , Vk.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE
  , Vk.DYNAMIC_STATE_LINE_WIDTH
  , Vk.DYNAMIC_STATE_DEPTH_BIAS_ENABLE
  , Vk.DYNAMIC_STATE_DEPTH_BIAS
  ]

-- | Fragment-shader depth/stencil dynamic states.
fragmentTestStates :: Vector Vk.DynamicState
fragmentTestStates =
  [ Vk.DYNAMIC_STATE_DEPTH_TEST_ENABLE
  , Vk.DYNAMIC_STATE_DEPTH_WRITE_ENABLE
  , Vk.DYNAMIC_STATE_DEPTH_COMPARE_OP
  , Vk.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE
  , Vk.DYNAMIC_STATE_DEPTH_BOUNDS
  , Vk.DYNAMIC_STATE_STENCIL_TEST_ENABLE
  , Vk.DYNAMIC_STATE_STENCIL_OP
  , Vk.DYNAMIC_STATE_STENCIL_COMPARE_MASK
  , Vk.DYNAMIC_STATE_STENCIL_WRITE_MASK
  , Vk.DYNAMIC_STATE_STENCIL_REFERENCE
  ]

fragmentOutputStates :: Vector Vk.DynamicState
fragmentOutputStates = [Vk.DYNAMIC_STATE_BLEND_CONSTANTS]

{- | The whole always-on set: pass this (or 'Nothing', which resolves to it) to a
full-dynamic pipeline builder, and feed the same set to 'applyDynamicStates' so
the pipeline's @dynamicStates@ and the per-frame applies stay in lockstep.
-}
allDynamicStates :: Vector Vk.DynamicState
allDynamicStates =
  preRasterizationStates <> fragmentTestStates <> fragmentOutputStates

{- | The set for a depth-only pipeline (no colour attachment): everything in
'allDynamicStates' except 'fragmentOutputStates' ('Vk.DYNAMIC_STATE_BLEND_CONSTANTS'),
which has no colour attachment to act on. Pair it with a depth-only pipeline and
feed the same set to 'applyDynamicStates'.
-}
depthOnlyDynamicStates :: Vector Vk.DynamicState
depthOnlyDynamicStates = preRasterizationStates <> fragmentTestStates

{- | The default dynamic-state set for an attachment shape: 'allDynamicStates' when
there is at least one colour attachment, otherwise 'depthOnlyDynamicStates'. The
default a pipeline builder resolves @Nothing@ to.
-}
defaultDynamicStatesFor
  :: Bool
  -- ^ Whether the pipeline has any colour attachment.
  -> Vector Vk.DynamicState
defaultDynamicStatesFor hasColor
  | hasColor = allDynamicStates
  | otherwise = depthOnlyDynamicStates

{- | Just the (fixed-count) viewport and scissor. For pipelines that want only
those dynamic and set them with 'Vk.cmdSetViewport' / 'Vk.cmdSetScissor' (not the
with-count variants 'applyDynamicStates' issues for 'allDynamicStates').
-}
minimalDynamicStates :: Vector Vk.DynamicState
minimalDynamicStates =
  [Vk.DYNAMIC_STATE_VIEWPORT, Vk.DYNAMIC_STATE_SCISSOR]

{- | Declare /nothing/ dynamic: every state is baked into the pipeline, so no
@cmdSet*@ need be issued (skip 'applyDynamicStates' entirely). Pass it where a
builder takes @Maybe (Vector Vk.DynamicState)@.

The pipeline must then carry a complete static state — crucially a baked viewport
and scissor — so this suits a builder that bakes those (typically a fixed-size
offscreen target). The colour-pipeline builders here instead leave viewport and
scissor dynamic, so they pair with 'Nothing' / 'allDynamicStates', not this.
-}
noDynamicStates :: Maybe (Vector Vk.DynamicState)
noDynamicStates = Just V.empty

----------------------------------------------------------------
-- Applying
----------------------------------------------------------------

{- | Issue the @cmdSet*@ for exactly the given states, pulling values from the
record. Use the same set passed to the pipeline builder for exact lockstep.
States not modelled by 'DynamicState' are skipped (the caller must set those
themselves).
-}
applyDynamicStates
  :: (MonadIO m) => Vector Vk.DynamicState -> Vk.CommandBuffer -> DynamicState -> m ()
applyDynamicStates states cb s = traverse_ go states
  where
    DynamicState{..} = s
    bothFaces = Vk.STENCIL_FACE_FRONT_AND_BACK
    (biasConstant, biasClamp, biasSlope) = depthBias
    (minBound', maxBound') = depthBounds
    go = \case
      Vk.DYNAMIC_STATE_VIEWPORT_WITH_COUNT -> Vk.cmdSetViewportWithCount cb viewports
      Vk.DYNAMIC_STATE_SCISSOR_WITH_COUNT -> Vk.cmdSetScissorWithCount cb scissors
      Vk.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY -> Vk.cmdSetPrimitiveTopology cb topology
      Vk.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE -> Vk.cmdSetPrimitiveRestartEnable cb primitiveRestart
      Vk.DYNAMIC_STATE_CULL_MODE -> Vk.cmdSetCullMode cb cullMode
      Vk.DYNAMIC_STATE_FRONT_FACE -> Vk.cmdSetFrontFace cb frontFace
      Vk.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE -> Vk.cmdSetRasterizerDiscardEnable cb rasterizerDiscard
      Vk.DYNAMIC_STATE_LINE_WIDTH -> Vk.cmdSetLineWidth cb lineWidth
      Vk.DYNAMIC_STATE_DEPTH_BIAS_ENABLE -> Vk.cmdSetDepthBiasEnable cb depthBiasEnable
      Vk.DYNAMIC_STATE_DEPTH_BIAS -> Vk.cmdSetDepthBias cb biasConstant biasClamp biasSlope
      Vk.DYNAMIC_STATE_DEPTH_TEST_ENABLE -> Vk.cmdSetDepthTestEnable cb depthTest
      Vk.DYNAMIC_STATE_DEPTH_WRITE_ENABLE -> Vk.cmdSetDepthWriteEnable cb depthWrite
      Vk.DYNAMIC_STATE_DEPTH_COMPARE_OP -> Vk.cmdSetDepthCompareOp cb depthCompareOp
      Vk.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE -> Vk.cmdSetDepthBoundsTestEnable cb depthBoundsTest
      Vk.DYNAMIC_STATE_DEPTH_BOUNDS -> Vk.cmdSetDepthBounds cb minBound' maxBound'
      Vk.DYNAMIC_STATE_STENCIL_TEST_ENABLE -> Vk.cmdSetStencilTestEnable cb stencilTest
      Vk.DYNAMIC_STATE_STENCIL_OP ->
        Vk.cmdSetStencilOp cb bothFaces stencilFailOp stencilPassOp stencilDepthFailOp stencilCompareOp
      Vk.DYNAMIC_STATE_STENCIL_COMPARE_MASK -> Vk.cmdSetStencilCompareMask cb bothFaces stencilCompareMask
      Vk.DYNAMIC_STATE_STENCIL_WRITE_MASK -> Vk.cmdSetStencilWriteMask cb bothFaces stencilWriteMask
      Vk.DYNAMIC_STATE_STENCIL_REFERENCE -> Vk.cmdSetStencilReference cb bothFaces stencilReference
      Vk.DYNAMIC_STATE_BLEND_CONSTANTS -> Vk.cmdSetBlendConstants cb blendConstants
      _ -> pure ()
