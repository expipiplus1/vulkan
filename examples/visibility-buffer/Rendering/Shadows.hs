{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

{-| The EVSM shadow-map unit.

The encoding parameters, the per-@(light, face)@ cube view-projections and their
SSBO uploads, and the multiview rendering info shared by the static bake
("Rendering.Static") and the per-frame orb refresh ("Rendering.Passes").
-}
module Rendering.Shadows
  ( format
  , resolution
  , params
  , cubeFaces
  , viewProjBytes
  , uploadViewProjs
  , uploadOrbViewProjs
  , renderingInfo
  , recordCube
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)
import Geomancy (Vec3, vec3)
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Transform (scale3, unTransform)
import qualified Geomancy.Vulkan.Projection as Projection
import qualified Geomancy.Vulkan.View as View
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as RenderingInfo (RenderingInfo (..))
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (fullScissor)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline

import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow as Shadow
import Pipeline.Shadow.Params (Params (..))
import qualified Scene.Lights as Lights
import qualified Scene.Objects as Objects
import qualified Upload

-- | EVSM shadow moments (4 exponential-variance moments per texel).
format :: Vk.Format
format = Vk.FORMAT_R32G32B32A32_SFLOAT

-- | Shadow cube face resolution (square).
resolution :: Word32
resolution = 256

{- | The EVSM encoding, specialized into the occluder and the resolve alike.

'far' spans a stage lamp to the far side of the rock ball (@stageRadius + caveRadius@ =
68 m) with room to spare; the fp32 ceiling on the moments only bites past @1.48 * far@.
-}
params :: Params
params = Params{far = 90, warpC = 30.0}

-- | The six cube-face directions + up vectors (standard cube-map convention).
faces :: [(Vec3, Vec3)]
faces =
  [ (vec3 1 0 0, vec3 0 (-1) 0)
  , (vec3 (-1) 0 0, vec3 0 (-1) 0)
  , (vec3 0 1 0, vec3 0 0 1)
  , (vec3 0 (-1) 0, vec3 0 0 (-1))
  , (vec3 0 0 1, vec3 0 (-1) 0)
  , (vec3 0 0 (-1), vec3 0 (-1) 0)
  ]

{- | View-projections for every @(light, face)@.

Light-major, so the vertex shader indexes @light*6 + gl_ViewIndex@; reverse-Z, 90°
square per face.
-}
viewProjs :: Float -> [Mat4.Mat4]
viewProjs t = concatMap (lightViewProjs . Lights.position) (Lights.lights t)

{- | The six shadow-cube view-projections for a light at @pos@ (reverse-Z, 90° square).

'faces' are the GL cube-face view vectors (Y-up NDC), but geomancy's Vulkan
projection is Y-down, so each face renders vertically flipped versus what the hardware
cube sampler reads. @flipY@ negates clip-space Y to realign them (cull is NONE, so the
inverted winding is harmless).
-}
lightViewProjs :: Vec3 -> [Mat4.Mat4]
lightViewProjs pos = do
  (dir, up) <- faces
  pure . unTransform $
    flipY <> proj <> View.lookAtRH pos (pos + dir) up
  where
    proj = Projection.reverseDepthRH (pi / 2) 0.07 (fromIntegral resolution) (fromIntegral resolution)
    flipY = scale3 1 (-1) 1

-- | The SSBO stride: one @mat4@ view-projection.
viewProjBytes :: Int
viewProjBytes = sizeOf (undefined :: Mat4.Mat4)

-- | View-projections per light: one per shadow-cube face.
cubeFaces :: Word32
cubeFaces = 6

-- | Fill the view-projection SSBO (one @mat4@ per @(light, face)@) for time @t@.
uploadViewProjs :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
uploadViewProjs cb buffer t = Upload.slice cb buffer 0 (viewProjs t)

-- | Rewrite just the orbs' view-projections (from 'Lights.orbBase') for time @t@.
uploadOrbViewProjs :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
uploadOrbViewProjs cb buffer t =
  Upload.slice cb buffer (Lights.orbBase * cubeFaces) $
    concatMap (lightViewProjs . Lights.position . (`Lights.orbLight` t)) Lights.orbs

-- | Multiview rendering info for one light's shadow cube (all six faces, viewMask 0x3F).
renderingInfo :: Vk.ImageView -> Vk.ImageView -> Vk.RenderingInfo '[]
renderingInfo colorView depthView =
  (Dynamic.renderingInfo (fullScissor (Vk.Extent2D resolution resolution)) [(colorView, Vk.Float32 0 0 0 0)] (Just (depthView, 0.0)))
    { RenderingInfo.viewMask = 0x3F
    }

{- | Record one light's shadow cube: a multiview draw of an occluder range into
its six faces. Shared by the static bake and the per-frame orb refresh.
-}
recordCube :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Vk.DescriptorSet -> Shade.Light -> Word32 -> Vk.ImageView -> Vk.ImageView -> Vk.Buffer -> Vk.DeviceSize -> m ()
recordCube cb pipeline shadowSet light faceBase renderView depthView indirect drawOffset = liftIO do
  Vk.cmdUseRendering cb (renderingInfo renderView depthView) do
    Vk.cmdSetViewport cb 0 [Vk.Viewport 0 0 (fromIntegral resolution) (fromIntegral resolution) 0 1]
    Vk.cmdSetScissor cb 0 [Vk.Rect2D (Vk.Offset2D 0 0) (Vk.Extent2D resolution resolution)]
    Pipeline.bind cb pipeline
    Shadow.pushShadow cb pipeline light faceBase
    Pipeline.bindSet cb pipeline 0 shadowSet
    Vk.cmdDrawIndirect cb indirect drawOffset Objects.occluderDrawCount Objects.drawStride
