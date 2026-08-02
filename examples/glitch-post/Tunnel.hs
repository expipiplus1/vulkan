{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The neon wormhole: checkerboarded ring tiles flying down a twisting tube,
threaded through a tumbling octahedron. The rings pass /through/ it — the depth
attachment sorts them, not draw order.

The geometry lives entirely on the GPU. A compute pass rebuilds the whole vertex
list from the elapsed time every frame; the graphics pipeline has no vertex
input at all, its vertex shader pulls from that SSBO. Both interfaces are
reflected out of "Tunnel.Shader".
-}
module Tunnel
  ( Tunnel (..)
  , Growth (..)
  , Vertex (..)
  , allocateTunnel
  , grow
  , draw
  , twistRate
  ) where

import qualified Buffer
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Traversable (for)
import Foreign.Storable (sizeOf)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute, allocateGraphicsPipeline, allocateReflectedLayout)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Camera (Camera)
import qualified Camera
import qualified Glsl
import PingPong (PingPong, Slot)
import qualified PingPong
import Tune (Knobs (..))
import qualified Tunnel.Grid as Grid
import qualified Tunnel.Shader as Shader

-- Generate the @Vertex@ SSBO element and the @Growth@ push-constant record.
reflectShaderTypesBytes Shader.geomCode

-- | The tube's two pipelines, and the vertex SSBO between them.
data Tunnel = Tunnel
  { geometry :: Pipeline
  , scene :: Pipeline
  , vertices :: Vk.Buffer
  , geometrySet :: Vk.DescriptorSet
  , sceneSets :: PingPong Vk.DescriptorSet
  }

allocateTunnel
  :: VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -- ^ Offscreen colour format.
  -> Vk.Format
  -- ^ Depth format.
  -> PingPong Camera
  -> ResourceT IO Tunnel
allocateTunnel vma dev colorFormat depthFormat cameras = do
  geometry <- allocateCompute dev () Shader.geomCode
  scene <- allocateScene dev colorFormat depthFormat
  vertices <- allocateVertices vma

  geometrySet <- Pipeline.allocateSet dev geometry 0
  Vk.updateDescriptorSets dev [bufferWrite geometrySet 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER vertices] []

  sceneSets <- for cameras \camera -> do
    set <- Pipeline.allocateSet dev scene 0
    Vk.updateDescriptorSets
      dev
      [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER camera.buffer
      , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER vertices
      ]
      []
    pure set

  pure Tunnel{geometry, scene, vertices, geometrySet, sceneSets}

allocateScene :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocateScene dev colorFormat depthFormat = do
  vertModule <- reflectBytes Shader.vertCode
  fragModule <- reflectBytes Glsl.colourFrag
  layout <- allocateReflectedLayout dev [vertModule, fragModule]
  allocateGraphicsPipeline
    dev
    layout
    zero{Dynamic.colorFormats = [colorFormat], Dynamic.depthFormat = Just depthFormat}
    ()
    [(vertModule, Shader.vertCode), (fragModule, Glsl.colourFrag)]

{- | The SSBO 'grow' writes and 'draw' pulls. One buffer serves every frame: the
hazard against the previous frame's pulls is "Main"'s barrier to issue.
-}
allocateVertices :: VMA.Allocator -> ResourceT IO Vk.Buffer
allocateVertices vma = do
  (_, buffer) <-
    Buffer.deviceBuffer
      vma
      (fromIntegral (Grid.vertexCount * sizeOf (undefined :: Vertex)))
      Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
      Nothing
  pure buffer

-- | Rebuild the tube: one invocation per tile or face.
grow :: (MonadIO m) => Tunnel -> Float -> Float -> Knobs -> Vk.CommandBuffer -> m ()
grow tunnel time twist knobs cb = do
  Pipeline.bind cb tunnel.geometry
  Pipeline.bindSet cb tunnel.geometry 0 tunnel.geometrySet
  Pipeline.push
    cb
    tunnel.geometry
    Growth
      { time
      , twist
      , banner = knobs.banner
      , beat = knobs.beat
      , energy = knobs.energy
      }
  Vk.cmdDispatch cb groups 1 1
  where
    groups = fromIntegral ((Grid.tiles + Grid.faces + Grid.workgroup - 1) `div` Grid.workgroup)

-- | Draw what 'grow' just wrote, through the slot's camera.
draw :: (MonadIO m) => Tunnel -> Slot -> Vk.CommandBuffer -> m ()
draw tunnel slot cb = do
  Pipeline.bind cb tunnel.scene
  Pipeline.bindSet cb tunnel.scene 0 (PingPong.current slot tunnel.sceneSets)
  Vk.cmdDraw cb (fromIntegral Grid.vertexCount) 1 0 0

{- | Radians per second of whole-tube rotation at full spin.

The caller integrates @'twistRate' * 'Tune.spin'@ into the angle 'grow' takes —
an angle, not a rate, so reversing the spin can't snap the rotation.
-}
twistRate :: Float
twistRate = 0.4
