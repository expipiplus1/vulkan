{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The spark swarm: a compute sim stepping every particle, and a graphics
pipeline drawing each as a small spinning eye-space triangle.

Unlike the tunnel's vertices — a pure function of time, rewritten whole every
frame — particle state must /survive/ from one frame to the next, so it
ping-pongs between two SSBOs: the sim reads what the previous frame wrote and
writes the other, and the draw pulls from the freshly written one.

The cracktro bit: every calm section the whole swarm springs onto the goal cells
of a banner ("Particles.Banner") and bursts apart again on the next drop.
-}
module Particles
  ( Particles (..)
  , Particle (..)
  , Sim (..)
  , Billboard (..)
  , Goal (..)
  , allocateParticles
  , zeroState
  , step
  , drawSwarm
  ) where

import qualified Buffer
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bits ((.|.))
import Data.Foldable (for_, toList)
import Data.Traversable (for)
import qualified Data.Vector as V
import Foreign.Marshal.Array (pokeArray)
import Foreign.Storable (sizeOf)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Mapped
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (bufferBarrier)
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
import qualified Particles.Banner as Banner
import qualified Particles.Grid as Grid
import qualified Particles.Shader as Shader
import PingPong (PingPong, Slot)
import qualified PingPong
import Tune (Knobs (..))

-- Generate the @Particle@ / @Goal@ SSBO elements and the two push-constant records.
reflectShaderTypesBytes Shader.simCode
reflectShaderTypesBytes Shader.vertCode

-- | The swarm's two pipelines and the buffers they pass between them.
data Particles = Particles
  { sim :: Pipeline
  , billboards :: Pipeline
  , state :: PingPong Vk.Buffer
  , goals :: Vk.Buffer
  , simSets :: PingPong Vk.DescriptorSet
  , drawSets :: PingPong Vk.DescriptorSet
  }

allocateParticles
  :: VMA.Allocator
  -> Vk.Device
  -> Vk.Format
  -- ^ Offscreen colour format.
  -> Vk.Format
  -- ^ Depth format.
  -> PingPong Camera
  -> ResourceT IO Particles
allocateParticles vma dev colorFormat depthFormat cameras = do
  sim <- allocateCompute dev () Shader.simCode
  billboards <- allocateBillboards dev colorFormat depthFormat
  state <- allocateState vma
  goals <- allocateGoals vma

  simSets <- for PingPong.slots \slot -> do
    set <- Pipeline.allocateSet dev sim 0
    Vk.updateDescriptorSets
      dev
      [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (PingPong.previous slot state)
      , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (PingPong.current slot state)
      , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER goals
      ]
      []
    pure set

  drawSets <- for PingPong.slots \slot -> do
    set <- Pipeline.allocateSet dev billboards 0
    Vk.updateDescriptorSets
      dev
      [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER (PingPong.current slot cameras).buffer
      , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER (PingPong.current slot state)
      ]
      []
    pure set

  pure Particles{sim, billboards, state, goals, simSets, drawSets}

allocateBillboards :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocateBillboards dev colorFormat depthFormat = do
  vertModule <- reflectBytes Shader.vertCode
  fragModule <- reflectBytes Glsl.colourFrag
  layout <- allocateReflectedLayout dev [vertModule, fragModule]
  allocateGraphicsPipeline
    dev
    layout
    zero{Dynamic.colorFormats = [colorFormat], Dynamic.depthFormat = Just depthFormat}
    ()
    [(vertModule, Shader.vertCode), (fragModule, Glsl.colourFrag)]

-- | The two buffers the sim ping-pongs between. @TRANSFER_DST@ for 'zeroState'.
allocateState :: VMA.Allocator -> ResourceT IO (PingPong Vk.Buffer)
allocateState vma =
  PingPong.twice $
    snd
      <$> Buffer.deviceBuffer
        vma
        (fromIntegral (Grid.count * sizeOf (undefined :: Particle)))
        (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
        Nothing

-- | The banner's goal cells: written once, read by every sim dispatch.
allocateGoals :: VMA.Allocator -> ResourceT IO Vk.Buffer
allocateGoals vma = do
  (buffer, mapped) <- Mapped.allocateBuffer vma Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT Grid.count
  liftIO $ pokeArray mapped (map Goal (Banner.goals Grid.count))
  pure buffer

{- | Zero both state buffers: age 0 sends every particle through the sim's
respawn path, which is how the swarm starts.
-}
zeroState :: (MonadIO m) => Particles -> Vk.CommandBuffer -> m ()
zeroState particles cb = do
  for_ particles.state \buffer -> Vk.cmdFillBuffer cb buffer 0 Vk.WHOLE_SIZE 0
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    []
    (V.fromList (toList (fmap seeded particles.state)))
    []
  where
    seeded = bufferBarrier Vk.ACCESS_TRANSFER_WRITE_BIT (Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_SHADER_WRITE_BIT)

-- | Advance the swarm: one invocation per particle.
step :: (MonadIO m) => Particles -> Slot -> Float -> Float -> Knobs -> Vk.CommandBuffer -> m ()
step particles slot time dt knobs cb = do
  Pipeline.bind cb particles.sim
  Pipeline.bindSet cb particles.sim 0 (PingPong.current slot particles.simSets)
  Pipeline.push
    cb
    particles.sim
    Sim
      { time
      , dt
      , banner = knobs.banner
      , energy = knobs.energy
      , spin = knobs.spin
      }
  Vk.cmdDispatch cb groups 1 1
  where
    groups = fromIntegral ((Grid.count + Grid.workgroup - 1) `div` Grid.workgroup)

{- | Draw the swarm and its dim, 180°-rotated echo — three pulled vertices per
particle each.

The echo goes first so the brighter copy wins overlaps (without blending, "dim"
paints darker, not more transparent), and is skipped once the banner has faded
it out rather than stamping black triangles over the text. The sparks still
depth-test against the tube but stop writing depth, so they don't occlude one
another.
-}
drawSwarm :: (MonadIO m) => Particles -> Slot -> Knobs -> Vk.CommandBuffer -> m ()
drawSwarm particles slot knobs cb = do
  Vk.cmdSetDepthWriteEnable cb False
  when (echoBrightness > 0.02) $ drawCopy echo
  drawCopy swarm
  where
    echoBrightness = 0.3 * (1 - knobs.banner)
    echo = Billboard{mirror = 1, brightness = echoBrightness, banner = knobs.banner, beat = knobs.beat}
    swarm = Billboard{mirror = 0, brightness = 1, banner = knobs.banner, beat = knobs.beat}
    drawCopy copy = do
      Pipeline.bind cb particles.billboards
      Pipeline.bindSet cb particles.billboards 0 (PingPong.current slot particles.drawSets)
      Pipeline.push cb particles.billboards copy
      Vk.cmdDraw cb (fromIntegral (3 * Grid.count)) 1 0 0
