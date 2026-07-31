{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The two-phase cull pipelines.

Wraps the "Pipeline.Cull.Shader" compute passes: 'reset' rewinds the camera,
late and per-orb occluder cube draws, 'recordEarly' refills the camera and
occluder draws from last frame's visibility, 'recordLate' re-tests against this
frame's pyramid and fills the late draw. Three graph passes in
"Rendering.Passes"; the barriers between the fills, the dispatches and the
draws that consume them are the graph tracker's.
-}
module Pipeline.Cull
  ( Cull (..)
  , allocateCull
  , EarlyBuffers (..)
  , allocateEarlySet
  , LateBuffers (..)
  , allocateLateSet
  , EarlyParams (..)
  , LateParams (..)
  , reset
  , recordEarly
  , recordLate
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import Foreign.Storable (Storable)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Pipeline.Cull.Shader as Shader
import qualified Scene.Objects as Objects

-- Generate the @EarlyParams@ / @LateParams@ push-constant records.
reflectShaderTypesBytes Shader.earlyCode
reflectShaderTypesBytes Shader.lateCode

-- | The early (last frame's visibility) and late (this frame's pyramid) phases.
data Cull = Cull
  { early :: Pipeline
  , late :: Pipeline
  }

allocateCull :: Vk.Device -> ResourceT IO Cull
allocateCull dev = do
  early <- allocateCompute dev () Shader.earlyCode
  late <- allocateCompute dev () Shader.lateCode
  pure Cull{early, late}

-- | The buffers the early phase reads and refills.
data EarlyBuffers = EarlyBuffers
  { objects :: Vk.Buffer
  -- ^ the shared object table (read: cave-cube bounds from the transforms).
  , indirect :: Vk.Buffer
  -- ^ the draw commands ("Scene.Objects"); the cube @instanceCount@s are the counters.
  , visMain :: Vk.Buffer
  -- ^ the camera instance remap, refilled from the early tests.
  , visOcc :: Vk.Buffer
  -- ^ the occluder instance remap, its per-orb ranges refilled from the reach tests.
  , lights :: Vk.Buffer
  -- ^ the lights SSBO (read: each orb's centre and reach).
  , visBits :: Vk.Buffer
  -- ^ the per-cube visibility words (read last frame's, overwrite with the draw decision).
  }

-- | The early set: 'EarlyBuffers' at bindings 0-3 and 5-6, the depth pyramid sampled at 4.
allocateEarlySet :: Vk.Device -> Pipeline -> EarlyBuffers -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateEarlySet dev pl bufs sampler hizView = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.objects
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.indirect
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visMain
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visOcc
    , combinedImageSamplerWrite set 4 sampler hizView Vk.IMAGE_LAYOUT_GENERAL
    , bufferWrite set 5 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.lights
    , bufferWrite set 6 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visBits
    ]
    []
  pure set

-- | The buffers the late phase reads and refills ('EarlyBuffers' sans occluder set).
data LateBuffers = LateBuffers
  { objects :: Vk.Buffer
  , indirect :: Vk.Buffer
  , visMain :: Vk.Buffer
  , visBits :: Vk.Buffer
  }

-- | The late set: 'LateBuffers' at bindings 0-2 and 4, the pyramid sampled at 3.
allocateLateSet :: Vk.Device -> Pipeline -> LateBuffers -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateLateSet dev pl bufs sampler hizView = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.objects
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.indirect
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visMain
    , combinedImageSamplerWrite set 3 sampler hizView Vk.IMAGE_LAYOUT_GENERAL
    , bufferWrite set 4 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visBits
    ]
    []
  pure set

{- | Reset the cube draws.

@mainCube.instanceCount@ rewinds to the glowstone prefix (= @caveBase@), the
late cube and each orb's occluder cube to empty. The full occluder set
(@occCube@) keeps the generator's count — only the bake draws it.
-}
reset :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Word32 -> m ()
reset cb indirect caveBase = do
  Vk.cmdFillBuffer cb indirect Objects.mainCubeCountOffset 4 caveBase
  Vk.cmdFillBuffer cb indirect Objects.lateCubeCountOffset 4 0
  forM_ Objects.orbOccCountOffsets \off -> Vk.cmdFillBuffer cb indirect off 4 0

-- | Record the early dispatch, refilling the reset draws from last frame's visibility.
recordEarly :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> EarlyParams -> Vk.CommandBuffer -> m ()
recordEarly pl set params = recordPhase pl set params params.caveCount

-- | Record the late dispatch, filling the late draw against this frame's pyramid.
recordLate :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> LateParams -> Vk.CommandBuffer -> m ()
recordLate pl set params = recordPhase pl set params params.caveCount

-- One invocation per cave cube, workgroups of the shaders' local_size_x.
recordPhase :: (Storable p, MonadIO m) => Pipeline -> Vk.DescriptorSet -> p -> Word32 -> Vk.CommandBuffer -> m ()
recordPhase pl set params caveCount cb = liftIO do
  Pipeline.bind cb pl
  Pipeline.push cb pl params
  Pipeline.bindSet cb pl 0 set
  Vk.cmdDispatch cb ((caveCount + 255) `div` 256) 1 1
