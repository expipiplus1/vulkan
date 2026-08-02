{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The post-processing pass that mangles the rendered scene on its way to the
swapchain ("Glitch.Shader").

The pipeline is built once; only its descriptor sets follow the swapchain,
because they bind views of targets that are resized with it.
-}
module Glitch
  ( Corruption (..)
  , allocateGlitch
  , allocateSets
  , dispatch
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT)
import Data.Foldable (toList)
import qualified Data.Vector as V
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (imageWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Glitch.Grid as Grid
import qualified Glitch.Shader as Shader
import PingPong (PingPong (..))
import Tune (Knobs (..))

-- Generate the @Corruption@ push-constant record.
reflectShaderTypesBytes Shader.code

allocateGlitch :: Vk.Device -> ResourceT IO Pipeline
allocateGlitch dev = allocateCompute dev () Shader.code

{- | One set per @(scene view, output view)@ pair, from one pool: releasing the
key frees both when the swapchain goes.
-}
allocateSets
  :: Vk.Device
  -> Pipeline
  -> PingPong (Vk.ImageView, Vk.ImageView)
  -- ^ Per frame in flight: the scene view to read, the output view to write.
  -> ResourceT IO (ReleaseKey, PingPong Vk.DescriptorSet)
allocateSets dev pl views = do
  layout <- Pipeline.set pl.layout 0
  (key, [ping, pong]) <- Pipeline.allocateDescriptorSets dev layout (length views)
  let sets = PingPong ping pong
  Vk.updateDescriptorSets dev (V.concat (toList (bindings <$> sets <*> views))) []
  pure (key, sets)
  where
    bindings set (scene, glitched) = [storageImage set 0 scene, storageImage set 1 glitched]
    storageImage set binding view = imageWrite set binding Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL view

-- | Read the scene image and write the corrupted one; both must be in @GENERAL@.
dispatch :: (MonadIO m) => Pipeline -> Vk.Extent2D -> Vk.DescriptorSet -> Float -> Knobs -> Vk.CommandBuffer -> m ()
dispatch pl (Vk.Extent2D width height) set time knobs cb = do
  Pipeline.bind cb pl
  Pipeline.bindSet cb pl 0 set
  Pipeline.push
    cb
    pl
    Corruption
      { time
      , banner = knobs.banner
      , beat = knobs.beat
      , energy = knobs.energy
      , build = knobs.build
      , impact = knobs.impact
      }
  Vk.cmdDispatch cb (groups width) (groups height) 1
  where
    groups n = (n + tile - 1) `div` tile
    tile = fromIntegral Grid.tile
