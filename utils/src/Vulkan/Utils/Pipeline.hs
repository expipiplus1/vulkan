{-# LANGUAGE NoFieldSelectors #-}

{-| A pipeline bundled with everything needed to drive it.

'Layout' keeps, next to the created handles, the inputs they were created from:
the per-set 'Vk.DescriptorSetLayoutCreateInfo's and the push-constant ranges.
Descriptor-set allocation and push recording need exactly those — 'allocateSet'
sizes its pool from the kept set info and 'push' takes its stage flags and byte
count from the kept range — so neither is hand-counted at call sites, where it
drifts from the shaders. The infos can be hand written or reflected from SPIR-V
("Vulkan.Utils.SpirV.Pipeline" in @vulkan-utils-spirv@ produces these types).

Designed for qualified import:

@
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline

Pipeline.bind cb pl
Pipeline.push cb pl params
set <- Pipeline.allocateSet dev pl 0
Pipeline.bindSet cb pl 0 set
@
-}
module Vulkan.Utils.Pipeline
  ( Pipeline (..)
  , allocateSet
  , bind
  , bindSet
  , push
  , Layout (..)
  , allocateLayout
  , set
  , Set (..)
  , allocateSetLayout
  , allocateDescriptorSet
  , allocateDescriptorSets
  ) where

import Control.Monad (guard, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

-- | A pipeline with its bind point and 'Layout', ready to bind, push and feed sets.
data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , bindPoint :: Vk.PipelineBindPoint
  , layout :: Layout
  }

bind :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> m ()
bind cb pl = Vk.cmdBindPipeline cb pl.bindPoint pl.pipeline

-- | Bind one descriptor set at @setNo@, via the pipeline's bind point and layout.
bindSet :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Word32 -> Vk.DescriptorSet -> m ()
bindSet cb pl setNo s =
  Vk.cmdBindDescriptorSets cb pl.bindPoint pl.layout.pipelineLayout setNo (V.singleton s) V.empty

{- | Push @a@ as the layout's single push-constant range: its stage flags, its size.

The range's size is what the layout accepts — often less than @a@'s 'Foreign.Storable.sizeOf'
(std430 blocks trailing-pad) — so exactly that many of @a@'s leading bytes are
pushed; a value too small to cover the range is an error, not an out-of-bounds
read. Layouts with several ranges (or a range off 0) need 'Vk.cmdPushConstants'
directly.
-}
push :: (Storable a, MonadIO m) => Vk.CommandBuffer -> Pipeline -> a -> m ()
push cb pl x = case pl.layout.pushRanges of
  [r]
    | r.offset == 0
    , fromIntegral r.size <= sizeOf x ->
        liftIO $ with x $ \p ->
          Vk.cmdPushConstants cb pl.layout.pipelineLayout r.stageFlags 0 r.size (castPtr p)
    | r.offset == 0 ->
        error ("Pipeline.push: the value's " <> show (sizeOf x) <> " bytes don't cover the " <> show r.size <> "-byte range")
  rs -> error ("Pipeline.push: expected a single range at offset 0, got " <> show rs)

-- | A descriptor set layout, kept with the info it was created from.
data Set = Set
  { layout :: Vk.DescriptorSetLayout
  , info :: Vk.DescriptorSetLayoutCreateInfo '[]
  }

-- | Create the set layout, keeping its info for 'allocateDescriptorSet' pool sizing.
allocateSetLayout :: (MonadResource m) => Vk.Device -> Vk.DescriptorSetLayoutCreateInfo '[] -> m Set
allocateSetLayout dev info = do
  (_, layout) <- Vk.withDescriptorSetLayout dev info Nothing allocate
  pure Set{layout, info}

{- | One descriptor set of the layout, from its own throwaway pool.

The pool is provisioned from the kept info's bindings, so it tracks whatever the
layout holds. It owns just this set and is released with @m@'s
'Control.Monad.Trans.Resource.ResourceT'.
-}
allocateDescriptorSet :: (MonadResource m) => Vk.Device -> Set -> m Vk.DescriptorSet
allocateDescriptorSet dev s = V.head . snd <$> allocateDescriptorSets dev s 1

{- | @count@ descriptor sets of the layout, from one throwaway pool.

The key releases the pool and with it every set — for sets bound to recreated
resources (a swapchain's image views).
-}
allocateDescriptorSets :: (MonadResource m) => Vk.Device -> Set -> Int -> m (ReleaseKey, V.Vector Vk.DescriptorSet)
allocateDescriptorSets dev s count = do
  (key, pool) <- Vk.withDescriptorPool dev zero{Vk.maxSets = fromIntegral count, Vk.poolSizes = poolSizes} Nothing allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = V.replicate count s.layout}
  pure (key, sets)
  where
    -- Runtime-sized bindings reflect as count 0, and a zero pool size is invalid;
    -- variable-count allocation is out of scope here, so skip them.
    poolSizes = do
      b <- s.info.bindings
      guard (b.descriptorCount > 0)
      pure $ Vk.DescriptorPoolSize b.descriptorType (b.descriptorCount * fromIntegral count)

-- | A pipeline layout, kept with its sets (by set number) and push-constant ranges.
data Layout = Layout
  { pipelineLayout :: Vk.PipelineLayout
  , sets :: [(Word32, Set)]
  , pushRanges :: [Vk.PushConstantRange]
  }

{- | Create the pipeline layout over the sets' layouts and the ranges.

Sharing a 'Set' between layouts keeps them set-compatible on that number. Set
numbers must be contiguous from 0 (pad gaps with empty sets) — 'bindSet'
addresses sets by number, which has to agree with the layout's slot order — and
anything else 'fail's here rather than misbinding at record time.
-}
allocateLayout :: (MonadResource m, MonadFail m) => Vk.Device -> [(Word32, Set)] -> [Vk.PushConstantRange] -> m Layout
allocateLayout dev sets pushRanges = do
  unless (map fst sets == take (length sets) [0 ..]) $
    fail ("Pipeline.allocateLayout: set numbers must be contiguous from 0, got " <> show (map fst sets))
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = V.fromList [s.layout | (_, s) <- sets]
        , Vk.pushConstantRanges = V.fromList pushRanges
        }
      Nothing
      allocate
  pure Layout{pipelineLayout, sets, pushRanges}

-- | The layout's set at @setNo@; 'fail's if there is none.
set :: (MonadFail m) => Layout -> Word32 -> m Set
set l setNo = case lookup setNo l.sets of
  Just s -> pure s
  Nothing -> fail ("Pipeline.set: no set " <> show setNo <> " in layout (sets: " <> show (map fst l.sets) <> ")")

-- | 'allocateDescriptorSet' for the pipeline's set @setNo@.
allocateSet :: (MonadResource m, MonadFail m) => Vk.Device -> Pipeline -> Word32 -> m Vk.DescriptorSet
allocateSet dev pl setNo = set pl.layout setNo >>= allocateDescriptorSet dev
