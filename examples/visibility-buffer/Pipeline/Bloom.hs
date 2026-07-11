{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The bloom pyramid pipelines.

Progressive downsample and additive upsample over a mip chain (see
"Pipeline.Bloom.Downsample" / "Pipeline.Bloom.Upsample"). Both share a set layout — a
combined image sampler (blur source, binding 0) and a storage image (target, binding
1) — differing only in their push constant.
-}
module Pipeline.Bloom
  ( Bloom (..)
  , allocateBloom
  , allocateSet
  , pushDownsample
  , pushUpsample
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)

import qualified Pipeline.Bloom.Downsample as Downsample
import qualified Pipeline.Bloom.Upsample as Upsample
import qualified Pipeline.Sets as Sets

data Bloom = Bloom
  { down :: Pipeline
  , up :: Pipeline
  }

allocateBloom :: Vk.Device -> ResourceT IO Bloom
allocateBloom dev = do
  down <- allocateCompute dev () Downsample.code
  up <- allocateCompute dev () Upsample.code
  pure Bloom{down, up}

{- | A descriptor set for one down/upsample step.

The blur source sampled through @sampler@ (binding 0) and the target mip as a
storage image (binding 1).
-}
allocateSet :: Vk.Device -> Pipeline -> Vk.Sampler -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl sampler srcView dstView =
  Sets.allocateSampledStorage dev pl sampler srcView [dstView]

-- | Push the Karis flag (1 on the first, full-resolution downsample).
pushDownsample :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Bool -> m ()
pushDownsample cb pl karis =
  Pipeline.push cb pl (if karis then 1 else 0 :: Word32)

-- | Push the upsample tent-filter radius (in the source mip's texture coordinates).
pushUpsample :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Float -> m ()
pushUpsample cb pl radius = Pipeline.push cb pl radius
