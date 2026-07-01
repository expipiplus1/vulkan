{-# LANGUAGE NoFieldSelectors #-}

{-| Assemble a pipeline's descriptor-set-layout bindings and push-constant ranges
from the per-stage contributions of several shaders.

A binding (or push-constant range) declared by more than one stage must become a
single entry whose 'Vk.stageFlags' is the OR of the contributing stages — that is
what a pipeline layout shared between, say, a vertex and a fragment shader needs.
'mergeDescriptorSetLayoutBindings' and 'mergePushConstantRanges' do that merge on
plain Vulkan values, independent of where the per-stage bindings came from (hand
written, or reflected — see @vulkan-utils-spirv@).
-}
module Vulkan.Utils.PipelineLayout
  ( mergeDescriptorSetLayoutBindings
  , mergePushConstantRanges
  , DescriptorBindingConflict (..)
  ) where

import Control.Monad (foldM)
import Data.Bits ((.|.))
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

{- | Two stages declared the same binding number with different descriptor types,
which cannot be reconciled into one binding.
-}
data DescriptorBindingConflict = DescriptorBindingConflict
  { binding :: Word32
  -- ^ The binding number the stages disagree on.
  , types :: (Vk.DescriptorType, Vk.DescriptorType)
  -- ^ The two differing descriptor types.
  }
  deriving (Eq, Show)

{- | Merge the descriptor-set-layout bindings contributed by several stages for a
single descriptor set. Bindings sharing a binding number are combined: their
'Vk.stageFlags' are OR-ed and their 'Vk.descriptorCount's maxed. A
'Vk.descriptorType' disagreement is a 'Left'. The result is ascending by
binding number.

Each input binding should carry the one stage that declares it (its
'Vk.stageFlags' set to that stage); the merge turns the per-stage bindings
into one multi-stage binding per binding number.
-}
mergeDescriptorSetLayoutBindings
  :: (Foldable f)
  => f Vk.DescriptorSetLayoutBinding
  -> Either DescriptorBindingConflict [Vk.DescriptorSetLayoutBinding]
mergeDescriptorSetLayoutBindings bindings =
  fmap (fmap snd . Map.toAscList) (foldM step Map.empty bindings)
  where
    step acc b = case Map.lookup b.binding acc of
      Nothing -> Right (Map.insert b.binding b acc)
      Just b0 -> (\b' -> Map.insert b.binding b' acc) <$> combine b0 b

    combine b0 b1
      | t0 /= t1 = Left (DescriptorBindingConflict b0.binding (t0, t1))
      | otherwise =
          Right
            ( b0
                { Vk.stageFlags = b0.stageFlags .|. b1.stageFlags
                , Vk.descriptorCount = max b0.descriptorCount b1.descriptorCount
                }
            )
      where
        t0 = b0.descriptorType
        t1 = b1.descriptorType

{- | Merge the push-constant ranges contributed by several stages: ranges sharing
the same @(offset, size)@ have their 'Vk.stageFlags' OR-ed. The result is
ascending by offset.
-}
mergePushConstantRanges
  :: (Foldable f) => f Vk.PushConstantRange -> [Vk.PushConstantRange]
mergePushConstantRanges ranges =
  [ zero{Vk.stageFlags = stage, Vk.offset = off, Vk.size = sz}
  | ((off, sz), stage) <- Map.toAscList byRange
  ]
  where
    byRange = foldl' add Map.empty ranges
    add m r = Map.insertWith (.|.) (r.offset, r.size) r.stageFlags m
