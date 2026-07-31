{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Descriptor-set allocation shared by the pipeline modules.
module Pipeline.Sets
  ( allocateSampledStorage
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (combinedImageSamplerWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)

{- | A set sampling one view (binding 0) into storage-image targets (binding 1).

The shape both mip pyramids reduce through ("Pipeline.Bloom", "Pipeline.HiZ");
the vector must fill binding 1's reflected arity exactly (checked). All views
stay in @GENERAL@ (sampling is legal there, so no layout churn per mip).
-}
allocateSampledStorage :: Vk.Device -> Pipeline -> Vk.Sampler -> Vk.ImageView -> V.Vector Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSampledStorage dev pl sampler srcView dstViews = do
  set0 <- Pipeline.set pl.layout 0
  let arity = maybe 0 (.descriptorCount) (V.find (\b -> b.binding == 1) set0.info.bindings)
  when (fromIntegral (V.length dstViews) /= arity) $
    error ("allocateSampledStorage: " <> show (V.length dstViews) <> " target views for reflected binding-1 arity " <> show arity)
  set <- Pipeline.allocateDescriptorSet dev set0
  Vk.updateDescriptorSets
    dev
    [ combinedImageSamplerWrite set 0 sampler srcView Vk.IMAGE_LAYOUT_GENERAL
    , SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 1
          , Vk.descriptorCount = fromIntegral (V.length dstViews)
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.imageInfo = V.map (\v -> zero{Vk.imageView = v, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL}) dstViews
          }
    ]
    []
  pure set
