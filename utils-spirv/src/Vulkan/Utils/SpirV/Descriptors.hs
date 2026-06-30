{-| Build Vulkan descriptor-set-layout and push-constant values from a reflected
'Module' at runtime. The reflected SPIR-V enums share Vulkan's numeric values, so
'Vk.DescriptorType' / 'Vk.ShaderStageFlags' come straight from the reflected
integers.
-}
module Vulkan.Utils.SpirV.Descriptors
  ( descriptorSetLayoutInfos
  , singleDescriptorSetLayoutInfo
  , pushConstantRanges
  , mergedDescriptorSetLayoutInfos
  , mergedPushConstantRanges
  , moduleStageFlags
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Word (Word32)
import Vulkan.Core10 qualified as Vk
import Vulkan.Zero (zero)

import Data.SpirV.Reflect.BlockVariable qualified
import Data.SpirV.Reflect.DescriptorBinding qualified as DescriptorBinding
import Data.SpirV.Reflect.DescriptorSet qualified
import Data.SpirV.Reflect.Enums.DescriptorType qualified as R
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified

import Vulkan.Utils.PipelineLayout (DescriptorBindingConflict (..), mergeDescriptorSetLayoutBindings, mergePushConstantRanges)
import Vulkan.Utils.SpirV.Layout (OffsetMap, mergeKeyed, renderMismatch)
import Vulkan.Utils.SpirV.Reflect.OffsetMaps (pushOffsetMaps, resourceOffsetMaps)

{- | One @(set number, layout create-info)@ per reflected descriptor set, with
all bindings tagged with this module's shader stage.
-}
descriptorSetLayoutInfos :: Module -> [(Word32, Vk.DescriptorSetLayoutCreateInfo '[])]
descriptorSetLayoutInfos m =
  [ ( ds.set
    , zero
        { Vk.bindings =
            V.fromList [bindingToVk stage b | b <- V.toList ds.bindings]
        }
    )
  | ds <- V.toList m.descriptor_sets
  ]
  where
    stage = moduleStageFlags m

bindingToVk :: Vk.ShaderStageFlags -> DescriptorBinding.DescriptorBinding -> Vk.DescriptorSetLayoutBinding
bindingToVk stage b =
  zero
    { Vk.binding = b.binding
    , Vk.descriptorType = Vk.DescriptorType (fromIntegral (descriptorTypeInt b))
    , Vk.descriptorCount = fromMaybe 1 b.count
    , Vk.stageFlags = stage
    }

{- | The create-info of a module's sole descriptor set — the common single-set
shader. 'Left' if the module declares no sets or more than one.
-}
singleDescriptorSetLayoutInfo :: Module -> Either String (Vk.DescriptorSetLayoutCreateInfo '[])
singleDescriptorSetLayoutInfo m =
  case descriptorSetLayoutInfos m of
    [(_, info)] -> Right info
    [] -> Left "shader declares no descriptor sets"
    sets -> Left ("shader declares " <> show (length sets) <> " descriptor sets, expected exactly one")

{- | One range per reflected push-constant block, tagged with this module's
shader stage.
-}
pushConstantRanges :: Module -> [Vk.PushConstantRange]
pushConstantRanges m =
  [ zero
      { Vk.stageFlags = stage
      , Vk.offset = pc.absolute_offset
      , Vk.size = pc.size
      }
  | pc <- V.toList m.push_constants
  ]
  where
    stage = moduleStageFlags m

{- | Descriptor set layouts for a /pipeline/ built from several stages: bindings
are collected across all modules and each binding's @stageFlags@ is the OR of the
stages that declare it — correct for a set shared between, say, vertex and
fragment (unlike the single-stage 'descriptorSetLayoutInfos').

On top of the generic Vulkan-binding merge ('mergeDescriptorSetLayoutBindings'),
stages sharing a @(set, binding)@ must also agree on its /block layout/ (their
reflected offset maps are unified, see "Vulkan.Utils.SpirV.Layout"). Either a
descriptor-type or a layout disagreement is a 'Left' naming the offending binding.
-}
mergedDescriptorSetLayoutInfos :: [Module] -> Either String [(Word32, Vk.DescriptorSetLayoutCreateInfo '[])]
mergedDescriptorSetLayoutInfos modules = do
  verifyKeyed descKey (concatMap resourceOffsetMaps modules)
  traverse mergeSet (Map.toAscList bindingsBySet)
  where
    descKey (s, b) = "descriptor set " <> show s <> " binding " <> show b

    -- set -> the bindings every stage contributes to it (each tagged with its stage).
    bindingsBySet :: Map Word32 [Vk.DescriptorSetLayoutBinding]
    bindingsBySet =
      Map.fromListWith
        (flip (++))
        [ (b.set, [bindingToVk (moduleStageFlags m) b])
        | m <- modules
        , b <- V.toList m.descriptor_bindings
        ]

    mergeSet (setNo, bs) = case mergeDescriptorSetLayoutBindings bs of
      Right merged -> Right (setNo, zero{Vk.bindings = V.fromList merged})
      Left c ->
        Left (descKey (setNo, c.binding) <> ": descriptor type disagreement between stages")

{- | Push-constant ranges for a multi-stage pipeline: ranges with the same offset
and size are merged with their stage flags OR-ed. Stages that share a range must
agree on its block layout (the offset maps are unified); a disagreement is a 'Left'.
-}
mergedPushConstantRanges :: [Module] -> Either String [Vk.PushConstantRange]
mergedPushConstantRanges modules = do
  verifyKeyed pushKey (concatMap pushOffsetMaps modules)
  pure (mergePushConstantRanges (concatMap pushConstantRanges modules))
  where
    pushKey (o, sz) = "push constant at offset " <> show o <> " size " <> show sz

-- | A module's shader stage as a Vulkan flag (SPIR-V shares Vulkan's values).
moduleStageFlags :: Module -> Vk.ShaderStageFlags
moduleStageFlags m = Vk.ShaderStageFlagBits (fromIntegral m.shader_stage)

-- Layout agreement across stages. -----------------------------------------------

{- | Entries sharing a key must unify; the first disagreement is reported with the
key rendered by @shw@.
-}
verifyKeyed :: (Ord k) => (k -> String) -> [(k, OffsetMap)] -> Either String ()
verifyKeyed shw entries = case mergeKeyed entries of
  Right _ -> Right ()
  Left (k, mm) -> Left (shw k <> ": " <> renderMismatch mm)

-- | The reflected descriptor type as its raw integer (Vulkan-compatible).
descriptorTypeInt :: DescriptorBinding.DescriptorBinding -> Int
descriptorTypeInt b = let R.DescriptorType n = b.descriptor_type in n
