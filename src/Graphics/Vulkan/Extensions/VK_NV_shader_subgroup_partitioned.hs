{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_NV_shader_subgroup_partitioned
  ( pattern VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV
  , pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
  , pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( VkSubgroupFeatureFlagBits(..)
  )


-- | @VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV@ specifies the device will
-- accept SPIR-V shader modules that contain the
-- @GroupNonUniformPartitionedNV@ capability.
pattern VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV :: VkSubgroupFeatureFlagBits
pattern VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV = VkSubgroupFeatureFlagBits 0x00000100
-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION"
pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION :: Integral a => a
pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME"
pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_NV_shader_subgroup_partitioned"
