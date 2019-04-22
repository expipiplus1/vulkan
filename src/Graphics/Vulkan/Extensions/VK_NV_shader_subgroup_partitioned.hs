{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_shader_subgroup_partitioned
  ( pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
  , pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
  , pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_NV_shader_subgroup_partitioned
  ( pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
  , pattern VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV
  )


-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME"
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION"
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION :: Integral a => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
