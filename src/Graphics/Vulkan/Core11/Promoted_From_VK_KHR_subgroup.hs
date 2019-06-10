{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceSubgroupProperties(..)
  , 
#endif
  SubgroupFeatureFlagBits
  , pattern SUBGROUP_FEATURE_BASIC_BIT
  , pattern SUBGROUP_FEATURE_VOTE_BIT
  , pattern SUBGROUP_FEATURE_ARITHMETIC_BIT
  , pattern SUBGROUP_FEATURE_BALLOT_BIT
  , pattern SUBGROUP_FEATURE_SHUFFLE_BIT
  , pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
  , pattern SUBGROUP_FEATURE_CLUSTERED_BIT
  , pattern SUBGROUP_FEATURE_QUAD_BIT
  , pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV
  , SubgroupFeatureFlags
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkSubgroupFeatureFlagBits(..)
  , pattern VK_SUBGROUP_FEATURE_ARITHMETIC_BIT
  , pattern VK_SUBGROUP_FEATURE_BALLOT_BIT
  , pattern VK_SUBGROUP_FEATURE_BASIC_BIT
  , pattern VK_SUBGROUP_FEATURE_CLUSTERED_BIT
  , pattern VK_SUBGROUP_FEATURE_QUAD_BIT
  , pattern VK_SUBGROUP_FEATURE_SHUFFLE_BIT
  , pattern VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
  , pattern VK_SUBGROUP_FEATURE_VOTE_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shader_subgroup_partitioned
  ( pattern VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupProperties"
data PhysicalDeviceSubgroupProperties = PhysicalDeviceSubgroupProperties
  { -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "subgroupSize"
  subgroupSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "supportedStages"
  supportedStages :: ShaderStageFlags
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "supportedOperations"
  supportedOperations :: SubgroupFeatureFlags
  , -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "quadOperationsInAllStages"
  quadOperationsInAllStages :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSubgroupProperties where
  zero = PhysicalDeviceSubgroupProperties Nothing
                                          zero
                                          zero
                                          zero
                                          False

#endif

-- No documentation found for TopLevel "SubgroupFeatureFlagBits"
type SubgroupFeatureFlagBits = VkSubgroupFeatureFlagBits


{-# complete SUBGROUP_FEATURE_BASIC_BIT, SUBGROUP_FEATURE_VOTE_BIT, SUBGROUP_FEATURE_ARITHMETIC_BIT, SUBGROUP_FEATURE_BALLOT_BIT, SUBGROUP_FEATURE_SHUFFLE_BIT, SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT, SUBGROUP_FEATURE_CLUSTERED_BIT, SUBGROUP_FEATURE_QUAD_BIT, SUBGROUP_FEATURE_PARTITIONED_BIT_NV :: SubgroupFeatureFlagBits #-}


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_BASIC_BIT"
pattern SUBGROUP_FEATURE_BASIC_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_BASIC_BIT = VK_SUBGROUP_FEATURE_BASIC_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_VOTE_BIT"
pattern SUBGROUP_FEATURE_VOTE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_VOTE_BIT = VK_SUBGROUP_FEATURE_VOTE_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_ARITHMETIC_BIT"
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT = VK_SUBGROUP_FEATURE_ARITHMETIC_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_BALLOT_BIT"
pattern SUBGROUP_FEATURE_BALLOT_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_BALLOT_BIT = VK_SUBGROUP_FEATURE_BALLOT_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_SHUFFLE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_SHUFFLE_BIT = VK_SUBGROUP_FEATURE_SHUFFLE_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_CLUSTERED_BIT"
pattern SUBGROUP_FEATURE_CLUSTERED_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_CLUSTERED_BIT = VK_SUBGROUP_FEATURE_CLUSTERED_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_QUAD_BIT"
pattern SUBGROUP_FEATURE_QUAD_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_QUAD_BIT = VK_SUBGROUP_FEATURE_QUAD_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV = VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV

-- No documentation found for TopLevel "SubgroupFeatureFlags"
type SubgroupFeatureFlags = SubgroupFeatureFlagBits
