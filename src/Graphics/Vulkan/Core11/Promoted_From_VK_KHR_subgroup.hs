{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( withCStructPhysicalDeviceSubgroupProperties
  , fromCStructPhysicalDeviceSubgroupProperties
  , PhysicalDeviceSubgroupProperties(..)
  , SubgroupFeatureFlagBits
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

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkPhysicalDeviceSubgroupProperties(..)
  , VkSubgroupFeatureFlagBits(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
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
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  )



-- | VkPhysicalDeviceSubgroupProperties - Structure describing subgroup
-- support for an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlags'
data PhysicalDeviceSubgroupProperties = PhysicalDeviceSubgroupProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSubgroupProperties" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSubgroupProperties' and
-- marshal a 'PhysicalDeviceSubgroupProperties' into it. The 'VkPhysicalDeviceSubgroupProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSubgroupProperties :: PhysicalDeviceSubgroupProperties -> (VkPhysicalDeviceSubgroupProperties -> IO a) -> IO a
withCStructPhysicalDeviceSubgroupProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSubgroupProperties)) (\pPNext -> cont (VkPhysicalDeviceSubgroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES pPNext (subgroupSize (marshalled :: PhysicalDeviceSubgroupProperties)) (supportedStages (marshalled :: PhysicalDeviceSubgroupProperties)) (supportedOperations (marshalled :: PhysicalDeviceSubgroupProperties)) (boolToBool32 (quadOperationsInAllStages (marshalled :: PhysicalDeviceSubgroupProperties)))))

-- | A function to read a 'VkPhysicalDeviceSubgroupProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSubgroupProperties'.
fromCStructPhysicalDeviceSubgroupProperties :: VkPhysicalDeviceSubgroupProperties -> IO PhysicalDeviceSubgroupProperties
fromCStructPhysicalDeviceSubgroupProperties c = PhysicalDeviceSubgroupProperties <$> -- Univalued Member elided
                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSubgroupProperties)))
                                                                                 <*> pure (vkSubgroupSize (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (vkSupportedStages (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (vkSupportedOperations (c :: VkPhysicalDeviceSubgroupProperties))
                                                                                 <*> pure (bool32ToBool (vkQuadOperationsInAllStages (c :: VkPhysicalDeviceSubgroupProperties)))

instance Zero PhysicalDeviceSubgroupProperties where
  zero = PhysicalDeviceSubgroupProperties Nothing
                                          zero
                                          zero
                                          zero
                                          False


-- | VkSubgroupFeatureFlagBits - Enum describing what subgroup operations are
-- supported
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlags'
type SubgroupFeatureFlagBits = VkSubgroupFeatureFlagBits


{-# complete SUBGROUP_FEATURE_BASIC_BIT, SUBGROUP_FEATURE_VOTE_BIT, SUBGROUP_FEATURE_ARITHMETIC_BIT, SUBGROUP_FEATURE_BALLOT_BIT, SUBGROUP_FEATURE_SHUFFLE_BIT, SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT, SUBGROUP_FEATURE_CLUSTERED_BIT, SUBGROUP_FEATURE_QUAD_BIT, SUBGROUP_FEATURE_PARTITIONED_BIT_NV :: SubgroupFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_BASIC_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniform@ capability.
pattern SUBGROUP_FEATURE_BASIC_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_BASIC_BIT = VK_SUBGROUP_FEATURE_BASIC_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_VOTE_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformVote@ capability.
pattern SUBGROUP_FEATURE_VOTE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_VOTE_BIT = VK_SUBGROUP_FEATURE_VOTE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_ARITHMETIC_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformArithmetic@ capability.
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT = VK_SUBGROUP_FEATURE_ARITHMETIC_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_BALLOT_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformBallot@ capability.
pattern SUBGROUP_FEATURE_BALLOT_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_BALLOT_BIT = VK_SUBGROUP_FEATURE_BALLOT_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_SHUFFLE_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformShuffle@ capability.
pattern SUBGROUP_FEATURE_SHUFFLE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_SHUFFLE_BIT = VK_SUBGROUP_FEATURE_SHUFFLE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformShuffleRelative@ capability.
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_CLUSTERED_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformClustered@ capability.
pattern SUBGROUP_FEATURE_CLUSTERED_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_CLUSTERED_BIT = VK_SUBGROUP_FEATURE_CLUSTERED_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VK_SUBGROUP_FEATURE_QUAD_BIT'
-- specifies the device will accept SPIR-V shader modules that contain the
-- @GroupNonUniformQuad@ capability.
pattern SUBGROUP_FEATURE_QUAD_BIT :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_QUAD_BIT = VK_SUBGROUP_FEATURE_QUAD_BIT


-- No documentation found for Nested "SubgroupFeatureFlagBits" "SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV :: (a ~ SubgroupFeatureFlagBits) => a
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV = VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV

-- | VkSubgroupFeatureFlags - Bitmask of VkSubgroupFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkSubgroupFeatureFlagBits'
type SubgroupFeatureFlags = SubgroupFeatureFlagBits
