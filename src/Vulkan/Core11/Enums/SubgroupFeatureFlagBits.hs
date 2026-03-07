{-# language CPP #-}
-- No documentation found for Chapter "SubgroupFeatureFlagBits"
module Vulkan.Core11.Enums.SubgroupFeatureFlagBits  ( SubgroupFeatureFlags
                                                    , SubgroupFeatureFlagBits( SUBGROUP_FEATURE_BASIC_BIT
                                                                             , SUBGROUP_FEATURE_VOTE_BIT
                                                                             , SUBGROUP_FEATURE_ARITHMETIC_BIT
                                                                             , SUBGROUP_FEATURE_BALLOT_BIT
                                                                             , SUBGROUP_FEATURE_SHUFFLE_BIT
                                                                             , SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
                                                                             , SUBGROUP_FEATURE_CLUSTERED_BIT
                                                                             , SUBGROUP_FEATURE_QUAD_BIT
                                                                             , SUBGROUP_FEATURE_PARTITIONED_BIT_EXT
                                                                             , SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT
                                                                             , SUBGROUP_FEATURE_ROTATE_BIT
                                                                             , ..
                                                                             )
                                                    ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type SubgroupFeatureFlags = SubgroupFeatureFlagBits

-- | VkSubgroupFeatureFlagBits - Bitmask describing what group operations are
-- supported with subgroup scope
--
-- = Description
--
-- -   #features-subgroup-basic# 'SUBGROUP_FEATURE_BASIC_BIT' specifies the
--     device will accept SPIR-V shader modules containing the
--     @GroupNonUniform@ capability.
--
-- -   #features-subgroup-vote# 'SUBGROUP_FEATURE_VOTE_BIT' specifies the
--     device will accept SPIR-V shader modules containing the
--     @GroupNonUniformVote@ capability.
--
-- -   #features-subgroup-arithmetic# 'SUBGROUP_FEATURE_ARITHMETIC_BIT'
--     specifies the device will accept SPIR-V shader modules containing
--     the @GroupNonUniformArithmetic@ capability.
--
-- -   #features-subgroup-ballot# 'SUBGROUP_FEATURE_BALLOT_BIT' specifies
--     the device will accept SPIR-V shader modules containing the
--     @GroupNonUniformBallot@ capability.
--
-- -   #features-subgroup-shuffle# 'SUBGROUP_FEATURE_SHUFFLE_BIT' specifies
--     the device will accept SPIR-V shader modules containing the
--     @GroupNonUniformShuffle@ capability.
--
-- -   #features-subgroup-shuffle-relative#
--     'SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT' specifies the device will
--     accept SPIR-V shader modules containing the
--     @GroupNonUniformShuffleRelative@ capability.
--
-- -   #features-subgroup-clustered# 'SUBGROUP_FEATURE_CLUSTERED_BIT'
--     specifies the device will accept SPIR-V shader modules containing
--     the @GroupNonUniformClustered@ capability.
--
-- -   #features-subgroup-quad# 'SUBGROUP_FEATURE_QUAD_BIT' specifies the
--     device will accept SPIR-V shader modules containing the
--     @GroupNonUniformQuad@ capability.
--
-- -   #features-subgroup-partitioned#
--     'SUBGROUP_FEATURE_PARTITIONED_BIT_EXT' specifies the device will
--     accept SPIR-V shader modules containing the
--     @GroupNonUniformPartitionedEXT@ capability.
--
-- -   #features-subgroup-rotate# 'SUBGROUP_FEATURE_ROTATE_BIT' specifies
--     the device will accept SPIR-V shader modules containing the
--     @GroupNonUniformRotateKHR@ capability.
--
-- -   #features-subgroup-rotate-clustered#
--     'SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT' specifies the device will
--     accept SPIR-V shader modules that use the @ClusterSize@ operand to
--     @OpGroupNonUniformRotateKHR@.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'SubgroupFeatureFlags'
newtype SubgroupFeatureFlagBits = SubgroupFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_BASIC_BIT"
pattern SUBGROUP_FEATURE_BASIC_BIT = SubgroupFeatureFlagBits 0x00000001

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_VOTE_BIT"
pattern SUBGROUP_FEATURE_VOTE_BIT = SubgroupFeatureFlagBits 0x00000002

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_ARITHMETIC_BIT"
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT = SubgroupFeatureFlagBits 0x00000004

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_BALLOT_BIT"
pattern SUBGROUP_FEATURE_BALLOT_BIT = SubgroupFeatureFlagBits 0x00000008

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_SHUFFLE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_BIT = SubgroupFeatureFlagBits 0x00000010

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = SubgroupFeatureFlagBits 0x00000020

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_CLUSTERED_BIT"
pattern SUBGROUP_FEATURE_CLUSTERED_BIT = SubgroupFeatureFlagBits 0x00000040

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_QUAD_BIT"
pattern SUBGROUP_FEATURE_QUAD_BIT = SubgroupFeatureFlagBits 0x00000080

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_PARTITIONED_BIT_EXT"
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_EXT = SubgroupFeatureFlagBits 0x00000100

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT"
pattern SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT = SubgroupFeatureFlagBits 0x00000400

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_ROTATE_BIT"
pattern SUBGROUP_FEATURE_ROTATE_BIT = SubgroupFeatureFlagBits 0x00000200

conNameSubgroupFeatureFlagBits :: String
conNameSubgroupFeatureFlagBits = "SubgroupFeatureFlagBits"

enumPrefixSubgroupFeatureFlagBits :: String
enumPrefixSubgroupFeatureFlagBits = "SUBGROUP_FEATURE_"

showTableSubgroupFeatureFlagBits :: [(SubgroupFeatureFlagBits, String)]
showTableSubgroupFeatureFlagBits =
  [ (SUBGROUP_FEATURE_BASIC_BIT, "BASIC_BIT")
  , (SUBGROUP_FEATURE_VOTE_BIT, "VOTE_BIT")
  ,
    ( SUBGROUP_FEATURE_ARITHMETIC_BIT
    , "ARITHMETIC_BIT"
    )
  , (SUBGROUP_FEATURE_BALLOT_BIT, "BALLOT_BIT")
  ,
    ( SUBGROUP_FEATURE_SHUFFLE_BIT
    , "SHUFFLE_BIT"
    )
  ,
    ( SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT
    , "SHUFFLE_RELATIVE_BIT"
    )
  ,
    ( SUBGROUP_FEATURE_CLUSTERED_BIT
    , "CLUSTERED_BIT"
    )
  , (SUBGROUP_FEATURE_QUAD_BIT, "QUAD_BIT")
  ,
    ( SUBGROUP_FEATURE_PARTITIONED_BIT_EXT
    , "PARTITIONED_BIT_EXT"
    )
  ,
    ( SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT
    , "ROTATE_CLUSTERED_BIT"
    )
  ,
    ( SUBGROUP_FEATURE_ROTATE_BIT
    , "ROTATE_BIT"
    )
  ]

instance Show SubgroupFeatureFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSubgroupFeatureFlagBits
      showTableSubgroupFeatureFlagBits
      conNameSubgroupFeatureFlagBits
      (\(SubgroupFeatureFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SubgroupFeatureFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSubgroupFeatureFlagBits
      showTableSubgroupFeatureFlagBits
      conNameSubgroupFeatureFlagBits
      SubgroupFeatureFlagBits
