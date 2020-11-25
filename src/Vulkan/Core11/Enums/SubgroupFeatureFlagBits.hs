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
                                                                             , SUBGROUP_FEATURE_PARTITIONED_BIT_NV
                                                                             , ..
                                                                             )
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type SubgroupFeatureFlags = SubgroupFeatureFlagBits

-- No documentation found for TopLevel "VkSubgroupFeatureFlagBits"
newtype SubgroupFeatureFlagBits = SubgroupFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_BASIC_BIT"
pattern SUBGROUP_FEATURE_BASIC_BIT            = SubgroupFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_VOTE_BIT"
pattern SUBGROUP_FEATURE_VOTE_BIT             = SubgroupFeatureFlagBits 0x00000002
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_ARITHMETIC_BIT"
pattern SUBGROUP_FEATURE_ARITHMETIC_BIT       = SubgroupFeatureFlagBits 0x00000004
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_BALLOT_BIT"
pattern SUBGROUP_FEATURE_BALLOT_BIT           = SubgroupFeatureFlagBits 0x00000008
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_SHUFFLE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_BIT          = SubgroupFeatureFlagBits 0x00000010
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
pattern SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT = SubgroupFeatureFlagBits 0x00000020
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_CLUSTERED_BIT"
pattern SUBGROUP_FEATURE_CLUSTERED_BIT        = SubgroupFeatureFlagBits 0x00000040
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_QUAD_BIT"
pattern SUBGROUP_FEATURE_QUAD_BIT             = SubgroupFeatureFlagBits 0x00000080
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV   = SubgroupFeatureFlagBits 0x00000100

conNameSubgroupFeatureFlagBits :: String
conNameSubgroupFeatureFlagBits = "SubgroupFeatureFlagBits"

enumPrefixSubgroupFeatureFlagBits :: String
enumPrefixSubgroupFeatureFlagBits = "SUBGROUP_FEATURE_"

showTableSubgroupFeatureFlagBits :: [(SubgroupFeatureFlagBits, String)]
showTableSubgroupFeatureFlagBits =
  [ (SUBGROUP_FEATURE_BASIC_BIT           , "BASIC_BIT")
  , (SUBGROUP_FEATURE_VOTE_BIT            , "VOTE_BIT")
  , (SUBGROUP_FEATURE_ARITHMETIC_BIT      , "ARITHMETIC_BIT")
  , (SUBGROUP_FEATURE_BALLOT_BIT          , "BALLOT_BIT")
  , (SUBGROUP_FEATURE_SHUFFLE_BIT         , "SHUFFLE_BIT")
  , (SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT, "SHUFFLE_RELATIVE_BIT")
  , (SUBGROUP_FEATURE_CLUSTERED_BIT       , "CLUSTERED_BIT")
  , (SUBGROUP_FEATURE_QUAD_BIT            , "QUAD_BIT")
  , (SUBGROUP_FEATURE_PARTITIONED_BIT_NV  , "PARTITIONED_BIT_NV")
  ]


instance Show SubgroupFeatureFlagBits where
showsPrec = enumShowsPrec enumPrefixSubgroupFeatureFlagBits
                          showTableSubgroupFeatureFlagBits
                          conNameSubgroupFeatureFlagBits
                          (\(SubgroupFeatureFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SubgroupFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixSubgroupFeatureFlagBits
                          showTableSubgroupFeatureFlagBits
                          conNameSubgroupFeatureFlagBits
                          SubgroupFeatureFlagBits

