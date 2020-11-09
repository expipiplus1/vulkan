{-# language CPP #-}
module Vulkan.Core11.Enums.SubgroupFeatureFlagBits  ( SubgroupFeatureFlagBits( SUBGROUP_FEATURE_BASIC_BIT
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
                                                    , SubgroupFeatureFlags
                                                    ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkSubgroupFeatureFlagBits - Enum describing what group operations are
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
--     'SUBGROUP_FEATURE_PARTITIONED_BIT_NV' specifies the device will
--     accept SPIR-V shader modules containing the
--     @GroupNonUniformPartitionedNV@ capability.
--
-- = See Also
--
-- 'SubgroupFeatureFlags'
newtype SubgroupFeatureFlagBits = SubgroupFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

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
-- No documentation found for Nested "VkSubgroupFeatureFlagBits" "VK_SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
pattern SUBGROUP_FEATURE_PARTITIONED_BIT_NV = SubgroupFeatureFlagBits 0x00000100

type SubgroupFeatureFlags = SubgroupFeatureFlagBits

instance Show SubgroupFeatureFlagBits where
  showsPrec p = \case
    SUBGROUP_FEATURE_BASIC_BIT -> showString "SUBGROUP_FEATURE_BASIC_BIT"
    SUBGROUP_FEATURE_VOTE_BIT -> showString "SUBGROUP_FEATURE_VOTE_BIT"
    SUBGROUP_FEATURE_ARITHMETIC_BIT -> showString "SUBGROUP_FEATURE_ARITHMETIC_BIT"
    SUBGROUP_FEATURE_BALLOT_BIT -> showString "SUBGROUP_FEATURE_BALLOT_BIT"
    SUBGROUP_FEATURE_SHUFFLE_BIT -> showString "SUBGROUP_FEATURE_SHUFFLE_BIT"
    SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT -> showString "SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT"
    SUBGROUP_FEATURE_CLUSTERED_BIT -> showString "SUBGROUP_FEATURE_CLUSTERED_BIT"
    SUBGROUP_FEATURE_QUAD_BIT -> showString "SUBGROUP_FEATURE_QUAD_BIT"
    SUBGROUP_FEATURE_PARTITIONED_BIT_NV -> showString "SUBGROUP_FEATURE_PARTITIONED_BIT_NV"
    SubgroupFeatureFlagBits x -> showParen (p >= 11) (showString "SubgroupFeatureFlagBits 0x" . showHex x)

instance Read SubgroupFeatureFlagBits where
  readPrec = parens (choose [("SUBGROUP_FEATURE_BASIC_BIT", pure SUBGROUP_FEATURE_BASIC_BIT)
                            , ("SUBGROUP_FEATURE_VOTE_BIT", pure SUBGROUP_FEATURE_VOTE_BIT)
                            , ("SUBGROUP_FEATURE_ARITHMETIC_BIT", pure SUBGROUP_FEATURE_ARITHMETIC_BIT)
                            , ("SUBGROUP_FEATURE_BALLOT_BIT", pure SUBGROUP_FEATURE_BALLOT_BIT)
                            , ("SUBGROUP_FEATURE_SHUFFLE_BIT", pure SUBGROUP_FEATURE_SHUFFLE_BIT)
                            , ("SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT", pure SUBGROUP_FEATURE_SHUFFLE_RELATIVE_BIT)
                            , ("SUBGROUP_FEATURE_CLUSTERED_BIT", pure SUBGROUP_FEATURE_CLUSTERED_BIT)
                            , ("SUBGROUP_FEATURE_QUAD_BIT", pure SUBGROUP_FEATURE_QUAD_BIT)
                            , ("SUBGROUP_FEATURE_PARTITIONED_BIT_NV", pure SUBGROUP_FEATURE_PARTITIONED_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "SubgroupFeatureFlagBits")
                       v <- step readPrec
                       pure (SubgroupFeatureFlagBits v)))

